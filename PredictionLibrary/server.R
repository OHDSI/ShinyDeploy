# @file server.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(plotly)
library(shinycssloaders)
library(DatabaseConnector)
library(DBI)
library(odbc)
library(dplyr)
library(stringr)
# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver   = "SQL Server",
#                       Server   = "healthdatascience.database.windows.net",
#                       Database = "hds1",
#                       UID      = rstudioapi::askForPassword("Database user"),
#                       PWD      = rstudioapi::askForPassword("Database password"),
#                       Port     = 1433)

source("helpers.R")
source("plots.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  filterIndex <- shiny::reactive({getFilter(summaryTable,input)})

  # need to remove over columns:

  output$summaryTable <- DT::renderDataTable({DT::datatable(summaryTable %>%
                                                              mutate_if(is.character, str_trim) %>%
                                                              mutate(AUC = round(AUC, 2)) %>%
                                                              mutate(Incidence = round(outcome_count / population_size, 2)))})
  
  
  
  selectedRow <- shiny::reactive({
    if(is.null(input$summaryTable_rows_selected[1])){
      return(1)
    }else{
      return(input$summaryTable_rows_selected[1])
    }
  })
  
  plpResult <- shiny::reactive({loadPlpFromDb(summaryTable[selectedRow(),])})
  # covariate table
  output$modelView <- DT::renderDataTable(editCovariates(plpResult()$covariateSummary)$table,  
                                          colnames = editCovariates(plpResult()$covariateSummary)$colnames)
  
  
  output$modelCovariateInfo <- DT::renderDataTable(data.frame(covariates = nrow(plpResult()$covariateSummary),
                                                              nonZeroCount = sum(plpResult()$covariateSummary$covariateValue!=0)))
  
  # Download plpresult
  output$plpResult <- shiny::downloadHandler(
    filename = function(){
      "plpResult.rds"
      },
    content = function(file) {
      saveRDS(plpResult(), file)
    }
  )
  
  # Downloadable csv of model ----
  output$downloadData <- shiny::downloadHandler(
    filename = function(){'model.csv'},
    content = function(file) {
      write.csv(plpResult()$covariateSummary[plpResult()$covariateSummary$covariateValue!=0,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                , file, row.names = FALSE)
    }
  )
  
  # input tables
  output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
  output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
  output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
  
  
  
  # author info
  authorInfo <- shiny::reactive(DBI::dbGetQuery(conn = con, paste0("SELECT * FROM researchers WHERE researcher_id = ",plpResult()$researcher_id)))
  output$authorName  <- shiny::renderText(paste("Author:",trimws(plpResult()$researcherInfo$researcher_name)))
  output$authorEmail <- shiny::renderText(paste("Email:",trimws(plpResult()$researcherInfo$researcher_email)))
  # prediction text
  output$info <- shiny::renderText(paste0('Within ', summaryTable[filterIndex(),'T'][selectedRow()],
                                          ' predict who will develop ',  summaryTable[filterIndex(),'O'][selectedRow()],
                                          ' during ',summaryTable[filterIndex(),'TAR start'][selectedRow()], ' day/s',
                                          ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToStart'][selectedRow()]==0, ' cohort start ', ' cohort end '),
                                          ' and ', summaryTable[filterIndex(),'TAR end'][selectedRow()], ' day/s',
                                          ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToEnd'][selectedRow()]==0, ' cohort start ', ' cohort end '))
  )
  

  
  # validation table and selection
  shiny::reactive(print(plpResult()$model_id))
  validationTable <-  shiny::reactive(DBI::dbGetQuery(conn = con, 
                                      paste("SELECT results.result_id, results.model_id, researcher_id, result_type, database_acronym AS Dev, target_name AS T, outcome_name AS O,
   model_type AS model, TAR, AUC, population_size, outcome_count
   FROM results
    LEFT JOIN (SELECT cohort_id, cohort_name as target_name FROM cohorts) AS cohorts ON results.target_id = cohorts.cohort_id
    LEFT JOIN (SELECT cohort_id, cohort_name as outcome_name FROM cohorts) AS targets ON results.outcome_id = targets.cohort_id
    LEFT JOIN (SELECT database_id, database_acronym FROM databases) as dbs ON results.database_id = dbs.database_id 
    LEFT JOIN (SELECT model_id, model_type FROM model_settings) AS mset ON results.model_id = mset.model_id
    LEFT JOIN (SELECT result_id, AUC_auc AS AUC, population_size, outcome_count 
                      FROM evaluation_statistics) AS es ON results.result_id = es.result_id
    WHERE results.model_id =", plpResult()$model_id)))

  output$validationTable <- DT::renderDataTable({DT::datatable(validationTable())})
  # output$validationTable <- DT::renderDataTable({DT::datatable(dplyr::select(validationTable(),c(Analysis, Dev, Val, AUC, `T Size`, `O Count`, `O Incidence (%)`)), 
  #                                                              rownames= FALSE) %>% DT::formatRound(c("AUC", "O Incidence (%)"))})
  output$databaseInfo <- DT::renderDataTable(DBI::dbGetQuery(conn = con, paste0("SELECT database_name, database_acronym, database_type FROM databases WHERE database_acronym IN (",knitr::combine_words(trimws(validationTable()$Dev), before = "'", after = "'", and = ','),")" )))
  valFilterIndex <- shiny::reactive({getFilter(validationTable(), input)})
  valSelectedRow <- shiny::reactive({
    if(is.null(input$validationTable_rows_selected[1])){
      return(1)
    }else{
      # return(input$validationTable_rows_selected[1])
      return(input$validationTable_rows_selected)
    }
  })
  
  # PLOTTING FUNCTION
  plotters <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    if(is.null(eval)){return(NULL)}
    
    calPlot <- NULL 
    rocPlot <- NULL
    prPlot <- NULL
    f1Plot <- NULL
    
    if(!is.null(eval)){
      #intPlot <- plotShiny(eval, input$slider1) -- RMS
      intPlot <- plotShiny(eval)
      rocPlot <- intPlot$roc
      prPlot <- intPlot$pr
      f1Plot <- intPlot$f1score
      
      list(rocPlot= rocPlot,
           prPlot=prPlot, f1Plot=f1Plot)
    }
  })
  
  
  performance <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    if(is.null(eval)){
      return(NULL)
    } else {
      intPlot <- getORC(eval, input$slider1)
      threshold <- intPlot$threshold
      prefthreshold <- intPlot$prefthreshold
      TP <- intPlot$TP
      FP <- intPlot$FP
      TN <- intPlot$TN
      FN <- intPlot$FN
    }
    
    twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
    colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
    rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
    
    list(threshold = threshold, 
         prefthreshold = prefthreshold,
         twobytwo = twobytwo,
         Incidence = (TP+FN)/(TP+TN+FP+FN),
         Threshold = threshold,
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP),
         PPV = TP/(TP+FP),
         NPV = TN/(TN+FN) )
  })
  
  
  # preference plot
  output$prefdist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPreferencePDF(plpResult()$performanceEvaluation, 
                        type="test" ) #+ 
      # ggplot2::geom_vline(xintercept=plotters()$prefthreshold) -- RMS
    }
  })
  
  output$preddist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictedPDF(plpResult()$performanceEvaluation, 
                       type="test" ) # + 
      #ggplot2::geom_vline(xintercept=plotters()$threshold) -- RMS     
    }
  })
  
  output$box <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictionDistribution(plpResult()$performanceEvaluation, type="test" )
    }
  })
  
  output$cal <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotSparseCalibration2(plpResult()$performanceEvaluation)
    }
  })

  output$demo <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      tryCatch(plotDemographicSummary(plpResult()$performanceEvaluation, 
                                      type="test" ),
               error= function(cond){return(NULL)})
    }
  })
# plots for the validation section. todo: add the development?
  
  valResult <- shiny::reactive({
    valtemplist <- list()
    valTable <- validationTable()
    dev <- loadPlpFromDb(valTable[1,])
    xmax <- max(dev$performanceEvaluation$calibrationSummary$averagePredictedProbability)
    ymax <- max(dev$performanceEvaluation$calibrationSummary$observedIncidence)
    rows <-  sort(valSelectedRow())
    names <- valTable[rows, "Dev"]
    for (i in 1:length(rows)){
      valtemplist[[i]] <- loadPlpFromDb(valTable[i,])
    }
    list(valtemplist, names, xmax, ymax)
  })
  
  valPlots <- shiny::reactive({
    results <- valResult()
    saveRDS(results,"results.rds")
    if(is.null(results[[1]][[1]]$performanceEvaluation)){
      return(NULL)
    } else{
      xmax = results[[3]]
      ymax = results[[4]]
      saveRDS(results, "results.RDS")
      valCalPlot <- PredictionComparison::plotMultipleCal(results[[1]], names = trimws(results[[2]]), style = "smooth" ) +
        ggplot2::annotate("rect", xmin = 0, ymin = 0, xmax = xmax, ymax = ymax, alpha = 0.2, fill = 'yellow')
      valRocPlot <- PredictionComparison::plotMultipleRoc(results[[1]], names = results[[2]], grid = F)
      list(valRocPlot= valRocPlot, valCalPlot = valCalPlot)
    }
  })
  
  output$valRoc <- shiny::renderPlot({
    try(valPlots()$valRocPlot)
  })
  output$valCal <- shiny::renderPlot({
    try(valPlots()$valCalPlot)
  })
  # Do the tables and plots:
  
  output$performance <- shiny::renderTable(performance()$performance, 
                                           rownames = F, digits = 3)
  output$twobytwo <- shiny::renderTable(performance()$twobytwo, 
                                        rownames = T, digits = 0)
  
  
  output$threshold <- shiny::renderText(format(performance()$threshold,digits=5))
  
  output$roc <- plotly::renderPlotly({
    plotters()$rocPlot
  })
  
  output$pr <- plotly::renderPlotly({
    plotters()$prPlot
  })
  output$f1 <- plotly::renderPlotly({
    plotters()$f1Plot
  })

  # covariate model plots
  covs <- shiny::reactive({
    if(is.null(plpResult()$covariateSummary))
      return(NULL)
    plotCovariateSummary(formatCovariateTable(plpResult()$covariateSummary))
  })
  
  output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
  output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
  
  # LOG
  output$log <- shiny::renderText( paste(plpResult()$log, collapse="\n") )
  
  # dashboard
  
  output$performanceBoxIncidence <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Incidence", paste0(round(performance()$Incidence*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Threshold", format((performance()$Threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
      color = "yellow"
    )
  })
  
  output$performanceBoxPPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "PPV", paste0(round(performance()$PPV*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$performanceBoxSpecificity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Specificity", paste0(round(performance()$Specificity*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  output$performanceBoxSensitivity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Sensitivity", paste0(round(performance()$Sensitivity*1000)/10, "%"), icon = shiny::icon("low-vision"),
      color = "blue"
    )
  })
  
  output$performanceBoxNPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "NPV", paste0(round(performance()$NPV*1000)/10, "%"), icon = shiny::icon("minus-square"),
      color = "black"
    )
  })
  
  #upload functionality and options
  researchers <- DBI::dbGetQuery("SELECT researcher_id, researcher_name, researcher_affiliation FROM researchers;", conn = con)
  updateSelectizeInput(session, 'researcherName', choices = c("",researchers$researcher_name), server = TRUE)
  updateSelectizeInput(session, 'researcherAffiliation', choices = c("",researchers$researcher_affiliation), server = TRUE)
  # database inputs
  databases <- DBI::dbGetQuery("SELECT * FROM databases;", conn = con)
  updateSelectizeInput(session, 'databaseName', choices = c("", databases$database_name), server = TRUE, options = list(maxItems = 5))
  updateSelectizeInput(session, 'databaseAcronym', choices = c("", databases$database_acronym), server = TRUE,options = list(maxItems = 5))
  updateSelectizeInput(session, 'databaseDesc', choices = c("", databases$database_description), server = TRUE, options = list(maxItems = 5))
  updateSelectizeInput(session, 'databaseType', choices = c("", databases$database_type), server = TRUE)
  observeEvent(input$submitStudy,{
    inputData <- data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("researcherName","researcherAffiliation","researcherEmail",
                                                                         "databaseName","databaseAcronym","databaseDesc","databaseType",
                                                                         "analysisType",
                                                                         "modelName","targetName","outcomeName"))))

    inputData[1,] <- c(
      input$researcherName,input$researcherAffiliation,input$researcherEmail,
      input$databaseName,input$databaseAcronym, input$databaseDesc, input$databaseType,
      input$AnalysisType,
      input$modelName, input$targetName,input$outcomeName)
      folderPath <- file.path("./dataHoldingFolder", paste0( trimws(input$researcherName),trimws(input$modelName)))
      if(!dir.exists(folderPath)){
        dir.create(folderPath)
      }
      readr::write_csv(inputData, path = file.path(folderPath,"inputdata.csv"))
      file.copy(from = input$libraryUpload$datapath, to = file.path(folderPath, "libraryUpload.zip"))
      
      # file.copy(from = input$plpResult$datapath, to = file.path(folderPath, "plpResult.rds"))
      # file.copy(from = input$targetCohort$datapath, to = file.path(folderPath, "target.json"))
      # file.copy(from = input$outcomeCohort$datapath,to =  file.path(folderPath, "outcome.json"))
  })
  
})