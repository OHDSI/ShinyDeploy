# @file server.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

source("helpers.R")
source("plots.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  filterIndex <- shiny::reactive({getFilter(summaryTable,input)})
  
  #print(summaryTable)
  
  # need to remove over columns:
  output$summaryTable <- DT::renderDataTable(DT::datatable(summaryTable[filterIndex(),!colnames(summaryTable)%in%c('addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')],
                                                           rownames= FALSE, selection = 'single',
                                             extensions = 'Buttons', options = list(
                                               dom = 'Blfrtip' , 
                                               buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ),
                                               scrollX = TRUE
                                               #pageLength = 100, lengthMenu=c(10, 50, 100,200)
                                             ),
                                             
                                             container = htmltools::withTags(table(
                                               class = 'display',
                                               thead(
                                                 #tags$th(title=active_columns[i], colnames(data)[i])
                                                 tr(apply(data.frame(colnames=c('Val', 'T','O',
                                                                                'TAR', 
                                                                                'Surv 2', 'C-statistic 2', 'E-statistic 2', 
                                                                                'Surv 3', 'C-statistic 3', 'E-statistic 3', 
                                                                                'Surv 5', 'C-statistic 5', 'E-statistic 5', 
                                                                                'Surv 10', 'C-statistic 10', 'E-statistic 10', 
                                                                                'T Size'), 
                                                                     labels=c( 'Database used to evaluate model', 'Target population - the patients you want to predict risk for','Outcome - what you want to predict', 
                                                                     'Time-at-risk period', 
                                                                     '1 - Survial at 2 years',
                                                                     'The discrimination C-statistic (test or validation)', 'The calibration E-statistic (test or validation)',
                                                                     '1 - Survial at 3 years',
                                                                     'The discrimination C-statistic (test or validation)', 'The calibration E-statistic (test or validation)',
                                                                     '1 - Survial at 5 years',
                                                                     'The discrimination C-statistic (test or validation)', 'The calibration E-statistic (test or validation)',
                                                                     '1 - Survial at 10 years',
                                                                     'The discrimination C-statistic (test or validation)', 'The calibration E-statistic (test or validation)',
                                                                     'Target population size of test or validation set')), 1,
                                                          function(x) th(title=x[2], x[1])))
                                               )
                                             ))
                                                          
                                             )
  )
                                             
  
  plpResult <- shiny::reactive({getPlpResult(result,validation,summaryTable, inputType,trueRow())})
  
  # covariate table
  output$modelView <- DT::renderDataTable(editCovariates(plpResult()$covariateSummary)$table,  
                                          colnames = editCovariates(plpResult()$covariateSummary)$colnames)
  
  
  output$modelCovariateInfo <- DT::renderDataTable(data.frame(covariates = nrow(plpResult()$covariateSummary),
                                                              nonZeroCount = sum(plpResult()$covariateSummary$covariateValue!=0)))
  
  # Downloadable csv of model ----
  output$downloadData <- shiny::downloadHandler(
    filename = function(){'model.csv'},
    content = function(file) {
      write.csv(plpResult()$covariateSummary[,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                , file, row.names = FALSE)
    }
  )
  
  # input tables
  output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
  output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
  output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
  
  
  
  
  # prediction text
  #output$info <- shiny::renderUI(shiny::HTML(paste0(shiny::strong('Model: '), summaryTable[trueRow(),'Model'], ' with covariate setting id ',summaryTable[trueRow(),'covariateSettingId'] , '<br/>',
  #                                                  shiny::strong('Question:'), ' Within ', summaryTable[trueRow(),'T'],
  #                                        ' predict who will develop ',  summaryTable[trueRow(),'O'],
  #                                        ' during ',summaryTable[trueRow(),'TAR'], '<br/>',
  #                                        ' Developed in database: ', shiny::strong(summaryTable[trueRow(),'Dev']), ' and ',
  #                                        ' validated in database:  ', shiny::strong(summaryTable[trueRow(),'Val'])
  #                                 ))
  #)
  
  output$sideSettings  <- shiny::renderTable(t(data.frame( 
                                                        Validation = as.character(summaryTable[trueRow(),'Val'])#,
                                                        #Model = as.character(summaryTable[trueRow(),'Model'])
                                                        )), rownames = T, colnames = F)
  
  output$sideSettings2  <- shiny::renderTable(t(data.frame(T = paste0(substring(as.character(summaryTable[trueRow(),'T']),0,25),'...') , 
                                                           O = paste0(substring(as.character(summaryTable[trueRow(),'O']),0,25),'...')  )), 
                                              rownames = T, colnames = F)
  
  
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
    
    eval <- as.data.frame(plpResult()$performanceEvaluation$evaluationStatistics)
    
    if(is.null(eval)){
      return(NULL)
    } else {
      survival2 <- as.double(as.character(eval$Value[eval$Metric=='survival_2']))
      cstat2 <- as.double(as.character(eval$Value[eval$Metric=='c-Statistic_2']))
      estat2 <- as.double(as.character(eval$Value[eval$Metric=='E-statistic_2']))
      survival3 <- as.double(as.character(eval$Value[eval$Metric=='survival_3']))
      cstat3 <- as.double(as.character(eval$Value[eval$Metric=='c-Statistic_3']))
      estat3 <- as.double(as.character(eval$Value[eval$Metric=='E-statistic_3']))
      survival5 <- as.double(as.character(eval$Value[eval$Metric=='survival_5']))
      cstat5 <- as.double(as.character(eval$Value[eval$Metric=='c-Statistic_5']))
      estat5 <- as.double(as.character(eval$Value[eval$Metric=='E-statistic_5']))
      survival10 <- as.double(as.character(eval$Value[eval$Metric=='survival_10']))
      cstat10 <- as.double(as.character(eval$Value[eval$Metric=='c-Statistic_10']))
      estat10 <- as.double(as.character(eval$Value[eval$Metric=='E-statistic_10']))
    }
    
    list(survival2 = survival2,
         cstat2 = cstat2,
         estat2 = estat2,
         survival3 = survival3,
         cstat3 = cstat3,
         estat3 = estat3,
         survival5 = survival5,
         cstat5 = cstat5,
         estat5 = estat5,
         survival10 = survival10,
         cstat10 = cstat10,
         estat10 = estat10)
  })
  
  #nbPlot
  output$nbPlot <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      nbSummary <- plpResult()$performanceEvaluation$nbSummary
      nbSummary <- nbSummary[, colnames(nbSummary)!='X']
      
      nbSummary <- reshape2::melt(nbSummary, id.vars = c('analysisId','time','threshold'))
      
      nbPlot <- ggplot2::ggplot(nbSummary, ggplot2::aes(x=threshold, y=value, color = variable))+
        #ggplot2::geom_point() + 
        ggplot2::geom_line() +
        ggplot2::facet_wrap(time ~ ., scales = 'free')
      return(nbPlot)
    }
    
  })

  
  
  # preference plot
  output$prefdist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPreferencePDF(plpResult()$performanceEvaluation) #+ 
        # ggplot2::geom_vline(xintercept=plotters()$prefthreshold) -- RMS
    }
  })
  
  output$preddist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictedPDF(plpResult()$performanceEvaluation) # + 
        #ggplot2::geom_vline(xintercept=plotters()$threshold) -- RMS     
    }
  })
  
  output$box <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictionDistribution(plpResult()$performanceEvaluation)
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
      tryCatch(plotDemographicSummary(plpResult()$performanceEvaluation),
               error= function(cond){return(NULL)})
    }
  })
  
  
  
  # Do the tables and plots:
  
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
  
  output$survival2 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Outcome", paste0(round(performance()$survival2*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$cstat2 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "C-statistic", paste0(round(performance()$cstat2*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$estat2 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "E-statistic", paste0(round(performance()$estat2*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  output$survival3 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Outcome", paste0(round(performance()$survival3*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$cstat3 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "C-statistic", paste0(round(performance()$cstat3*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$estat3 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "E-statistic", paste0(round(performance()$estat3*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  output$survival5 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Outcome", paste0(round(performance()$survival5*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$cstat5 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "C-statistic", paste0(round(performance()$cstat5*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$estat5 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "E-statistic", paste0(round(performance()$estat5*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })

  output$survival10 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Outcome", paste0(round(performance()$survival10*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$cstat10 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "C-statistic", paste0(round(performance()$cstat10*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$estat10 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "E-statistic", paste0(round(performance()$estat10*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  
  # SELECTING RESULTS - for PERFORMANCE/MODEl
  ##selectedRow <- shiny::reactiveVal(value = 1)
  trueRow <- shiny::reactiveVal(value = 1)
  
  # row selection updates dropdowns
  shiny::observeEvent(input$summaryTable_rows_selected,{
    #selectedRow(input$summaryTable_rows_selected)
    trueRow(filterIndex()[input$summaryTable_rows_selected])
    shiny::updateSelectInput(session, "selectResult",
                           selected = myResultList[[trueRow()]]
                           )
  })
  
  #drop downs update row and other drop down
  sumProxy <- DT::dataTableProxy("summaryTable", session = session)

  shiny::observeEvent(input$selectResult,{
    val <- which(myResultList==input$selectResult)
    trueRow(val)
    DT::selectRows(sumProxy, which(filterIndex()==val)) # reset filter here?
  })
  

  
  # HELPER INFO
  showInfoBox <- function(title, htmlFileName) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  
  observeEvent(input$DescriptionInfo, {
    showInfoBox("Description", "html/Description.html")
  })
  observeEvent(input$SummaryInfo, {
    showInfoBox("Summary", "html/Summary.html")
  })
  observeEvent(input$PerformanceInfo, {
    showInfoBox("Performance", "html/Performance.html")
  })
  observeEvent(input$ModelInfo, {
    showInfoBox("Model", "html/Model.html")
  })
  observeEvent(input$LogInfo, {
    showInfoBox("Log", "html/Log.html")
  })
  observeEvent(input$SettingsInfo, {
    showInfoBox("Settings", "html/Settings.html")
  })
  observeEvent(input$DataInfoInfo, {
    showInfoBox("DataInfo", "html/DataInfo.html")
  })
  observeEvent(input$HelpInfo, {
    showInfoBox("HelpInfo", "html/Help.html")
  })
  
  
  observeEvent(input$rocHelp, {
    showInfoBox("ROC Help", "html/rocHelp.html")
  })
  observeEvent(input$prcHelp, {
    showInfoBox("PRC Help", "html/prcHelp.html")
  })
  observeEvent(input$f1Help, {
    showInfoBox("F1 Score Plot Help", "html/f1Help.html")
  })
  observeEvent(input$boxHelp, {
    showInfoBox("Box Plot Help", "html/boxHelp.html")
  })
  observeEvent(input$predDistHelp, {
    showInfoBox("Predicted Risk Distribution Help", "html/predDistHelp.html")
  })
  observeEvent(input$prefDistHelp, {
    showInfoBox("Preference Score Distribution Help", "html/prefDistHelp.html")
  })
  observeEvent(input$calHelp, {
    showInfoBox("Calibration Help", "html/calHelp.html")
  })
  observeEvent(input$demoHelp, {
    showInfoBox("Demographic Help", "html/demoHelp.html")
  })

  
  
})
