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

source("utils.R")
source("plots.R")

shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  # reactive values - contains the location of the plpResult
  ##reactVars <- shiny::reactiveValues(resultLocation=NULL,
  ##                                   plpResult= NULL)
#=============
    
    shiny::observeEvent(input$covhelp, {
      test <- ?FeatureExtraction::createCovariateSettings
      file.show(getRd(test))
    })
    shiny::observeEvent(input$pophelp, {
      test <- ?PatientLevelPrediction::createStudyPopulation
      file.show(getRd(test))
    })
    
    summaryData <- shiny::reactive({
      ind <- 1:nrow(allPerformance)
      if(input$devDatabase!='All'){
        ind <- intersect(ind,which(as.character(allPerformance$devDatabase)==input$devDatabase))
      }
      if(input$valDatabase!='All'){
        ind <- intersect(ind,which(as.character(allPerformance$valDatabase)==input$valDatabase))
      }
      if(input$T!='All'){
        ind <- intersect(ind,which(allPerformance$cohortName==input$T))
      }
      if(input$O!='All'){
        ind <- intersect(ind,which(allPerformance$outcomeName==input$O))
      }
      if(input$modelSettingName!='All'){
        ind <- intersect(ind,which(as.character(allPerformance$modelSettingName)==input$modelSettingName))
      }
      if(input$riskWindowStart!='All'){
        ind <- intersect(ind,which(allPerformance$riskWindowStart==input$riskWindowStart))
      }
      if(input$riskWindowEnd!='All'){
        ind <- intersect(ind,which(allPerformance$riskWindowEnd==input$riskWindowEnd))
      }
      
      ind
    })
    
    
    
    output$summaryTable <- DT::renderDataTable(DT::datatable(formatPerformance[summaryData(),!colnames(formatPerformance)%in%c('addExposureDaysToStart','addExposureDaysToEnd')],
                                                             rownames= FALSE))
    
    
    dataofint <- shiny::reactive({
      if(is.null(input$summaryTable_rows_selected[1])){
        ind <- 1
      }else{
        ind <- input$summaryTable_rows_selected[1]
      }
      
      loc <- plpResultLocation[summaryData(),][ind,]
      logLocation <- gsub('plpResult.rds','plplog.txt', as.character(loc[1]))
      txt <- readLines(logLocation)
      
      covariates <- NULL
      population <- NULL
      modelset <- NULL
      #if(loc[2]=='loadPlpResult'){
      #  eval <- tryCatch(do.call(as.character(loc[2]), list(dirPath=as.character(loc[1]))),
      #                   error = function(err) return(NULL))
      #  type <- 'test'
      #} else {
        #eval <- tryCatch(do.call(as.character(loc[2]), list(file=as.character(loc[1]))),
        #                 error = function(err) return(NULL))
        eval <- readRDS(as.character(loc[1]))
        if(!'inputSetting'%in%names(eval)){
          eval <- eval[[1]]
        }
        type <- 'test' #'validationre'
      #}
      if(!is.null(eval)){
        covariates <- eval$inputSetting$dataExtrractionSettings$covariateSettings
        population <- eval$inputSetting$populationSettings
        covariates <- data.frame(covariateName = names(covariates), 
                                 SettingValue = unlist(lapply(covariates, 
                                                              function(x) paste0(x, 
                                                                                 collapse='-')))
        )
        population$attrition <- NULL # remove the attrition as result and not setting
        population <- data.frame(Setting = names(population), 
                                 Value = unlist(lapply(population, 
                                                       function(x) paste0(x, 
                                                                          collapse='-')))
        )
        modelset <- data.frame(Setting = c('Model',names(eval$model$modelSettings[[2]])),
                               Value = c(eval$model$modelSettings[[1]], unlist(lapply(eval$model$modelSettings[[2]], 
                                                                                      function(x) paste0(x, collapse=''))))
        )
        
        row.names(covariates) <- NULL
        row.names(population) <- NULL
        row.names(modelset) <- NULL
      }
      
      return(list(eval=eval, type=type, 
                  logtext = txt,
                  logLocation=logLocation,
                  covariates = covariates,
                  population = population,
                  modelset = modelset))
    })
    
    plotters <- shiny::reactive({
      
      eval <- dataofint()$eval$performanceEvaluation
      if(is.null(eval)){return(NULL)}
      
      calPlot <- NULL 
      rocPlot <- NULL
      prPlot <- NULL
      f1Plot <- NULL
      demoPlot <- NULL
      boxPlot <- NULL
      distPlot <- NULL
      txt <- 'Empty'
      predictionText <- c()
      
      if(!is.null(eval)){
        intPlot <- plotShiny(eval, input$slider1)
        rocPlot <- intPlot$roc
        prPlot <- intPlot$pr
        f1Plot <- intPlot$f1score
        threshold <- intPlot$threshold
        prefthreshold <- intPlot$prefthreshold
        TP <- intPlot$TP
        FP <- intPlot$FP
        TN <- intPlot$TN
        FN <- intPlot$FN
        prefdistPlot <- plotPreferencePDF(eval, type=dataofint()$type )
        prefdistPlot <- prefdistPlot + ggplot2::geom_vline(xintercept=prefthreshold)
        preddistPlot <- plotPredictedPDF(eval, type=dataofint()$type )
        preddistPlot <- preddistPlot + ggplot2::geom_vline(xintercept=threshold)
        boxPlot <-  plotPredictionDistribution(eval, type=dataofint()$type )
        
        calPlot <- plotSparseCalibration2(eval, type=dataofint()$type )
        demoPlot <- tryCatch(plotDemographicSummary(eval, type=dataofint()$type ),
                             error= function(cond){return(NULL)})
        
        predictionText <- paste0('Within ', formatPerformance[summaryData(),'T'][1],
                                 ' predict who will develop ', formatPerformance[summaryData(),'O'][1],
                                 ' during ', formatPerformance[summaryData(),'TAR start'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToStart'][1]==0, ' cohort start ', ' cohort end '),
                                 ' and ', formatPerformance[summaryData(),'TAR end'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToEnd'][1]==0, ' cohort start ', ' cohort end '))
        
      }
      
      twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
      colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
      rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
      
      performance <- data.frame(Incidence = (TP+FN)/(TP+TN+FP+FN),
                                Threshold = threshold,
                                Sensitivity = TP/(TP+FN),
                                Specificity = TN/(TN+FP),
                                PPV = TP/(TP+FP),
                                NPV = TN/(TN+FN))
      
      list(rocPlot= rocPlot, calPlot=calPlot, 
           prPlot=prPlot, f1Plot=f1Plot, 
           demoPlot=demoPlot, boxPlot=boxPlot,
           prefdistPlot=prefdistPlot,
           preddistPlot=preddistPlot, predictionText=predictionText,
           threshold = format(threshold, digits=5), 
           twobytwo=twobytwo,
           performance = performance )
    })
    
    output$performance <- shiny::renderTable(plotters()$performance, 
                                             rownames = F, digits = 3)
    output$twobytwo <- shiny::renderTable(plotters()$twobytwo, 
                                          rownames = T, digits = 0)
    
    output$modelTable <- DT::renderDataTable(dataofint()$modelset)
    output$covariateTable <- DT::renderDataTable(dataofint()$covariates)
    output$populationTable <- DT::renderDataTable(dataofint()$population)
    
    output$info <- shiny::renderText(plotters()$predictionText)
    output$log <- shiny::renderText( paste(dataofint()$logtext, collapse="\n") )
    output$threshold <- shiny::renderText(plotters()$threshold)
    
    output$roc <- plotly::renderPlotly({
      plotters()$rocPlot
    })
    output$cal <- shiny::renderPlot({
      plotters()$calPlot
    })
    output$pr <- plotly::renderPlotly({
      plotters()$prPlot
    })
    output$f1 <- plotly::renderPlotly({
      plotters()$f1Plot
    })
    output$demo <- shiny::renderPlot({
      plotters()$demoPlot
    })
    output$box <- shiny::renderPlot({
      plotters()$boxPlot
    })
    output$preddist <- shiny::renderPlot({
      plotters()$preddistPlot
    })
    output$prefdist <- shiny::renderPlot({
      plotters()$prefdistPlot
    })
    
    
    covs <- shiny::reactive({
      if(is.null(dataofint()$eval))
        return(NULL)
      plotCovariateSummary(dataofint()$eval$covariateSummary)
    })
    
    output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
    output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
  
    
    output$modelView <- DT::renderDataTable(dataofint()$eval$covariateSummary[,c('covariateName','covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )])
  
#=============  
  
})

