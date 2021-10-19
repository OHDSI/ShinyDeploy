discriminationViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    # summary table
    shiny::fluidRow(
      shinydashboard::box(status = 'info', width = 12,
                          title = 'Summary',
                          solidHeader = TRUE,
                          DT::dataTableOutput(ns('summaryTable'))
      )
    ),
    
    
  shiny::fluidRow(
   shinydashboard::box( status = 'info',
                         title = actionLink(ns("rocHelp"), "ROC Plot", icon = icon("info")),
                         solidHeader = TRUE,
                         shinycssloaders::withSpinner(plotly::plotlyOutput(ns('roc')))),
    shinydashboard::box(status = 'info',
                        title = actionLink(ns("prcHelp"), "Precision recall plot", icon = icon("info")),
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(plotly::plotlyOutput(ns('pr'))))),
  
  shiny::fluidRow(
    shinydashboard::box(status = 'info',
                        title = actionLink(ns("f1Help"), "F1 Score Plot", icon = icon("info")),
                        solidHeader = TRUE,
                        shinycssloaders::withSpinner(plotly::plotlyOutput(ns('f1')))),
    shinydashboard::box(status = 'info',
                        title = actionLink(ns("boxHelp"),"Box Plot", icon = icon("info")),
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('box'))))),
  
  shiny::fluidRow(
    shinydashboard::box(status = 'info',
                        title = actionLink(ns("predDistHelp"),"Prediction Score Distribution", icon = icon("info")),
                        solidHeader = TRUE,
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('preddist')))),
    shinydashboard::box(status = 'info',
                        title = actionLink(ns("prefDistHelp"),"Preference Score Distribution", icon = icon("info")),
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('prefdist')))))
  )
}

discriminationServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
 
      output$summaryTable <- DT::renderDataTable({
        if(is.null(plpResult()$performanceEvaluation)){
          DT::datatable(NULL)
        } else{
          data <- plpResult()$performanceEvaluation$evaluationStatistics
          print(data$Metric)
          data <- as.data.frame(data)
          data$Metric <- as.character(data$Metric)
          data$Value <- as.double(as.character(data$Value))
          ind <- data$Metric %in% c('auc',
                                    'AUROC', 
                                    'AUC.auc',
                                    'auc95lb',
                                    'auc.auc95lb',
                                    'AUC.auc_lb95ci',
                                    'auc.auc95ub', 
                                    'auc95ub',
                                    'AUC.auc_ub95ci',
                                    'AUPRC'
                                    )
          
          result <- reshape2::dcast(data[ind,],
                                    Eval ~ Metric, value.var = 'Value')
          row.names(result) <- NULL
          DT::datatable(result,selection = 'single')
        }
      })
      
      
      plots <-  shiny::reactive({
        
          result <- list(roc = tryCatch({rocPlot(eval = plpResult()$performanceEvaluation)},
                                        error = function(cond){
                                          list(train = emptyPlot(title = 'No performanceEvaluation'))
                                          }),
                         pr = tryCatch({prPlot(eval = plpResult()$performanceEvaluation)},
                                       error = function(cond){
                           list(train = emptyPlot(title = 'No performanceEvaluation'))
                         }),
                         f1 = tryCatch({f1Plot(eval = plpResult()$performanceEvaluation)},
                                       error = function(cond){
                                         list(train = emptyPlot(title = 'No performanceEvaluation'))
                                       }),
                         prefpdf = tryCatch({plotPreferencePDF(plpResult()$performanceEvaluation)},
                                            error = function(cond){
                                              NULL
                                            }),
                         predpdf = tryCatch({plotPredictedPDF(plpResult()$performanceEvaluation)},
                                            error = function(cond){
                                              NULL
                                            }),
                         box = tryCatch({plotPredictionDistribution(plpResult()$performanceEvaluation)},
                                        error = function(cond){
                                          NULL
                                        })
          )
          
        print('dne plts')
        return(result)
      }
      )
      
     output$roc <- plotly::renderPlotly({
       type <- plpResult()$performanceEvaluation$thresholdSummary$Eval[input$summaryTable_rows_selected]
       tryCatch({plots()$roc[[type]]}, error = function(err){emptyPlot(title = err)})
     })
      
      output$pr <- plotly::renderPlotly({
        type <- plpResult()$performanceEvaluation$thresholdSummary$Eval[input$summaryTable_rows_selected]
        tryCatch({plots()$pr[[type]]}, error = function(err){emptyPlot(title = err)})
     })
      
      output$f1 <- plotly::renderPlotly({
        plots()$f1[['train']]
      })
      
      # preference plot
      output$prefdist <- shiny::renderPlot({
        plots()$prefpdf
      })
      
      output$preddist <- shiny::renderPlot({
        plots()$predpdf
      })
      
      output$box <- shiny::renderPlot({
        plots()$box
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
      
    
      
    }
  )
}



# pltting
rocPlot <- function(eval, type){
  
  types <- unique(eval$thresholdSummary$Eval)
  rocobject <- list()
  length(rocobject) <- length(types)
  names(rocobject) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$Eval%in%type,]

    rocobject[[type]] <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
      plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                        text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      plotly::layout(title = "ROC Plot",
                     xaxis = list(title = "1-specificity"),
                     yaxis = list (title = "Sensitivity"),
                     showlegend = FALSE)
  }
  return(rocobject)
}

prPlot <- function(eval, type){
  types <- unique(eval$thresholdSummary$Eval)
  probject <- list()
  length(probject) <- length(types)
  names(probject) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$Eval%in%type,]
    
    popAv <- data$trueCount[1]/(data$trueCount[1] + data$falseCount[1])
    probject[[type]]  <- plotly::plot_ly(x = data$sensitivity) %>%
      plotly::add_lines(y = data$positivePredictiveValue, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(popAv,popAv),mode = 'lines',
                        line = list(dash = "dash"), color = I('red'),
                        type='scatter') %>%
      plotly::layout(title = "PR Plot",
                     xaxis = list(title = "Recall"),
                     yaxis = list (title = "Precision"),
                     showlegend = FALSE)
 
  }
  return(probject)
}

f1Plot <- function(eval, type){
  types <- unique(eval$thresholdSummary$Eval)
  f1object <- list()
  length(f1object) <- length(types)
  names(f1object) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$Eval%in%type,]
    
    f1object[[type]]  <- plotly::plot_ly(x = data$predictionThreshold) %>%
      plotly::add_lines(y = data$f1Score, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::layout(title = "F1-Score Plot",
                     xaxis = list(title = "Prediction Threshold"),
                     yaxis = list (title = "F1-Score"),
                     showlegend = FALSE)
    
  }
  return(f1object)
}
