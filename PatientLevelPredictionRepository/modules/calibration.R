calibrationViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
  shiny::fluidRow(
     #shinydashboard::box(status = 'info', width = 3,
     #                   title = 'Plot Select',
     #                   solidHeader = TRUE,
     #                   shiny::uiOutput(outputId = ns('calTypeSelect'))
    #),
    shinydashboard::box(status = 'info', width = 12,
                        title = 'Summary',
                        solidHeader = TRUE,
                        DT::dataTableOutput(ns('calTable'))
    )
  ),
  
  shiny::fluidRow(
    shinydashboard::box(status = 'info',
                        title = shiny::actionLink(ns("calHelp"),"Calibration Plot", icon = shiny::icon("info")),
                        solidHeader = TRUE,
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('cal')))),
    shinydashboard::box(status = 'info',
                        title = shiny::actionLink(ns("demoHelp"),"Demographic Plot", icon = shiny::icon("info")),
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('demo'))))
  )
  )
}

calibrationServer <- function(id, plpResult) {
  shiny::moduleServer(
      id,
    function(input, output, session) {
      
      output$calTable <- DT::renderDataTable({
        if(is.null(plpResult()$performanceEvaluation)){
          DT::datatable(NULL)
        } else{
          data <- plpResult()$performanceEvaluation$evaluationStatistics
          data <- as.data.frame(data)
          data$Metric <- as.character(data$Metric)
          data$Value <- as.double(as.character(data$Value))
          ind <- data$Metric %in% c('CalibrationIntercept', 
                                    'CalibrationSlope',
                                    'CalibrationInLarge',
                                    'Emean',
                                    'E90',
                                    'Emax', 
                                    'correctionFactor',
                                    'adjustGradient',
                                    'adjustIntercept')
          
          result <- reshape2::dcast(data[ind,],
                                    Eval ~ Metric, value.var = 'Value')
          row.names(result) <- NULL
          DT::datatable(result,selection = 'single')
        }
      })
      
      output$cal <- shiny::renderPlot({
        type <- plpResult()$performanceEvaluation$calibrationSummary$Eval[input$calTable_rows_selected]
        
          tryCatch({plotSparseCalibration2(evaluation = plpResult()$performanceEvaluation, 
                                 type =  type)},#input$recal)
                   error = function(err){emptyPlot(title = err)})
      })
      
      output$demo <- shiny::renderPlot({
        type <- plpResult()$performanceEvaluation$calibrationSummary$Eval[input$calTable_rows_selected]
          tryCatch(plotDemographicSummary(evaluation = plpResult()$performanceEvaluation, 
                                          type = type),
                   error= function(cond){return(NULL)})
      })
      
      
      shiny::observeEvent(input$calHelp, {
        shiny::showInfoBox("Calibration Help", "html/calHelp.html")
      })
      shiny::observeEvent(input$demoHelp, {
        shiny::showInfoBox("Demographic Help", "html/demoHelp.html")
      })
      
      
    }
  )
}
