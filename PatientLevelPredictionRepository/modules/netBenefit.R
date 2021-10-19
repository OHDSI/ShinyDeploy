nbViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
  shiny::fluidRow(
    shinydashboard::box(status = 'info', width = 12,
                        title = 'Settings',
                        solidHeader = TRUE,
                        uiOutput(ns('nbSelect'))
    )
    
  ),
  
  shiny::fluidRow(
    shinydashboard::box(status = 'info', width = 6,
                        title = 'Net Benefit Plot',
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('nbPlot')))),
    
    shinydashboard::box(status = 'info', width = 6,
                        title = 'Summary',
                        solidHeader = TRUE,
                        shiny::tableOutput(ns('nbTable'))
    )
  )
  )
}

nbServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$nbSelect = shiny::renderUI({
        shiny::selectInput(session$ns('nbSelect'), 
                           'Type:', 
                           unique(plpResult()$performanceEvaluation$thresholdSummary$Eval))
      })
      
      output$nbTable <- shiny::renderTable({
        if(is.null(plpResult()$performanceEvaluation)){
          return(NULL)
        } else{
          result <- extractNetBenefit(performanceEvaluation = plpResult()$performanceEvaluation, 
                                      type=input$nbSelect)
          unique(result)
        }
      })
      
      output$nbPlot <- shiny::renderPlot({
        if(is.null(plpResult()$performanceEvaluation)){
          return(NULL)
        } else{
          result <- extractNetBenefit(performanceEvaluation = plpResult()$performanceEvaluation, 
                                      type=input$nbSelect)
          result <- unique(result)
          plot(result$pt, result$netBenefit)
        }
      })
      
    }
  )
}