covariateSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
  shiny::fluidRow(
    shinydashboard::box( status = 'info',
                         title = "Binary", solidHeader = TRUE,
                         shinycssloaders::withSpinner(plotly::plotlyOutput(ns('covariateSummaryBinary')))),
    shinydashboard::box(status = 'info',
                        title = "Measurements", solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(plotly::plotlyOutput(ns('covariateSummaryMeasure'))))),
  shiny::fluidRow(width=12,
                  shinydashboard::box(status = 'info', width = 12,
                                      title = "Covariates", solidHeader = TRUE,
                                      DT::dataTableOutput(ns('modelCovariateInfo')))),
  shiny::fluidRow(width=12,
                  shinydashboard::box(status = 'info', width = 12,
                                      title = "Model Table", solidHeader = TRUE,
                                      shiny::downloadButton("downloadData", "Download Model"),
                                      DT::dataTableOutput(ns('modelView'))))
  )

}

covariateSummaryServer <- function(id, plpResult, summaryTable, resultRow, mySchema, con,
                                   inputSingleView) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
        covariateSummary <- shiny::reactive({
          if(inputSingleView == "Model"){
          if(is.null(plpResult()$covariateSummary)){
            covariateSummary <- loadCovSumFromDb(summaryTable[resultRow(),], mySchema, con)
            return(covariateSummary)
          } else{
            return(plpResult()$covariateSummary)
          }
          }
        })
        
      # covariate table
      output$modelView <- DT::renderDataTable(editCovariates(covariateSummary())$table,
                                              colnames = editCovariates(covariateSummary())$colnames)
      
      output$modelCovariateInfo <- DT::renderDataTable(data.frame(covariates = nrow(covariateSummary()),
                                                                  nonZeroCount = sum(covariateSummary()$covariateValue!=0),
                                                                  intercept = ifelse(class(plpResult()$model$model)=='character' || !'model '%in% names(plpResult()$model),plpResult()$model$model$coefficient[1],plpResult()$model$model$coefficient[1])))
      
      # covariate model plots
      covs <- shiny::reactive({
        if(is.null(covariateSummary()))
          return(NULL)
        plotCovariateSummary(formatCovariateTable(covariateSummary()))
      })
      
      output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
      output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
      
      # Downloadable csv of model ----
      output$downloadData <- shiny::downloadHandler(
        filename = function(){'model.csv'},
        content = function(file) {
          write.csv(covariateSummary()[,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                    , file, row.names = FALSE)
        }
      )
      
    }
  )
}