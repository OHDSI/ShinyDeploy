settingsViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
  shiny::h3('Model Settings: ',
            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank")
  ),
  DT::dataTableOutput(ns('modelTable')),
  
  shiny::h3('Population Settings: ',
            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank")
  ),
  DT::dataTableOutput(ns('populationTable')),
  
  shiny::h3('Covariate Settings: ',
            shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank")
  ),
  DT::dataTableOutput(ns('covariateTable')),
  
  shiny::h3("Hyper-parameters"),
  DT::dataTableOutput(ns('hpTable')),
  shiny::h3("Attrition"),
  DT::dataTableOutput(ns('attritionTable'))
  )
}

setingsServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # input tables
      output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
      output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
      output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
      
      output$hpTable <- DT::renderDataTable(DT::datatable(as.data.frame(plpResult()$model$hyperParamSearch),
                                                          options = list(scrollX = TRUE)))
      output$attritionTable <- DT::renderDataTable(plpResult()$inputSetting$populationSettings$attrition)
      
      
    }
  )
}