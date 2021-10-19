validationViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(style = "font-size:70%",
    DT::dataTableOutput(ns('validationTable')), 
    
  shiny::fluidRow(
    shinydashboard::box(status = 'info',
                        title = "Roc Plot",
                        solidHeader = TRUE,
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('valRoc')))),
    shinydashboard::box(status = 'info',
                        title = "Calibration Plot",
                        solidHeader = TRUE,
                        side = "right",
                        shinycssloaders::withSpinner(shiny::plotOutput(ns('valCal'))))
  )
  )
}

validationServer <- function(id, 
                             plpResult,
                             result,
                             validation,
                             inputType,
                             useDatabase,
                             summaryTable,
                             resultRow,
                             con, 
                             mySchema,
                             connectionDetails) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      if (useDatabase == F){
        validationTable <- shiny::reactive(dplyr::filter(summaryTable,
                                                         Analysis == summaryTable[resultRow(),'Analysis']))
      }
      else{
        # validationTable <- shiny::reactive(getValSummary(con, mySchema, summaryTable[filterIndex(),'Analysis'][trueRow()]))
        validationTable <- shiny::reactive(getValSummary(con, mySchema, summaryTable[resultRow(),'Analysis']))
      }
      
      #shiny::reactive({print(validationTable())})
      #output$validationTable <- DT::renderDataTable(dplyr::select(validationTable(),c(Analysis, Dev, Val, AUC)), rownames= FALSE)
      output$validationTable <- DT::renderDataTable({
        if(nrow(validationTable())>0){
          validationTable()[,c('Analysis','T','O', 'Val', 'AUC','calibrationInLarge', 'T Size', 'O Count','Val (%)')]
        } else{
          NULL
        }
      }, escape = FALSE, filter = 'top', rownames= FALSE ) #options = list(filter = 'top'))
      
      # plots for the validation section.
      # should make this store the loaded ones to save time
      valtemplist <- list()
      valResult <- shiny::reactive({
        
        valTable <- validationTable()[input$validationTable_rows_selected,]
        if(nrow(valTable)>0){
          names <- valTable[, "Val"]
          Ts <- valTable[, "T"]
          Os <- valTable[, "O"]
          for (i in 1:nrow(valTable)){
            valtemplist[[i]] <- getPlpResult(result,validation,valTable, inputType, i, val = T, 
                                             mySchema = mySchema, connectionDetails = connectionDetails)
          }
          list(results = valtemplist, databaseName = names, Ts=Ts, Os=Os)
        }else{
          list(results = list(list()), databaseName = '', Ts='', Os='')
        }
      })
      
      output$valRoc <- shiny::renderPlot({
        
        if(is.null(valResult()$results[[1]]$performanceEvaluation)){
          return(NULL)
        } else{
          plotRocs(evaluationList = valResult()$results, 
                   modelNames = paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5)))
        }
      })
      output$valCal <- shiny::renderPlot({
        
        if(is.null(valResult()$results[[1]]$performanceEvaluation)){
          return(NULL)
        } else{
          plotCals(evaluationList = valResult()$results, 
                   modelNames =  paste0(1:length(valResult()$Ts),':',substr(valResult()$Ts,1,5),'-',substr(valResult()$Os,1,5),'-', substr(valResult()$databaseName,1,5)))
        }
        
      })
      
    }
  )
}