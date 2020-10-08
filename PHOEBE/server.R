library(shiny)
library(DT)
source("DataPulls.R")

shinyServer(function(input, output) {
  
  # use user's input
  data <- eventReactive(input$inputAction, {
    if (input$sourceString == '') {
      stop("need to input a search string")
    }  
    else {  
      initial_concept <- loadDataFromDB(connPool, input$sourceDomain, input$sourceString)
      return(initial_concept)
    }  
  })
  
  # output the result of the function into a table
  output$table <- DT::renderDataTable({
    dt <- data()
    colnames(dt) <- c("concept id","concept name","vocabulary id","domain id",
                      "standard concept","record count","database count", "record with descendants count",
                      "database with descendants count")
    dt
  },
  options = list(autoWidth = TRUE, rownames = FALSE)
  )
})
