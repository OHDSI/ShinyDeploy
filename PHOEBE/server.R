library(shiny)
library(DT)
source("DataPulls.R")

shinyServer(function(input, output) {
  
  # use user's input
  data <- eventReactive(input$input_action, {
  string <-  gsub("[\r\n]", "", input$source_string)
  domain <-  gsub("[\r\n]", "", input$source_domain)
  if (string == '') {
    stop("need to input a search string")
  }  
  else {  
    connPool <- connect(connectionDetails)
    initial_concept<-loadDataFromDB(connPool,domain,string)
    return(initial_concept)
  }  
  })

  #output the result of the function into a table
  output$table<-DT::renderDataTable({
    dt<-data()#[,c("concept_id","concept_name","vocabulary_id","domain_id","standard_concept","rc","dbc","drc","ddbc")]
    colnames (dt) = c("concept id","concept name","voabulary id","domain id",
                    "standard conceptl","record count","database count", "record with descendants count",
                    "database with descendants count")
    dt
    },
  options = list(autoWidth = TRUE,rownames= FALSE)
  )
})
