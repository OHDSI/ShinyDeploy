library(shiny)
library(DT)
library(dplyr)
source("DataPulls.R")

renderBorderTag <-  function() {
  return(htmltools::withTags(
    div(class="cohort-heading")
  ))
}

showInputError <- function(message) {
  showModal(modalDialog(
    title = "Input Error",
    message
  ))
}

shinyServer(function(input, output) {
  
  # use user's input for initial concept
  InitialConceptData <- eventReactive(input$inputActionConcept, {
    if (input$sourceString == '') {
      showInputError("need to input a search string")
      return(NULL)
    }  else {  
      withProgress(message = "Searching...", {
        initial_concept <- loadConceptFromDB(connPool, input$sourceDomain, input$sourceString)
      })
      return(initial_concept)
    }  
  })
  
  # use user's input for initial concept
  ConceptSetStandardData <- eventReactive(input$inputActionList, {
    if (input$conceptList == '' || is.na(as.numeric(unlist(strsplit(input$conceptList, ","))))) {
      showInputError("need to input a comma-separated concept ID list")
      return(NULL)
    } else {  
      withProgress(message = "Searching...", {
        concept_list <- loadRecommenderStandardFromDB(connPool, input$conceptList)
      })
      return(concept_list)
    }  
  })
  
  ConceptSetSourceData <- eventReactive(input$inputActionList, {
    if (input$conceptList == '' || is.na(as.numeric(unlist(strsplit(input$conceptList, ","))))) {
      showInputError("need to input a comma-separated concept ID list")
      return(NULL)
    }else {  
      withProgress(message = "Searching...", {
        concept_list <- loadRecommenderSourceFromDB(connPool, input$conceptList)
        return(concept_list)
      })
    }  
  })
  
  # output the initial concept recommendation into a table
  output$InitialConceptTable <- DT::renderDataTable({
    dt <- InitialConceptData()
    if (is.null(dt)) {
      return(NULL)
    }
    colnames(dt) <- c("concept id","concept name","vocabulary id","domain id",
                      "standard concept","record count","database count", "record with descendants count",
                      "database with descendants count")
    dt
  })
  
  # output the initial concept recommendation into a table
  output$ConceptSetStandardTable <- DT::renderDataTable({
     dt <- ConceptSetStandardData()
     if (is.null(dt)) {
       return(NULL)
     }
     dt %>%
     mutate (concept_in_set = as.factor(concept_in_set),
             domain_id = as.factor(domain_id),
             vocabulary_id = as.factor(vocabulary_id),
             concept_id = as.character (concept_id)) %>% 
     select (concept_in_set, everything())   %>%
     datatable(colnames = c( "concept in set", "concept id","concept name","vocabulary id","domain id",
                            "standard concept","record count","database count", "record with descendants count",
                            "database with descendants count"), filter = 'top') %>% 
     formatStyle(1,  color = JS("value == 'Included' ? 'green' : value == 'Not included - parent' ? 'orange' : value == 'Not included - descendant' ? 'orange' : 'red'"))
    
  })
  
  # output the initial concept recommendation into a table
  output$ConceptSetSourceTable <- DT::renderDataTable({
    dt <- ConceptSetSourceData()
    if (is.null(dt)) {
      return(NULL)
    }
    dt %>%
      mutate (domain_id = as.factor(domain_id),
              vocabulary_id = as.factor(vocabulary_id),
              source_vocabulary_id = as.factor(source_vocabulary_id),
              concept_id = as.character (concept_id),
              source_concept_id = as.character (source_concept_id)) %>% 
      datatable(colnames = c("concept id","concept name","domain id","vocabulary id","standard concept","record count","database count",
                             "record with descendants count", "database with descendants count", "source concept id", "source concept name",
                             "source_vocabulary_id","source_concept_code","source record count","source database count"), 
                             filter = 'top',  
                             options = list(scrollX = TRUE))

  })

  output$dlConceptSetStandard <- downloadHandler(
    filename = function() {
      'concept_set_standard.csv'
      },
    content = function(file) {
    table<-ConceptSetStandardData()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  output$dlConceptSetSource <- downloadHandler(
    filename = function() {
      'concept_set_source.csv'
    },
    content = function(file) {
      table<-ConceptSetSourceData()
      write.csv(table, file, row.names = FALSE, na = "")
    })
  
  output$borderInitialConcept <- renderUI({
    return(renderBorderTag())
  })
  
  output$borderConceptSet <- renderUI({
    return(renderBorderTag())
  })
  
  
})
