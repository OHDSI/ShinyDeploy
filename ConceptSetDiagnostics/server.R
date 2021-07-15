shiny::shinyServer(function(input, output, session) {
  numberOfKeywords <- reactiveVal(value = 1)
  col_names <-
    shiny::reactive(x = paste0("col", seq_len(numberOfKeywords())))
  
  observeEvent(eventExpr = input$addKeyword,
               handlerExpr = {
                 numberOfKeywords(numberOfKeywords() + 1)
               })
  
  observeEvent(eventExpr = input$removeKeyword,
               handlerExpr = {
                 if (numberOfKeywords() > 1) {
                   numberOfKeywords(numberOfKeywords() - 1)
                 }
               })
  
  output$col <- renderUI({
    purrr::map(
      .x = col_names(),
      .f = ~ shiny::textInput(
        inputId = .x,
        label = NULL,
        value = isolate(expr = input[[.x]])
      )
    )
  })
  
  conceptSetSearchResults <- reactiveVal(value = NULL)
  conceptSetSearchResultsPassingtoConceptSetExpression <-
    reactiveVal(value = NULL)
  conceptSetResultsExpression <- reactiveVal(value = NULL)
  observeEvent(eventExpr = input$search,
               handlerExpr = {
                 shiny::withProgress(expr = {
                   shinyjs::runjs(paste0('$("#col input").css("background-color","white")'))
                   keywords <- purrr::map_chr(.x = col_names(),
                                              .f = ~ input[[.x]] %||% "")
                   if (length(keywords) > 0) {
                     conceptSetExpressionAllTerms <- list()
                     searchResultConceptIdsAllTerms <- list()
                     for (i in 1:length(keywords)) {
                       vocabularyIdOfInterest <- input$vocabularyId
                       domainIdOfInterest <- input$domainId
                       
                       # step perform string search
                       searchResultConceptIds <-
                         ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                                        searchString =  keywords[[i]])
                       if (length(vocabularyIdOfInterest) > 0) {
                         searchResultConceptIds <- searchResultConceptIds %>%
                           dplyr::filter(.data$vocabularyId %in% vocabularyIdOfInterest)
                       }
                       if (length(domainIdOfInterest) > 0) {
                         searchResultConceptIds <- searchResultConceptIds %>%
                           dplyr::filter(.data$domainId %in% domainIdOfInterest)
                       }
                       
                       searchResultConceptIdsAllTerms[[i]] <-
                         searchResultConceptIds
                     }
                     
                     searchResultConceptIdsAllTerms <-
                       dplyr::bind_rows(searchResultConceptIdsAllTerms) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(dplyr::desc(.data$drc))
                     conceptSetSearchResults(searchResultConceptIdsAllTerms)
                     conceptSetSearchResultsPassingtoConceptSetExpression(searchResultConceptIdsAllTerms)
                     conceptSetResultsExpression(NULL)
                   }
                 }, message = "Loading, Please Wait . .")
               })
  
  output$isSearchResultFound <- shiny::reactive({
    return(is.null(conceptSetSearchResultsPassingtoConceptSetExpression()))
  })
  
  outputOptions(output, 'isSearchResultFound', suspendWhenHidden = FALSE)
  
  output$searchResultConceptIds <- DT::renderDT({
    if (is.null(conceptSetSearchResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetSearchResults(), selectionMode = "single") %>%
        DT::formatStyle('conceptName',
                        'standardConcept',
                        color = DT::styleEqual(c('S', 'N', 'C'), c('blue', 'red', 'purple')))
    }
  })
  
  
  output$numberOfRowSelectedInSearchResult <- shiny::reactive({
    return(length(input$searchResultConceptIds_rows_selected))
  })
  
  outputOptions(output,
                'numberOfRowSelectedInSearchResult',
                suspendWhenHidden = FALSE)
  
  observeEvent(
    eventExpr = purrr::map_chr(.x = col_names(),
                               .f = ~ input[[.x]] %||% ""),
    handlerExpr = {
      shinyjs::runjs(paste0('$("#col input").css("background-color","white")'))
    }
  )
  
  observeEvent(eventExpr = input$deleteSearchResult,
               handlerExpr = {
                 if (!is.null(input$searchResultConceptIds_rows_selected)) {
                   conceptSetSearchResults(conceptSetSearchResults()[-as.integer(input$searchResultConceptIds_rows_selected), ])
                   conceptSetSearchResultsPassingtoConceptSetExpression(conceptSetSearchResultsPassingtoConceptSetExpression()[-as.integer(input$searchResultConceptIds_rows_selected), ])
                   shinyjs::runjs(
                     paste0(
                       ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
                     )
                   )
                 }
               })
  
  # observeEvent(eventExpr = input$searchResultConceptIds_rows_selected,handlerExpr = {
  #   idx <- input$searchResultConceptIds_rows_selected
  #   conceptName <- conceptSetSearchResults()[idx,]$conceptName
  #   shinyWidgets::updatePickerInput(session = session,inputId = "conceptId",choices = conceptName)
  #   ConceptSetDiagnostics::getConceptIdDetails(conceptIds = c(4028741),connection = connection)
  # })
  
  getConceptSetExpression <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      # develop a concept set expression based on string search
      conceptSetExpressionDataFrame <-
        ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
          conceptSetExpressionDataFrame = conceptSetSearchResultsPassingtoConceptSetExpression(),
          selectAllDescendants = TRUE
        ) %>%
        ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>%
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
          updateVocabularyFields = TRUE,
          recordCount = TRUE,
          connection = connection
        ) %>%
        dplyr::arrange(dplyr::desc(.data$drc))
    })
    return(conceptSetExpressionDataFrame)
  })
  
  observeEvent(eventExpr = input$deleteConceptSetExpression,
               handlerExpr = {
                 if (!is.null(input$conceptSetExpression_checkboxes_checked)) {
                   conceptSetResultsExpression(conceptSetResultsExpression()[-as.integer(input$conceptSetExpression_checkboxes_checked), ])
                   conceptSetSearchResults(NULL)
                   shinyjs::runjs(
                     paste0(
                       ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
                     )
                   )
                 }
               })
  
  output$numberOfRowSelectedInConceptSetExpression <-
    shiny::reactive({
      return(length(input$conceptSetExpression_checkboxes_checked))
    })
  
  outputOptions(output,
                'numberOfRowSelectedInConceptSetExpression',
                suspendWhenHidden = FALSE)
  
  observeEvent(
    eventExpr = list(
      input$descendants_checkboxes_checked,
      input$mapped_checkboxes_checked,
      input$excluded_checkboxes_checked
    ),
    handlerExpr = {
      if (!is.null(input$descendants_checkboxes_checked) ||
          !is.null(input$mapped_checkboxes_checked) ||
          !is.null(input$excluded_checkboxes_checked)) {
        conceptSetSearchResults(NULL)
        shinyjs::runjs(
          paste0(
            ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
          )
        )
        data <- conceptSetResultsExpression()
        if (!is.null(input$descendants_checkboxes_checked)) {
          for (i in min(as.integer(input$descendants_checkboxes_checked)):max(as.integer(input$descendants_checkboxes_checked))) {
            if (i %in% as.integer(input$descendants_checkboxes_checked)) {
              data$includeDescendants[i] <- TRUE
            } else {
              data$includeDescendants[i] <- FALSE
            }
          }
        }
        if (!is.null(input$mapped_checkboxes_checked)) {
          for (i in min(as.integer(input$mapped_checkboxes_checked)):max(as.integer(input$mapped_checkboxes_checked))) {
            if (i %in% as.integer(input$mapped_checkboxes_checked)) {
              data$includeMapped[i] <- TRUE
            } else {
              data$includeMapped[i] <- FALSE
            }
          }
        }
        if (!is.null(input$excluded_checkboxes_checked)) {
          for (i in min(as.integer(input$excluded_checkboxes_checked)):max(as.integer(input$excluded_checkboxes_checked))) {
            if (i %in% as.integer(input$excluded_checkboxes_checked)) {
              data$isExcluded[i] <- TRUE
            } else {
              data$isExcluded[i] <- FALSE
            }
          }
        }
        conceptSetResultsExpression(data)
        
      }
    }
  )
  
  observeEvent(eventExpr = input$cohortDetails,
               handlerExpr = {
                 if (input$cohortDetails == "conceptSetExpression" &&
                     !is.null(conceptSetSearchResults())) {
                   conceptSetResultsExpression(getConceptSetExpression())
                 }
               })
  
  output$conceptSetExpression <- DT::renderDT({
    data <- conceptSetResultsExpression()
    if (is.null(data)) {
      return(NULL)
    } else {
      data$checkedDescendants <- ""
      data$checkedMapped <- ""
      data$checkedExcluded <- ""
      for (i in 1:nrow(data)) {
        if (data[i, ]$includeDescendants) {
          data[i,]$checkedDescendants <- 'checked=\"checked\"'
        }
        if (data[i, ]$includeMapped) {
          data[i,]$checkedMapped <- 'checked=\"checked\"'
        }
        if (data[i, ]$isExcluded) {
          data[i,]$checkedExcluded <- 'checked=\"checked\"'
        }
      }
      data <- data %>%
        dplyr::mutate(
          # use glue to create checked field in javascript
          select = glue::glue(
            '<input type="checkbox" class="selectConceptSetExpressionRow"  name="selectConceptSetExpressionRow"  value="{1:nrow(data)}"><br>'
          ),
          selectDescendants = glue::glue(
            '<input type="checkbox" class="selectDescendants"  name="selectDescendants" {data$checkedDescendants}  value="{1:nrow(data)}"><br>'
          ),
          selectMapped = glue::glue(
            '<input type="checkbox" class="selectMapped"  name="selectMapped" {data$checkedMapped}  value="{1:nrow(data)}"><br>'
          ),
          selectExcluded = glue::glue(
            '<input type="checkbox" class="selectExcluded"  name="selectExcluded" {data$checkedExcluded}  value="{1:nrow(data)}"><br>'
          )
        ) %>%
        dplyr::relocate(select)
      standardDataTable(
        data = data %>%
          dplyr::select(
            -.data$includeDescendants,-.data$includeMapped,-.data$isExcluded,-.data$checkedDescendants,-.data$checkedMapped,-.data$checkedExcluded
          ),
        selectionMode = "none"
      )
    }
  })
  
  getResolved <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      data <- conceptSetResultsExpression()
      data$includeDescendants <- FALSE
      data$includeMapped <- FALSE
      data$isExcluded <- FALSE
      if (is.null(input$descendants_checkboxes_checked)) {
        data$includeDescendants <-
          conceptSetResultsExpression()$includeDescendants
      } else {
        data$includeDescendants[as.integer(input$descendants_checkboxes_checked)] <-
          TRUE
      }
      if (is.null(input$mapped_checkboxes_checked)) {
        data$includeMapped <- conceptSetResultsExpression()$includeMapped
      } else {
        data$includeMapped[as.integer(input$mapped_checkboxes_checked)] <-
          TRUE
      }
      if (is.null(input$excluded_checkboxes_checked)) {
        data$isExcluded <- conceptSetResultsExpression()$isExcluded
      } else {
        data$isExcluded[as.integer(input$excluded_checkboxes_checked)] <-
          TRUE
      }
      # data <- data %>%
      #   dplyr::select(
      #     -.data$selectDescendants,-.data$selectMapped,-.data$selectExcluded,-.data$checkedDescendants,-.data$checkedMapped,-.data$checkedExcluded
      #   )
      
      conceptSetExpression <-
        ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = data)
      result <-
        ConceptSetDiagnostics::resolveConceptSetExpression(conceptSetExpression = conceptSetExpression,
                                                           connection = connection)
    })
    return(result)
  })
  
  output$resolvedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getResolved()$resolvedConcepts)
    }
  })
  
  output$mappedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getResolved()$mappedConcepts)
    }
  })
  
  getRecommendation <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      conceptSetExpression <-
        ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = conceptSetResultsExpression())
      data <-
        ConceptSetDiagnostics::getRecommendationForConceptSetExpression(conceptSetExpression = conceptSetExpression,
                                                                        connection = connection)
    })
    return(data)
  })
  
  output$recommendedStandardConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedStandard)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedSource)
    }
  })
  
  output$conceptSetExpressionJSON <- shiny::renderText({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      data <-
        ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = conceptSetResultsExpression()) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }
  })
  
  selectedConceptId <- reactiveVal(NULL)
  observeEvent(eventExpr = input$searchResultConceptIds_rows_selected,
               handlerExpr = {
                 idx <- input$searchResultConceptIds_rows_selected
                 selectedConceptId(conceptSetSearchResults()$conceptId[idx])
  })
  
  observeEvent(eventExpr = input$resolvedConceptSetExpression_rows_selected,
               handlerExpr = {
                 idx <- input$resolvedConceptSetExpression_rows_selected
                 selectedConceptId(getResolved()$resolvedConcepts$conceptId[idx])
               })
  
  observeEvent(eventExpr = input$mappedConceptSetExpression_rows_selected,
               handlerExpr = {
                 idx <- input$mappedConceptSetExpression_rows_selected
                 selectedConceptId(getResolved()$mappedConcepts$conceptId[idx])
               })
  
  output$conceptSynonym <- DT::renderDT({
    if (!is.null(selectedConceptId())) {
      shiny::withProgress(message = "Loading. Please wait . . .", {
        data <-
          ConceptSetDiagnostics::getConceptSynonym(conceptIds = selectedConceptId(), connection = connection)
        data <- data %>%
          dplyr::rename("SynonymName" = "conceptSynonymName") %>%
          dplyr::select(.data$SynonymName)
        return(data)
      })
    }
  })
  
  output$conceptRelationship <- DT::renderDT({
    if (!is.null(selectedConceptId())) {
      shiny::withProgress(message = "Loading. Please wait . . .", {
        data1 <-
          ConceptSetDiagnostics::getConceptRelationship(conceptIds = selectedConceptId(), connection = connection) %>%
          dplyr::arrange(.data$relationshipId)
        
        data2 <-
          ConceptSetDiagnostics::getConceptIdDetails(conceptIds = data1$conceptId2,
                                                     connection = connection)
        
        data <- data1 %>%
          dplyr::inner_join(data2, by = c("conceptId2" = "conceptId")) %>%
          dplyr::select(
            .data$relationshipId,
            .data$conceptName,
            .data$conceptId2,
            .data$vocabularyId
          ) %>%
          dplyr::rename(
            c("Relationship" = "relationshipId"),
            c("Relates To" = "conceptName"),
            c("ConceptID" = "conceptId2"),
            c("Vocabulary" = "vocabularyId")
          )
        return(data)
      })
    }
  })
})