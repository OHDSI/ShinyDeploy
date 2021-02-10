shiny::shinyServer(function(input, output, session) {

  ############### search tab ######################################
  # Cohort search results
  cohortSearchResults <- shiny::reactive(x = {
    if (input$searchText != "") {
      # to do -
      # check if multiple words, then split each word stringr::str_split(pattern = " ")
      # iterate over each word.
      # if all iterations are TRUE then return
      data <-   cohort %>%
        dplyr::mutate(
          searchTerms = paste(
            .data$cohortName,
            .data$logicDescription,
            .data$referentConceptIdsSearchTerms,
            .data$phenotypeName,
            .data$cohortType,
            .data$json
          )
        ) %>%
        dplyr::filter(stringr::str_detect(
          string = tolower(.data$searchTerms),
          pattern = tolower(input$searchText)
        ))
    } else {
      data <- cohort
    }
    return(data)
  })
  
  output$cohortSearchTableResults <- DT::renderDT(expr = {
    data <- cohortSearchResults()
    data <- data %>%
      dplyr::select(.data$phenotypeId,
                    .data$phenotypeName,
                    .data$cohortId,
                    .data$cohortName) %>% 
      dplyr::rename("clinicalGroup" = .data$phenotypeName)
    table <- standardDataTable(data = data,
                               selectionMode = "multiple")
    return(table)
  }, server = TRUE)

  
  # selection of rows
  cohortSearchRowsSelected <- shiny::reactive(x = {
    idx <- input$cohortSearchTableResults_rows_selected
    if (length(idx) > 1) {
      # get the last two rows selected
      lastRowsSelected <- idx[c(length(idx) - 1, length(idx))]
    } else {
      lastRowsSelected <- idx
    }
    return(cohortSearchResults()[lastRowsSelected,])
  })
  
  cohortSearchRowsSelectedDetails <- shiny::reactive(x = {
    data <- cohortSearchRowsSelected()
    if (nrow(cohortSearchRowsSelected()) > 0) {
      details <- list()
      for (i in (1:nrow(data))) {
        circeExpression <-
          CirceR::cohortExpressionFromJson(expressionJson = data[i, ]$json)
        circeExpressionMarkdown <-
          CirceR::cohortPrintFriendly(circeExpression)
        circeConceptSetListmarkdown <-
          CirceR::conceptSetListPrintFriendly(circeExpression$conceptSets)
        details[[i]] <- data[i, ]
        details[[i]]$circeConceptSetListmarkdown <-
          circeConceptSetListmarkdown
        details[[i]]$htmlExpressionCohort <-
          convertMdToHtml(circeExpressionMarkdown)
        details[[i]]$htmlExpressionConceptSetExpression <-
          convertMdToHtml(circeConceptSetListmarkdown)
      }
      details <- dplyr::bind_rows(details)
    } else {
      return(NULL)
    }
    return(details)
  })
  
  # count number of rows selected
  cohortSearchResultsCountOfSelectedReactive <- shiny::reactive({
    return(length(input$cohortSearchTableResults_rows_selected))
  })
  output$cohortSearchResultsCountOfSelected<- shiny::reactive({
    return(cohortSearchResultsCountOfSelectedReactive())
  })
  shiny::outputOptions(x = output,
                name = "cohortSearchResultsCountOfSelected",
                suspendWhenHidden = FALSE)
  
  shiny::observeEvent(eventExpr = cohortSearchResultsCountOfSelectedReactive() != 2,
                      handlerExpr = {
                        shinyWidgets::updatePickerInput(session = session,
                                                        inputId = "compareCohorts",
                                                        selected = "No Comparision")
                      })

  cohortDetailsTextReactive <- shiny::reactive(x = {
    data <- cohortSearchRowsSelectedDetails()
    if (!is.null(data) && nrow(data) > 0) {
      if (exists("phenotypeDescription")) {
        phenotypeDetails <- phenotypeDescription %>%
          dplyr::filter(.data$phenotypeId %in% data$phenotypeId) %>% 
          dplyr::select(.data$phenotypeId,
                        .data$overview,
                        .data$presentation,
                        .data$assessment,
                        .data$plan,
                        .data$prognosis)
        data <- data %>%
          dplyr::left_join(y = phenotypeDetails,
                           by = "phenotypeId")
      }
      if (!"phenotypeName" %in% colnames(data)) {
        data$phenotypeName <- "Unassigned"
        data$overview <- ""
        data$presentation <- ""
        data$assessment <- ""
        data$plan <- ""
      }
      if (is.null(data)) {
        return(NULL)
      } else {
        details <- list()
        for (i in (1:nrow(data))) {
          details[[i]] <-       tags$table(
            style = "margin-top: 5px;",
            tags$tr(
              tags$td(tags$strong("Cohort ID: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$cohortId)
            ),
            tags$tr(
              tags$td(tags$strong("Cohort Name: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$cohortName)
            ),
            tags$tr(
              tags$td(tags$strong("Logic: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$logicDescription)
            ),
            tags$tr(
              tags$td(tags$strong("Synonyms: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$referentConceptIdsSearchTerms)
            ),
            tags$tr(
              tags$td(tags$strong("Clinical Group: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(paste(data[i, ]$phenotypeName, " (",data[i, ]$phenotypeId,")"))
            ),
            tags$tr(
              tags$td(tags$strong("Overview: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$overview)
            ),
            tags$tr(
              tags$td(tags$strong("Presentation: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$presentation)
            ),
            tags$tr(
              tags$td(tags$strong("Assessment: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$assessment)
            ),
            tags$tr(
              tags$td(tags$strong("Plan: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$plan)
            ),
            tags$tr(
              tags$td(tags$strong("Prognosis: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$prognosis)
            )
          )
        }
        return(details)
      }
    }
  })
  
  cohortConceptSets <- shiny::reactive(x = {
    if (is.null(cohortSearchRowsSelectedDetails())) {
      return(NULL)
    } else {
      details <- list()
      for (i in 1:nrow(cohortSearchRowsSelectedDetails())) {
        details[[i]] <- getConceptSetDetailsFromCohortDefinition(
          cohortDefinitionExpression =
            RJSONIO::fromJSON(cohortSearchRowsSelectedDetails()[i,]$json)
        )
      }
      return(details)
    }
  })
  
  output$cohortDetailsTextFirst <- shiny::renderUI(expr = {
    return(cohortDetailsTextReactive()[[1]])
  })
  output$cohortDefinitionJsonFirst <- shiny::renderText({
    cohortSearchRowsSelectedDetails()[1,]$json
  }) 
  output$cohortDefinitionSqlFirst <- shiny::renderText({
    cohortSearchRowsSelectedDetails()[1,]$sql
  })
  output$cohortDefinitionDetailsFirst <- shiny::renderUI(expr = {
    cohortSearchRowsSelectedDetails()[1, ]$htmlExpressionCohort %>%
      shiny::HTML()
  })
  output$cohortDefinitionConceptSetsTableFirst <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression) &&
          nrow(cohortConceptSets()[[1]]$conceptSetExpression) > 0) {
        data <- cohortConceptSets()[[1]]$conceptSetExpression %>%
          dplyr::select(.data$id, .data$name)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  
  output$cohortDetailsTextSecond <- shiny::renderUI(expr = {
    if (!is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortDetailsTextReactive()[[2]])
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionJsonSecond <- shiny::renderText({
    if (!is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchRowsSelectedDetails()[2,]$json)
    } else {
      return(NULL)
    }
  }) 
  output$cohortDefinitionSqlSecond <- shiny::renderText({
    if (!is.null(cohortSearchRowsSelectedDetails()[[2]])) {
      return(cohortSearchRowsSelectedDetails()[2,]$sql)
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionDetailsSecond <- shiny::renderUI(expr = {
    if (!is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchRowsSelectedDetails()[2, ]$htmlExpressionCohort %>%
               shiny::HTML())
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionConceptSetsTableSecond <-
    DT::renderDT(expr = {
      if (length(cohortConceptSets()) == 2) {
        data <- cohortConceptSets()[[2]]$conceptSetExpression
        if (nrow(data) > 0) {
          data <- data %>%
            dplyr::select(.data$id, .data$name)
          dataTable <- standardDataTable(data = data)
          return(dataTable)
        }
      } else {
        return(NULL)
      }
    })
 
  # selected concept set in a cohort definition.
  cohortConceptSetsSelectedFirst <- shiny::reactive(x = {
    if (is.null(input$cohortDefinitionConceptSetsTableFirst_rows_selected)) {
      return(NULL)
    } else {
      idx <- input$cohortDefinitionConceptSetsTableFirst_rows_selected
      if (length(idx) > 0) {
        if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression) &&
            nrow(cohortConceptSets()[[1]]$conceptSetExpression)) {
          data <- cohortConceptSets()[[1]]$conceptSetExpression[idx, ]
          return(data)
        }
      }     
    }
  })
  output$cohortConceptSetsSelectedFirstRowIsSelected <- shiny::reactive(x = {
    return(!is.null(cohortConceptSetsSelectedFirst()))
  })
  shiny::outputOptions(x = output,
                       name = "cohortConceptSetsSelectedFirstRowIsSelected",
                       suspendWhenHidden = FALSE)
  output$cohortConceptsetExpressionDataTableFirst <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSetsSelectedFirst())) {
        data <- cohortConceptSetsSelectedFirst()
        data <- cohortConceptSets()[[1]]$conceptSetExpressionDetails
        data <- data %>%
          dplyr::filter(.data$id == cohortConceptSetsSelectedFirst()$id)
        dataTable <- standardDataTable(data = data, selectionMode = "single")
        return(dataTable)
      } else {NULL}
    })
  output$cohortConceptsetExpressionJsonFirst <- shiny::renderText({
    cohortConceptSetsSelectedFirst()$json
  })
  
  
  cohortConceptSetsSelectedSecond <- shiny::reactive(x = {
    if (is.null(input$cohortDefinitionConceptSetsTableSecond_rows_selected)) {
      return(NULL)
    } else {
      idx <- input$cohortDefinitionConceptSetsTableSecond_rows_selected
      if (length(idx) > 0 && length(cohortConceptSets()) == 2) {
        data <- cohortConceptSets()[[2]]$conceptSetExpression
        if (nrow(data)) {
          data <- data[idx, ]
          return(data)
        }
      } else {
        return(NULL)
      }
    }
  })
  output$cohortConceptSetsSelectedSecondRowIsSelected <- shiny::reactive(x = {
    return(!is.null(cohortConceptSetsSelectedSecond()))
  })
  shiny::outputOptions(x = output,
                       name = "cohortConceptSetsSelectedSecondRowIsSelected",
                       suspendWhenHidden = FALSE)
  output$cohortConceptsetExpressionDataTableSecond <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSetsSelectedSecond())) {
        data <- cohortConceptSetsSelectedSecond()
        data <- cohortConceptSets()[[1]]$conceptSetExpressionDetails
        data <- data %>%
          dplyr::filter(.data$id == cohortConceptSetsSelectedSecond()$id)
        dataTable <- standardDataTable(data = data, selectionMode = "single")
        return(dataTable)
      } else {NULL}
    })
  output$cohortConceptsetExpressionJsonSecond <- shiny::renderText({
    cohortConceptSetsSelectedSecond()$json
  })
  
  
  # resolved
  resolvedConceptSetExpressionReactiveFirst <-
   shiny::reactive(x = {
     if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression$json)) {
       expression <- cohortConceptSetsSelectedFirst()$expression
       data <- resolveConceptSetExpressionUsingDatabase(dataSource = dataSource,
                                                        conceptSetExpression = expression)
       return(data)
     } else {
       return(NULL)
     }
    })
  output$resolvedConceptSetExpressionDtStandardFirst <-
    DT::renderDT(expr = {
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        data <- resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$resolvedConceptSetExpressionDtMappedFirst <-
    DT::renderDT(expr = {
      data <- resolvedConceptSetExpressionReactiveFirst()$mappedConcepts
      if (!is.null(data)) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  resolvedConceptSetExpressionReactiveSecond <-
    shiny::reactive(x = {
      if (length(cohortConceptSets()) == 2) {
        expression <- cohortConceptSetsSelectedSecond()$expression
          data <-
            resolveConceptSetExpressionUsingDatabase(dataSource = dataSource,
                                                     conceptSetExpression = expression)
          return(data)
      } else {
        return(NULL)
      }
    })
  output$resolvedConceptSetExpressionDtStandardSecond <-
    DT::renderDT(expr = {
      data <- resolvedConceptSetExpressionReactiveSecond()
      if (!is.null(data)) {
        data <- data$resolvedConcepts
        if (!is.null(data) && nrow(data) > 0) {
          dataTable <- standardDataTable(data = data)
          return(dataTable)
        }
      } else {
        return(NULL)
      }
    })
  output$resolvedConceptSetExpressionDtMappedSecond <-
    DT::renderDT(expr = {
      if (!is.null(resolvedConceptSetExpressionReactiveSecond()$mappedConcepts)) {
        data <- resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  
  
  #optimized
  optimizedConceptSetExpressionReactiveFirst <-
    shiny::reactive(x = {
      if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression$json)) {
        expression <- cohortConceptSetsSelectedFirst()$expression
        data <- getOptimizationRecommendationForConceptSetExpression(dataSource = dataSource,
                                                                     conceptSetExpression = expression)
        return(data)
      } else {
        return(NULL)
      }
    })
  output$optimizedConceptSetExpressionDtRetainedFirst <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveFirst()
      if (!is.null(data) &&
          nrow(data) > 0) {
        data <- data %>% 
          dplyr::filter(!.data$conceptId == 0) %>% 
          dplyr::filter(.data$removed == 0) %>% 
          dplyr::select(-.data$removed)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$optimizedConceptSetExpressionDtRemovedFirst <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveFirst()
      if (!is.null(data) &&
          nrow(data) > 0) {
        data <- data %>% 
          dplyr::filter(!.data$conceptId == 0) %>% 
          dplyr::filter(!.data$removed == 0) %>% 
          dplyr::select(-.data$removed)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  optimizedConceptSetExpressionReactiveSecond <-
    shiny::reactive(x = {
      if (!is.null(cohortConceptSets()[[2]]$conceptSetExpression$json)) {
        expression <- cohortConceptSetsSelectedSecond()$expression
        data <- getOptimizationRecommendationForConceptSetExpression(dataSource = dataSource,
                                                                     conceptSetExpression = expression)
        return(data)
      } else {
        return(NULL)
      }
    })
  output$optimizedConceptSetExpressionDtRetainedSecond <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        data <- data %>% 
          dplyr::filter(!.data$conceptId == 0) %>% 
          dplyr::filter(.data$removed == 0) %>% 
          dplyr::select(-.data$removed)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$optimizedConceptSetExpressionDtRemovedSecond <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        data <- data %>% 
          dplyr::filter(!.data$conceptId == 0) %>% 
          dplyr::filter(!.data$removed == 0) %>% 
          dplyr::select(-.data$removed)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  
  
  #recommended
  recommendedConceptSetExpressionStandardReactiveFirst <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        resolvedConcepts <- resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
        mappedConcepts <- resolvedConceptSetExpressionReactiveFirst()$mappedConcepts
        conceptIds <- c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <- loadRecommenderStandardFromDatabase(dataSource = dataSource,
                                                      conceptList = conceptIds)
        }
      }
      return(data)
    })
  recommendedConceptSetExpressionSourceReactiveFirst <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        resolvedConcepts <- resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
        mappedConcepts <- resolvedConceptSetExpressionReactiveFirst()$mappedConcepts
        conceptIds <- c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <- loadRecommenderSourceFromDatabase(dataSource = dataSource,
                                                      conceptList = conceptIds) %>% 
            dplyr::select(-.data$standard)
        }
      }
      return(data)
    })
  output$recommendedConceptSetExpressionDtStandardFirst <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionStandardReactiveFirst()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$recommendedConceptSetExpressionDtSourceFirst <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionSourceReactiveFirst()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  recommendedConceptSetExpressionStandardReactiveSecond <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveSecond())) {
        resolvedConcepts <- resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
        mappedConcepts <- resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        conceptIds <- c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <- loadRecommenderStandardFromDatabase(dataSource = dataSource,
                                                      conceptList = conceptIds)
        }
      }
      return(data)
    })
  recommendedConceptSetExpressionSourceReactiveSecond <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveSecond())) {
        resolvedConcepts <- resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
        mappedConcepts <- resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        conceptIds <- c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <- loadRecommenderSourceFromDatabase(dataSource = dataSource,
                                                    conceptList = conceptIds)
        }
      }
      return(data)
    })
  output$recommendedConceptSetExpressionDtStandardSecond <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionStandardReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$recommendedConceptSetExpressionDtSourceSecond <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionSourceReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  output$logicDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchRowsSelectedDetails()[1,]
    cohort2 <- cohortSearchRowsSelectedDetails()[2,]
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
    file1 <- tempfile()
    writeLines(cohort1$logicDescription, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$logicDescription, con = file2)
    detailsDiffOutput <- diffr::diffr(
      file1,
      file2,
      wordWrap = TRUE,
      before = cohort1$cohortName,
      after = cohort2$cohortName
    )
    unlink(file1)
    unlink(file2)
    return(detailsDiffOutput)
  })
  
  output$jsonDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchRowsSelectedDetails()[1,]
    cohort2 <- cohortSearchRowsSelectedDetails()[2,]
  
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
  
    file1 <- tempfile()
    writeLines(cohort1$json, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$json, con = file2)
    jsonDiffOutput <- diffr::diffr(
      file1,
      file2,
      wordWrap = TRUE,
      before = cohort1$cohortName,
      after = cohort2$cohortName
    )
    unlink(file1)
    unlink(file2)
    return(jsonDiffOutput)
  })
  
  output$sqlDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchRowsSelectedDetails()[1,]
    cohort2 <- cohortSearchRowsSelectedDetails()[2,]
  
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
  
    file1 <- tempfile()
    writeLines(cohort1$sql, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$sql, con = file2)
    sqlDiffOutput <- diffr::diffr(
      file1,
      file2,
      wordWrap = FALSE,
      before = cohort1$cohortName,
      after = cohort2$cohortName,
      width = "100%"
    )
    unlink(file1)
    unlink(file2)
    return(sqlDiffOutput)
  })

  output$phenotypeDescriptionText <- shiny::renderUI(expr = {
    row <- cohortSearchRowsSelectedDetails()[1,]
    if (is.null(row)) {
      return(NULL)
    } else {
      text <-  row$clinicalDescription

      referentConcept <-
        getDetailsForConceptIds(dataSource, row$phenotypeId / 1000)
      if (nrow(referentConcept) > 0) {
        text <-
          paste(
            sprintf(
              "<strong>Referent concept: </strong>%s (concept ID: %s)<br/><br/>",
              referentConcept$conceptName,
              referentConcept$conceptId
            ),
            text
          )
      }
      shiny::HTML(text)
    }
  })

  output$phenotypeLiteratureReviewText <- shiny::renderUI(expr = {
    row <- cohortSearchRowsSelectedDetails()[1,]
    if (is.null(row)) {
      return(NULL)
    } else {
      files <-
        listFilesInGitHub(phenotypeId = row$phenotypeId,
                          subFolder = "literature")
      if (nrow(files) == 0) {
        return("Nothing here (yet)")
      } else {
        return(HTML(paste(files$html, sep = "<br/>")))
      }
    }
  })

  output$phenotypeEvaluationText <- shiny::renderUI(expr = {
    row <- cohortSearchRowsSelectedDetails()[1,]
    if (is.null(row)) {
      return(NULL)
    } else {
      files <-
        listFilesInGitHub(phenotypeId = row$phenotypeId,
                          subFolder = "evaluation")
      if (nrow(files) == 0) {
        return("Nothing here (yet)")
      } else {
        return(HTML(paste(files$html, sep = "<br/>")))
      }
    }
  })

  output$phenotypeNotesText <- shiny::renderUI(expr = {
    row <- cohortSearchRowsSelectedDetails()[1,]
    if (is.null(row)) {
      return(NULL)
    } else {
      files <-
        listFilesInGitHub(phenotypeId = row$phenotypeId,
                          subFolder = "notes")
      if (nrow(files) == 0) {
        return("Nothing here (yet)")
      } else {
        return(HTML(paste(files$html, sep = "<br/>")))
      }
    }
  })
  

  
  ######## After selecting cohort using button ################
  # shiny header drop down options
  headerFilterOptionsPhenotypeDatabaseCohort <- shiny::reactive(x = {
    if (nrow(cohortSearchRowsSelected()) != 0) {
      data <- combinationsOfPhenotypeDatabaseCohort %>%
        dplyr::filter(.data$cohortId %in%
                        cohortSearchRowsSelected()$cohortId) %>%
        dplyr::left_join(
          y = database %>%
            dplyr::select(.data$databaseId,
                          .data$databaseName),
          by = "databaseId"
        ) %>%
        dplyr::left_join(
          y = cohort %>%
            dplyr::select(.data$cohortId,
                          .data$cohortName),
          by = "cohortId"
        )
      
      if (exists("phenotypeDescription")) {
        data <- data %>%
          dplyr::left_join(
            y = phenotypeDescription %>%
              dplyr::select(.data$phenotypeId,
                            .data$phenotypeName),
            by = "phenotypeId"
          )
      } else {
        data$phenotypeName <- "No phenotype name"
      }
    } else {
      return(dplyr::tibble())
    }
    return(data)
  }, 
  label = "drop down options")
  
  optionsForDropDownDatabase <- shiny::reactive(x = {
    return(
      headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$databaseId, .data$databaseName) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$databaseName)
    )
  })
  optionsForDropDownCohort <- shiny::reactive(x = {
    return(
      headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$cohortId, .data$cohortName) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$cohortName)
    )
  })
  optionsForDropDownPhenotype <- shiny::reactive(x = {
    return(
      headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$phenotypeId, .data$phenotypeName) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$phenotypeName)
    )
  })
  
  shinyWidgetsPickerOptions <- shinyWidgets::pickerOptions(
    actionsBox = TRUE,
    liveSearch = TRUE,
    liveSearchNormalize = TRUE,
    size = 10,
    liveSearchStyle = "contains",
    liveSearchPlaceholder = "Not selected",
    virtualScroll = 50,
    # mobile = TRUE,
    selectOnTab = TRUE,
    showTick = TRUE,
    width	= TRUE,
    windowPadding = 2
  )
  
  shiny::observeEvent(eventExpr = input$loadSelectedCohorts,
                      handlerExpr = {
                        shinyWidgets::updatePickerInput(
                          session = session,
                          inputId = "selectedDatabases",
                          label = "Database",
                          selected = optionsForDropDownDatabase()$databaseName,
                          choices = optionsForDropDownDatabase()$databaseName
                        )
                        shinyWidgets::updatePickerInput(
                          session = session,
                          inputId = "selectedCohorts",
                          label = "Cohort",
                          selected = optionsForDropDownCohort()$cohortName,
                          choices = optionsForDropDownCohort()$cohortName
                        )
                        shinyWidgets::updatePickerInput(
                          session = session,
                          inputId = "selectedPhenotypes",
                          choicesOpt = list(style = rep_len("color: black;", 999)),
                          choices = optionsForDropDownPhenotype()$phenotypeName,
                          selected = optionsForDropDownPhenotype()$phenotypeName
                        )
                        
                        # filter combinations
                        filterCombinations <- shiny::reactive(x = {
                          validate(need(
                            expr = length(input$selectedCohorts) > 0, message = "No cohorts selected"
                          ))
                          data <- headerFilterOptionsPhenotypeDatabaseCohort() %>% 
                            dplyr::filter(.data$cohortName %in% input$selectedCohorts &
                                            .data$databaseName %in% input$selectedDatabases &
                                            .data$phenotypeName %in% input$selectedPhenotypes) %>% 
                            dplyr::distinct() 
                          return(data)
                        })
                        
                        cohortCountsFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getCohortCountResult(dataSource = dataSource,
                                                           cohortIds = cohortSearchRowsSelected()$cohortId)
                            } else {
                              data <- dplyr::tibble()
                            }
                            return(data)
                          })
                        # Cohort Counts --------------------------------------------------------------------------
                        cohortCountsDataFiltered <- reactive({
                          validate(need(
                            expr = length(input$selectedCohorts) > 0, message = "No cohorts selected"
                          ))
                          data <- cohortCountsFromRemote() %>%
                            dplyr::filter(
                              .data$databaseId %in% filterCombinations()$databaseId &
                              .data$cohortId %in% filterCombinations()$cohortId
                            )
                          return(data)
                        })
                        output$cohortCountsTable <- DT::renderDT(expr = {
                          data <- cohortCountsDataFiltered()
                          if (nrow(data) > 0) {
                            data <- addMetaDataInformationToResults(data)
                          }
                          dataTable <- standardDataTable(data = data)
                          return(dataTable)
                        }, server = TRUE)
                        
                        
                        incidenceRateDataFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getIncidenceRateResult(dataSource = dataSource,
                                                             cohortIds = cohortSearchRowsSelected()$cohortId)
                            } else {
                              data <- dplyr::tibble()
                            }
                            return(data)
                          })
                        
                        if (nrow(incidenceRateDataFromRemote()) > 0) {
                          ageFilter <-
                            incidenceRateDataFromRemote()$ageGroup %>% unique()
                          ageFilter <-
                            ageFilter[!ageFilter == 'All']
                          genderFilter <-
                            incidenceRateDataFromRemote()$gender %>% unique()
                          genderFilter <-
                            genderFilter[!genderFilter == 'All']
                          calendarFilter <-
                            incidenceRateDataFromRemote()$calendarYear %>% unique()
                          calendarFilter <-
                            calendarFilter[!calendarFilter == 'All']
                        } else {
                          ageFilter <- NULL
                          genderFilter <- NULL
                          calendarFilter <- NULL
                        }
                        
                        shiny::updateSelectizeInput(
                          session = session,
                          inputId = "incidenceRateAgeFilter",
                          label = "Age",
                          selected = ageFilter,
                          choices = ageFilter,
                          server = TRUE
                        )
                        shiny::updateSelectizeInput(
                          session = session,
                          inputId = "incidenceRateGenderFilter",
                          label = "Gender",
                          selected = genderFilter,
                          choices = genderFilter,
                          server = TRUE
                        )
                        shiny::updateSelectizeInput(
                          session = session,
                          inputId = "incidenceRateCalenderFilter",
                          label = "Calendar Year",
                          selected = calendarFilter,
                          choices = calendarFilter,
                          server = TRUE
                        )
                        
                        timeDistributionFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getTimeDistributionResult(dataSource = dataSource,
                                                                cohortIds = cohortSearchRowsSelected()$cohortId)
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        inclusionRuleTableFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getInclusionRuleStats(dataSource = dataSource,
                                                            cohortIds = cohortSearchRowsSelected()$cohortId)
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        indexEventBreakDownDataFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getIndexEventBreakdown(
                                dataSource = dataSource,
                                cohortIds = cohortSearchRowsSelected()$cohortId,
                                cohortCounts = cohortCountsFromRemote()
                              )
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        visitContextDataFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getVisitContextResults(
                                dataSource = dataSource,
                                cohortIds = cohortSearchRowsSelected()$cohortId,
                                cohortCounts = cohortCountsFromRemote()
                              )
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        characterizationDataFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getCovariateValueResult(
                                dataSource = dataSource,
                                table = "covariateValue",
                                cohortIds = cohortSearchRowsSelected()$cohortId
                              )
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        temporalCharacterizationDataFromRemote <-
                          shiny::reactive(x = {
                            if (nrow(cohortSearchRowsSelected()) != 0) {
                              data <- getCovariateValueResult(
                                dataSource = dataSource,
                                table = "temporalCovariateValue",
                                cohortIds = cohortSearchRowsSelected()$cohortId
                              )
                            } else {
                              return(dplyr::tibble())
                            }
                            return(data)
                          })
                        
                        cohortOverlapFromRemote <- shiny::reactive({
                          combisOfTargetComparator <-
                            tidyr::crossing(
                              targetCohortId = cohortSearchRowsSelected()$cohortId,
                              comparatorCohortId = cohortSearchRowsSelected()$cohortId
                            ) %>%
                            dplyr::filter(!.data$targetCohortId == .data$comparatorCohortId) %>%
                            dplyr::distinct()
                          validate(need(
                            nrow(combisOfTargetComparator) > 0,
                            paste0("Please select at least two cohorts.")
                          ))
                          data <- getCohortOverlapResult(
                            dataSource = dataSource,
                            targetCohortIds = combisOfTargetComparator$targetCohortId,
                            comparatorCohortIds = combisOfTargetComparator$comparatorCohortId
                          )
                          return(data)
                        })
                        
                        temporalCharacterizationDataFilterOptions <-
                          shiny::reactive({
                            data <- temporalCharacterizationDataFromRemote() %>%
                              dplyr::select(.data$timeId,
                                            .data$covariateId) %>%
                              dplyr::distinct() %>%
                              dplyr::left_join(temporalCovariateChoices, by = "timeId") %>%
                              dplyr::left_join(temporalCovariateRef, by = "covariateId") %>%
                              dplyr::left_join(
                                temporalAnalysisRef %>%
                                  dplyr::select(.data$analysisId,
                                                .data$analysisName,
                                                .data$domainId),
                                by = "analysisId"
                              )
                            return(data)
                          })
                        
                        
                        if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
                          temporalAnalysisNameFilter <-
                            temporalCharacterizationDataFilterOptions()$analysisName %>% unique()
                          temporalDomainFilter <-
                            temporalCharacterizationDataFilterOptions()$domainId %>% unique()
                        } else {
                          temporalAnalysisNameFilter <- NULL
                          temporalDomainFilter <- NULL
                        }
                        
                        shiny::updateSelectizeInput(
                          session = session,
                          inputId = "temporalAnalysisNameFilter",
                          label = "Analysis Choices",
                          selected = temporalAnalysisNameFilter,
                          choices = temporalAnalysisNameFilter,
                          server = TRUE
                        )
                        shiny::updateSelectizeInput(
                          session = session,
                          inputId = "temporalDomainFilter",
                          label = "Domain Choices",
                          selected = temporalDomainFilter,
                          choices = temporalDomainFilter,
                          server = TRUE
                        )
                      })
  

  
  # cohortId <- shiny::reactive(x = {
  #   return(cohort %>%
  #            dplyr::filter(.data$cohortId %in% input$selectedCohorts))
  # })
  #
  # cohortIds <- shiny::reactive(x = {
  #   return(cohort$cohortId[cohort$cohortName  %in% input$selectedCohorts])
  # })
  
  # timeId <- shiny::reactive(x = {
  #   return(
  #     temporalCovariateChoices %>%
  #       dplyr::filter(choices %in% input$timeIdChoices) %>%
  #       dplyr::pull(timeId)
  #   )
  # })
  #
  # phenotypeId <- shiny::reactive(x = {
  #   return(phenotypeDescription$phenotypeId[phenotypeDescription$phenotypeName == input$phenotypes])
  # })
  
  # if (exists(x = "phenotypeDescription")) {
  #   shiny::observe(x = {
  #     idx <- which(phenotypeDescription$phenotypeName == input$phenotypes)
  #     shiny::isolate({
  #       proxy <- DT::dataTableProxy(
  #         outputId = "phenoTypeDescriptionTable",
  #         session = session,
  #         deferUntilFlush = FALSE
  #       )
  #       DT::selectRows(proxy, idx)
  #       DT::selectPage(
  #         proxy,
  #         which(input$phenoTypeDescriptionTable_rows_all == idx) %/%
  #           input$phenoTypeDescriptionTable_state$length + 1
  #       )
  #     })
  #   })
  # }
  
  # cohortSubset <- shiny::reactive(x = {
  #     return(cohort %>%
  #              dplyr::arrange(.data$cohortId))
  # })
  
  
  
  # phenotypeSubset <- shiny::reactive(x = {
  #   if (exists(x = "phenotypeDescription")) {
  #     return(phenotypeDescription %>%
  #              dplyr::arrange(.data$phenotypeId))
  #   }
  # })
  
  # shiny::observe(x = {
  #   subset <-
  #     unique(conceptSets$conceptSetName[conceptSets$cohortId == cohortId()]) %>% sort()
  #   shinyWidgets::updatePickerInput(
  #     session = session,
  #     inputId = "conceptSet",
  #     choicesOpt = list(style = rep_len("color: black;", 999)),
  #     choices = subset
  #   )
  # })
  
  # Phenotype Description ------------------------------------------------------------------------------
  # output$phenoTypeDescriptionTable <- DT::renderDT(expr = {
  #   data <- phenotypeDescription %>%
  #     dplyr::select(
  #       .data$phenotypeId,
  #       .data$phenotypeName,
  #       .data$overview,
  #       .data$cohortDefinitions
  #     )
  #   dataTable <- standardDataTable(data = data)
  #   return(dataTable)
  # }, server = TRUE)
  
  # selectedPhenotypeDescriptionRow <- reactive({
  #   idx <- input$phenoTypeDescriptionTable_rows_selected
  #   if (is.null(idx)) {
  #     return(NULL)
  #   } else {
  #     row <- phenotypeDescription[idx, ]
  #     return(row)
  #   }
  # })
  
  # output$phenotypeRowIsSelected <- reactive({
  #   return(!is.null(selectedPhenotypeDescriptionRow()))
  # })
  
  # output$phenotypeDescriptionText <- shiny::renderUI(expr = {
  #   row <- selectedPhenotypeDescriptionRow()
  #   if (is.null(row)) {
  #     return(NULL)
  #   } else {
  #     text <-  row$clinicalDescription
  #
  #     referentConcept <-
  #       getConceptDetails(dataSource, row$phenotypeId / 1000)
  #     if (nrow(referentConcept) > 0) {
  #       text <-
  #         paste(
  #           sprintf(
  #             "<strong>Referent concept: </strong>%s (concept ID: %s)<br/><br/>",
  #             referentConcept$conceptName,
  #             referentConcept$conceptId
  #           ),
  #           text
  #         )
  #     }
  #     shiny::HTML(text)
  #   }
  # })
  
  # output$phenotypeLiteratureReviewText <- shiny::renderUI(expr = {
  #   row <- selectedPhenotypeDescriptionRow()
  #   if (is.null(row)) {
  #     return(NULL)
  #   } else {
  #     files <-
  #       listFilesInGitHub(phenotypeId = row$phenotypeId,
  #                         subFolder = "literature")
  #     if (nrow(files) == 0) {
  #       return("Nothing here (yet)")
  #     } else {
  #       return(HTML(paste(files$html, sep = "<br/>")))
  #     }
  #   }
  #   table <- standardDataTable(data)
  #   return(table)
  # })
  

  
  
  
  
  
  
  
  
  
  
  

  

  
  

  
  
  # shiny::observeEvent(input$loadSelectedCohorts, {
  #   cohortOptions <- cohortSearchForComparison()$cohortName
  #   phenotypeOptions <- cohortSearchForComparison()$phenotypeName %>% unique()
  
  # shinyWidgets::updatePickerInput(
  #   session = session,
  #   inputId = "selectedCohorts",
  #   choicesOpt = list(style = rep_len("color: black;", 999)),
  #   choices = cohortOptions,
  #   selected = c(cohortOptions[1], cohortOptions[2])
  # )
  #
  # shinyWidgets::updatePickerInput(
  #   session = session,
  #   inputId = "phenotypes",
  #   choicesOpt = list(style = rep_len("color: black;", 999)),
  #   choices = phenotypeOptions
  # )
  # })
  
  # Phenotype Description ------------------------------------------------------------------------------
  # output$phenoTypeDescriptionTable <- DT::renderDT(expr = {
  #   data <- phenotypeDescription %>%
  #     dplyr::select(
  #       .data$phenotypeId,
  #       .data$phenotypeName,
  #       .data$overview,
  #       .data$cohortDefinitions
  #     )
  #   dataTable <- standardDataTable(data = data)
  #   return(dataTable)
  # }, server = TRUE)
  #
  # selectedPhenotypeDescriptionRow <- reactive({
  #   idx <- input$phenoTypeDescriptionTable_rows_selected
  #   if (is.null(idx)) {
  #     return(NULL)
  #   } else {
  #     row <- phenotypeDescription[idx, ]
  #     return(row)
  #   }
  # })
  #
  # output$phenotypeRowIsSelected <- reactive({
  #   return(!is.null(selectedPhenotypeDescriptionRow()))
  # })
  # outputOptions(x = output,
  #               name = "phenotypeRowIsSelected",
  #               suspendWhenHidden = TRUE)
  
  # observeEvent(input$selectPhenotypeButton, {
  #   shinyWidgets::updatePickerInput(
  #     session = session,
  #     inputId = "phenotypes",
  #     selected = selectedPhenotypeDescriptionRow()$phenotypeName
  #   )
  # })
  
  # Cohort Definition ---------------------------------------------------------
  # output$cohortDefinitionTable <- DT::renderDT(expr = {
  #   data <- cohort %>%
  #     dplyr::select(.data$cohortId, .data$cohortName)
  #   dataTable <-
  #     standardDataTable(data = data, selectionMode = 'multiple')
  #   return(dataTable)
  # }, server = TRUE)
  


  
  # Incidence rate --------------------------------------------------------------------------------
  incidenceRateDataFiltered <- reactive({
    validate(need(
      expr = length(input$selectedCohorts) > 0,
      message = "No cohorts selected"
    ))
    stratifyByAge <- "Age" %in% input$irStratification
    stratifyByGender <- "Gender" %in% input$irStratification
    stratifyByCalendarYear <-
      "Calendar Year" %in% input$irStratification
    
    data <- incidenceRateDataFromRemote() %>%
      dplyr::filter(.data$databaseId %in% input$selectedDatabases)
    
    if (stratifyByAge) {
      data <- data %>%
        dplyr::filter(.data$ageGroup %in% input$incidenceRateAgeFilter)
    }
    if (stratifyByCalendarYear) {
      data <- data %>%
        dplyr::filter(.data$calendarYear %in% input$incidenceRateCalenderFilter)
    }
    if (stratifyByGender) {
      data <- data %>%
        dplyr::filter(.data$gender %in% input$incidenceRateGenderFilter)
    }
    return(data)
  })
  
  output$incidenceRatePlot <- ggiraph::renderggiraph(expr = {
    data <- incidenceRateDataFiltered()
    stratifyByAge <- "Age" %in% input$irStratification
    stratifyByGender <- "Gender" %in% input$irStratification
    stratifyByCalendarYear <-
      "Calendar Year" %in% input$irStratification
    plot <- plotIncidenceRate(
      data = data,
      shortNameRef = cohort,
      stratifyByAgeGroup = stratifyByAge,
      stratifyByGender = stratifyByGender,
      stratifyByCalendarYear = stratifyByCalendarYear,
      yscaleFixed = input$irYscaleFixed,
      minPersonYears = 1000
    )
    return(plot)
  })
  
  output$incidenceRateTable <- DT::renderDT(expr = {
    data <- incidenceRateDataFiltered()
    if (nrow(data) > 0) {
      data <- addMetaDataInformationToResults(data)
      colnames(data) <-
        colnames(data) %>% stringr::str_replace_all(string = .,
                                                    pattern = "Value",
                                                    replacement = "")
    }
    table <- standardDataTable(data)
    return(table)
  }, server = TRUE)
  
  # Time distribution -----------------------------------------------------------------------------
  timeDistributionFiltered <- reactive({
    validate(need(
      expr = length(input$selectedCohorts) > 0,
      message = "No cohorts selected"
    ))
    data <- timeDistributionFromRemote()
    if (nrow(data) > 0) {
      data <- timeDistributionFromRemote() %>%
        dplyr::filter(
          .data$databaseId %in% input$selectedDatabases,
          .data$cohortId %in% input$selectedCohorts
        )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$timeDistributionPlot <- ggiraph::renderggiraph(expr = {
    data <- timeDistributionFiltered()
    validate(need(nrow(data) > 0, paste0("No data for this combination")))
    plot <- plotTimeDistribution(data = data,
                                 shortNameRef = cohort)
    return(plot)
  })
  output$timeDistributionTable <- DT::renderDT(expr = {
    data <- timeDistributionFiltered()
    if (nrow(data) > 0) {
      data <- data %>% dplyr::relocate(.data$timeMetric)
      data <- addMetaDataInformationToResults(data)
      colnames(data) <-
        colnames(data) %>% stringr::str_replace_all(string = .,
                                                    pattern = "Value",
                                                    replacement = "")
    }
    table <- standardDataTable(data)
    return(table)
  }, server = TRUE)
  
  # included concepts table --------------------------------------------------------------------------
  # output$includedConceptsTable <- DT::renderDT(expr = {
  #   validate(need(length(input$selectedDatabases) > 0, "No data sources chosen"))
  #   data <- getIncludedConceptResult(
  #     dataSource = dataSource,
  #     cohortId = cohortId(),
  #     databaseIds = input$selectedDatabases
  #   )
  #   data <- data %>%
  #     dplyr::filter(.data$conceptSetName == input$conceptSet)
  #   if (nrow(data) == 0) {
  #     return(dplyr::tibble("No data available for selected databases and cohorts"))
  #   }
  #
  #   databaseIds <- unique(data$databaseId)
  #
  #   if (!all(input$selectedDatabases %in% databaseIds)) {
  #     return(dplyr::tibble(
  #       Note = paste0(
  #         "There is no data for the databases:\n",
  #         paste0(setdiff(input$selectedDatabases, databaseIds),
  #                collapse = ",\n "),
  #         ".\n Please unselect them."
  #       )
  #     ))
  #   }
  #
  #   maxCount <- max(data$conceptCount, na.rm = TRUE)
  #
  #   if (input$includedType == "Source Concepts") {
  #     table <- data %>%
  #       dplyr::select(
  #         .data$databaseId,
  #         .data$sourceConceptId,
  #         .data$conceptSubjects,
  #         .data$conceptCount
  #       ) %>%
  #       dplyr::arrange(.data$databaseId) %>%
  #       tidyr::pivot_longer(cols = c(.data$conceptSubjects, .data$conceptCount)) %>%
  #       dplyr::mutate(name = paste0(
  #         databaseId,
  #         "_",
  #         stringr::str_replace(
  #           string = .data$name,
  #           pattern = 'concept',
  #           replacement = ''
  #         )
  #       )) %>%
  #       tidyr::pivot_wider(
  #         id_cols = c(.data$sourceConceptId),
  #         names_from = .data$name,
  #         values_from = .data$value
  #       ) %>%
  #       dplyr::inner_join(
  #         data %>%
  #           dplyr::select(
  #             .data$sourceConceptId,
  #             .data$sourceConceptName,
  #             .data$sourceVocabularyId,
  #             .data$sourceConceptCode
  #           ) %>%
  #           dplyr::distinct(),
  #         by = "sourceConceptId"
  #       ) %>%
  #       dplyr::relocate(
  #         .data$sourceConceptId,
  #         .data$sourceConceptName,
  #         .data$sourceVocabularyId,
  #         .data$sourceConceptCode
  #       )
  #
  #     if (nrow(table) == 0) {
  #       return(dplyr::tibble(
  #         Note = paste0("No data available for selected databases and cohorts")
  #       ))
  #     }
  #     table <- table[order(-table[, 5]), ]
  #     dataTable <- standardDataTable(table)
  #   } else {
  #     table <- data %>%
  #       dplyr::select(
  #         .data$databaseId,
  #         .data$conceptId,
  #         .data$conceptSubjects,
  #         .data$conceptCount
  #       ) %>%
  #       dplyr::group_by(.data$databaseId,
  #                       .data$conceptId) %>%
  #       dplyr::summarise(
  #         conceptSubjects = sum(.data$conceptSubjects),
  #         conceptCount = sum(.data$conceptCount)
  #       ) %>%
  #       dplyr::ungroup() %>%
  #       dplyr::arrange(.data$databaseId) %>%
  #       tidyr::pivot_longer(cols = c(.data$conceptSubjects, .data$conceptCount)) %>%
  #       dplyr::mutate(name = paste0(
  #         databaseId,
  #         "_",
  #         stringr::str_replace(
  #           string = .data$name,
  #           pattern = "concept",
  #           replacement = ""
  #         )
  #       )) %>%
  #       tidyr::pivot_wider(
  #         id_cols = c(.data$conceptId),
  #         names_from = .data$name,
  #         values_from = .data$value
  #       ) %>%
  #       dplyr::inner_join(
  #         data %>%
  #           dplyr::select(.data$conceptId,
  #                         .data$conceptName,
  #                         .data$vocabularyId) %>%
  #           dplyr::distinct(),
  #         by = "conceptId"
  #       ) %>%
  #       dplyr::relocate(.data$conceptId, .data$conceptName, .data$vocabularyId)
  #
  #     if (nrow(table) == 0) {
  #       return(dplyr::tibble(
  #         Note = paste0('No data available for selected databases and cohorts')
  #       ))
  #     }
  #     table <- table[order(-table[, 4]), ]
  #     dataTable <- standardDataTable(table)
  #   }
  #   return(dataTable)
  # }, server = TRUE)
  
  # orphan concepts table -------------------------------------------------------------------------
  # output$orphanConceptsTable <- DT::renderDT(expr = {
  #   validate(need(length(input$selectedDatabases) > 0, "No data sources chosen"))
  #
  #   data <- getOrphanConceptResult(
  #     dataSource = dataSource,
  #     cohortId = cohortId(),
  #     databaseIds = input$selectedDatabases
  #   )
  #   data <- data %>%
  #     dplyr::filter(.data$conceptSetName == input$conceptSet)
  #
  #   if (nrow(data) == 0) {
  #     return(dplyr::tibble(Note = paste0(
  #       "There is no data for the selected combination."
  #     )))
  #   }
  #   databaseIds <- unique(data$databaseId)
  #
  #   if (!all(input$selectedDatabases %in% databaseIds)) {
  #     return(dplyr::tibble(
  #       Note = paste0(
  #         "There is no data for the databases:\n",
  #         paste0(setdiff(input$selectedDatabases, databaseIds),
  #                collapse = ",\n "),
  #         ".\n Please unselect them."
  #       )
  #     ))
  #   }
  #
  #   maxCount <- max(data$conceptCount, na.rm = TRUE)
  #
  #   table <- data %>%
  #     dplyr::select(.data$databaseId,
  #                   .data$conceptId,
  #                   .data$conceptSubjects,
  #                   .data$conceptCount) %>%
  #     dplyr::group_by(.data$databaseId,
  #                     .data$conceptId) %>%
  #     dplyr::summarise(
  #       conceptSubjects = sum(.data$conceptSubjects),
  #       conceptCount = sum(.data$conceptCount)
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::arrange(.data$databaseId) %>%
  #     tidyr::pivot_longer(cols = c(.data$conceptSubjects, .data$conceptCount)) %>%
  #     dplyr::mutate(name = paste0(
  #       databaseId,
  #       "_",
  #       stringr::str_replace(
  #         string = .data$name,
  #         pattern = "concept",
  #         replacement = ""
  #       )
  #     )) %>%
  #     tidyr::pivot_wider(
  #       id_cols = c(.data$conceptId),
  #       names_from = .data$name,
  #       values_from = .data$value
  #     ) %>%
  #     dplyr::inner_join(
  #       data %>%
  #         dplyr::select(
  #           .data$conceptId,
  #           .data$conceptName,
  #           .data$vocabularyId,
  #           .data$conceptCode
  #         ) %>%
  #         dplyr::distinct(),
  #       by = "conceptId"
  #     ) %>%
  #     dplyr::relocate(.data$conceptId,
  #                     .data$conceptName,
  #                     .data$vocabularyId,
  #                     .data$conceptCode)
  #
  #   if (nrow(table) == 0) {
  #     return(dplyr::tibble(
  #       Note = paste0('No data available for selected databases and cohorts')
  #     ))
  #   }
  #
  #   table <- table[order(-table[, 5]), ]
  #   dataTable <- standardDataTable(table)
  #   return(table)
  # }, server = TRUE)
  
  # Concept set diagnostics ---------------------------------------------------------------------
  
  
  # Inclusion rules table -----------------------------------------------------------------------
  inclusionRuleFiltered <- reactive({
    validate(need(
      expr = length(input$selectedCohorts) > 0,
      message = "No cohorts selected"
    ))
    if (nrow(inclusionRuleTableFromRemote()) > 0) {
      data <- inclusionRuleTableFromRemote() %>%
        dplyr::filter(
          .data$databaseId %in% input$selectedDatabases,
          .data$cohortId %in% input$selectedCohorts
        )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$inclusionRuleTable <- DT::renderDT(expr = {
    if (nrow(inclusionRuleFiltered()) > 0) {
      data <- inclusionRuleFiltered()
      data <- addMetaDataInformationToResults()
      table <- standardDataTable(data)
      return(table)
    } else {
      return(dplyr::tibble())
    }
  }, server = TRUE)
  
  
  # Index event breakdown ----------------------------------------------------------------
  indexEventBreakDownDataFiltered <- reactive({
    validate(need(
      expr = length(input$selectedCohorts) > 0,
      message = "No cohorts selected"
    ))
    if (nrow(indexEventBreakDownDataFromRemote()) > 0) {
      data <- indexEventBreakDownDataFromRemote() %>%
        dplyr::filter(
          .data$databaseId %in% input$selectedDatabases,
          .data$cohortId %in% input$selectedCohorts
        )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$indexEventBreakDownTable <- DT::renderDT(expr = {
    data <-
      addMetaDataInformationToResults(indexEventBreakDownDataFiltered()) %>%
      dplyr::arrange(dplyr::desc(.data$percent))
    dataTable <- standardDataTable(data)
    return(dataTable)
  }, server = TRUE)
  
  
  # Visit Context --------------------------------------------------------------------------------------------
  visitContextDataFiltered <- reactive({
    validate(need(
      expr = length(input$selectedCohorts) > 0,
      message = "No cohorts selected"
    ))
    if (nrow(visitContextDataFromRemote()) > 0) {
      data <- visitContextDataFromRemote() %>%
        dplyr::filter(
          .data$databaseId %in% input$selectedDatabases,
          .data$cohortId %in% input$selectedCohorts
        )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$visitContextTable <- DT::renderDT(expr = {
    data <-
      addMetaDataInformationToResults(visitContextDataFiltered())
    table <- standardDataTable(data)
  }, server = TRUE)
  
  
  # Characterization -----------------------------------------------------------------
  characterizationDataFiltered <- shiny::reactive(x = {
    validate(need(
      length(input$selectedDatabases) > 0,
      "No data sources chosen"
    ))
    data <- characterizationDataFromRemote() %>%
      dplyr::filter(databaseId %in% input$selectedDatabases)
  })
  
  # Characterization --------------------------------------------------
  characterizationTableRaw <- shiny::reactive(x = {
    data <- characterizationDataFiltered()
    data <- addMetaDataInformationToResults(data)
    return(data)
  })
  
  characterizationTablePretty <- shiny::reactive(x = {
    data <- characterizationDataFiltered()
    analysisIds <- prettyAnalysisIds
    table <- data %>%
      prepareTable1(covariateRef = covariateRef) %>%
      dplyr::rename(percent = .data$value)
    characteristics <- table %>%
      dplyr::select(.data$characteristic,
                    .data$position,
                    .data$header,
                    .data$sortOrder) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$characteristic, .data$position, .data$header) %>%
      dplyr::summarise(sortOrder = max(.data$sortOrder)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$position, desc(.data$header)) %>%
      dplyr::mutate(sortOrder = dplyr::row_number()) %>%
      dplyr::distinct()
    
    characteristics <- dplyr::bind_rows(
      tidyr::crossing(
        characteristics %>%
          dplyr::filter(.data$header == 1),
        dplyr::tibble(cohortId = cohortSearchForComparison()$cohortId),
        dplyr::tibble(databaseId = input$selectedDatabases)
      ),
      characteristics %>%
        dplyr::filter(.data$header == 0) %>%
        tidyr::crossing(dplyr::tibble(databaseId = input$selectedDatabases)) %>%
        tidyr::crossing(
          dplyr::tibble(cohortId = cohortSearchForComparison()$cohortId)
        )
    )
    data <- characteristics %>%
      dplyr::left_join(
        table %>%
          dplyr::select(-.data$sortOrder),
        by = c(
          "databaseId",
          "cohortId",
          "characteristic",
          "position",
          "header"
        )
      )  %>%
      dplyr::arrange(.data$databaseId, .data$cohortId, .data$sortOrder) %>%
      dplyr::select(-.data$position, -.data$header) %>%
      dplyr::relocate(.data$sortOrder, .after = dplyr::last_col())
    data <- addMetaDataInformationToResults(data)
    return(data)
  })
  
  output$characterizationTablePretty <- DT::renderDT(expr = {
    data <- characterizationTablePretty()
    table <- standardDataTable(data)
    return(table)
  })
  output$characterizationTableRaw <- DT::renderDT(expr = {
    data <- characterizationTableRaw()
    table <- standardDataTable(data)
    return(table)
  })
  
  # covariateIdArray <- reactiveVal()
  # covariateIdArray(c())
  # observeEvent(input$rows, {
  #   if (input$rows[[2]] %in% covariateIdArray())
  #     covariateIdArray(covariateIdArray()[covariateIdArray() %in% input$rows[[2]] == FALSE])
  #   else
  #     covariateIdArray(c(covariateIdArray(), input$rows[[2]]))
  # })
  
  # Temporal characterization -----------------------------------------------------------------
  temporalCharacterizationDataFiltered <- shiny::reactive(x = {
    validate(need(
      length(input$selectedDatabases) > 0,
      "No data sources chosen"
    ))
    if (all(
      is.null(input$temporalChoicesFilter),
      is.null(input$temporalAnalysisNameFilter),
      is.null(input$temporalDomainFilter)
    )) {
      return(dplyr::tibble())
    }
    dataFilterOptions <-
      temporalCharacterizationDataFilterOptions() %>%
      dplyr::filter(
        # choices %in% input$timeIdChoices,
        analysisName %in% input$temporalAnalysisNameFilter,
        domainId %in% input$temporalDomainFilter
      )
    
    data <- temporalCharacterizationDataFromRemote() %>%
      dplyr::filter(databaseId %in% input$selectedDatabases) %>%
      dplyr::inner_join(y = dataFilterOptions,
                        by = c("timeId" = "timeId",
                               "covariateId" = "covariateId")) %>%
      dplyr::rename(temporalChoices = .data$choices) %>%
      dplyr::relocate(
        .data$temporalChoices,
        .data$analysisName,
        .data$domainId,
        .data$covariateName
      )
    return(data)
  })
  
  output$temporalCharacterizationTable <-
    DT::renderDT(expr = {
      data <- temporalCharacterizationDataFiltered() %>%
        dplyr::select(-.data$timeId,
                      -.data$covariateId,
                      -.data$analysisId)
      data <- addMetaDataInformationToResults(data = data)
      table <- standardDataTable(data = data)
      return(table)
    }, server = TRUE)
  
  
  
  #
  #
  #
  # # Temporal characterization table that shows the covariates selected by lasso method
  # output$temporalCharacterizationCovariateLassoTable <-
  #   DT::renderDT(expr = {
  #     data <- temporalCharacterizationDataFiltered()
  #     if (nrow(data) > 1000) {
  #       data <- data %>%
  #         dplyr::filter(.data$mean > 0.01)
  #     }
  #     table <- data %>%
  #       dplyr::select(.data$covariateName)
  #     table <- table[selectedPlotPoints(),]
  #     options = list(
  #       pageLength = 10,
  #       searching = TRUE,
  #       searchHighlight = TRUE,
  #       scrollX = TRUE,
  #       lengthChange = TRUE,
  #       ordering = TRUE,
  #       paging = TRUE,
  #       stateSave = TRUE,
  #       dom = 'tip',
  #       columnDefs = list(truncateStringDef(0, 40))
  #     )
  #
  #     table <- DT::datatable(
  #       table,
  #       options = options,
  #       rownames = FALSE,
  #       colnames = colnames(table) %>%
  #         camelCaseToTitleCase(),
  #       escape = FALSE,
  #       filter = "top",
  #       class = "stripe nowrap compact"
  #     )
  #     return(table)
  #   })
  #
  # selectedtemporalCharacterizationCovariateRow <- reactive({
  #   # _row_selected is an inbuilt property of DT that provides the index of selected row.
  #   idx <-
  #     input$temporalCharacterizationCovariateTable_rows_selected
  #   if (is.null(idx)) {
  #     return(NULL)
  #   } else {
  #     return(idx)
  #   }
  # })
  #
  # filteredTemporalCovariateName <- reactiveVal()
  # filteredTemporalCovariateName(c())
  # # collect the user selected covariate names
  # observeEvent(input$temporalCharacterizationCovariateTable_state, {
  #   if (input$temporalCharacterizationCovariateTable_state$columns[[1]]$search$search != "")
  #     filteredTemporalCovariateName(input$temporalCharacterizationCovariateTable_state$columns[[1]]$search$search)
  #   else
  #     filteredTemporalCovariateName(c())
  # })
  #
  # selectedPlotPoints <- reactiveVal()
  # selectedPlotPoints(c())
  # # observe the selection of covariates inside the plot
  # observeEvent(input$compareTemporalCharacterizationPlot_selected, {
  #   selectedPlotPoints(input$compareTemporalCharacterizationPlot_selected)
  # })
  #
  # output$compareTemporalCharacterizationPlot <-
  #   ggiraph::renderggiraph(expr = {
  #     data <- filterByTimeIdAndDomainId()
  #     if (nrow(data) == 0) {
  #       return(dplyr::tibble(Note = "No data for the selected combination."))
  #     }
  #     data <-
  #       compareTemporalCohortCharacteristics(characteristics1 = data,
  #                                            characteristics2 = data)
  #     if (!is.null(selectedtemporalCharacterizationCovariateRow())) {
  #       data <- data[selectedtemporalCharacterizationCovariateRow(), ]
  #     }
  #     else if (!is.null(filteredTemporalCovariateName())) {
  #       data <- data %>%
  #         dplyr::filter(grepl(filteredTemporalCovariateName(), .data$covariateName))
  #     }
  #
  #     if (nrow(data) > 1000) {
  #       data <- data %>%
  #         dplyr::filter(.data$mean1 > 0.01 | .data$mean2 > 0.01)
  #     }
  #     plot <- plotTemporalCohortComparison(
  #       balance = data,
  #       shortNameRef = cohort,
  #       domain = input$temporalDomainId
  #     )
  #     return(plot)
  #   })
  #
  # output$compareTemporalCharacterizationLassoPlot <-
  #   ggiraph::renderggiraph(expr = {
  #     data <- filterByTimeIdAndDomainId()
  #     if (nrow(data) == 0) {
  #       return(dplyr::tibble(Note = "No data for the selected combination."))
  #     }
  #     data <-
  #       compareTemporalCohortCharacteristics(characteristics1 = data,
  #                                            characteristics2 = data)
  #     if (nrow(data) > 1000) {
  #       data <- data %>%
  #         dplyr::filter(.data$mean1 > 0.01 | .data$mean2 > 0.01)
  #     }
  #     data <- data[selectedPlotPoints(), ]
  #     plot <- plotTemporalLassoCohortComparison(
  #       balance = data,
  #       shortNameRef = cohort,
  #       domain = input$temporalDomainId
  #     )
  #     return(plot)
  #   })
  
  
  
  
  
  
  #Cohort Overlap ------------------------
  cohortOverlaDataFiltered <- shiny::reactive(x = {
    validate(need(
      length(input$selectedDatabases) > 0,
      "No data sources chosen"
    ))
    data <- cohortOverlapFromRemote()
    data <- data %>%
      dplyr::filter(.data$databaseId %in% input$selectedDatabases)
    return(data)
  })
  
  output$cohortOverlapPlot <- ggiraph::renderggiraph(expr = {
    data <- cohortOverlaDataFiltered()
    plot <- plotCohortOverlap(
      data = data,
      shortNameRef = cohort,
      yAxis = input$overlapPlotType
    )
    return(plot)
  })
  
  output$cohortOverlapData <-
    DT::renderDT(expr = {
      data <- cohortOverlaDataFiltered()
      # data <- addMetaDataInformationToResults(data = data)
      table <- standardDataTable(data = data)
      return(table)
    }, server = TRUE)
  
  # Compare cohort characteristics --------------------------------------------
  # computeBalance <- shiny::reactive(x = {
  #   validate(need((length(input$selectedCohorts) != 1),
  #                 paste0("Please select atleast two different cohorts.")
  #   ))
  #   validate(need((length(input$selectedDatabases) >= 1),
  #                 paste0("Please select atleast one datasource.")
  #   ))
  #   covs1 <- getCovariateValueResult(
  #     dataSource = dataSource,
  #     cohortIds = input$selectedCohorts
  #   )
  #   balance <- compareCohortCharacteristics(covs1, covs1) %>%
  #     dplyr::mutate(absStdDiff = abs(.data$stdDiff))
  #   return(balance)
  # })
  #
  # output$charCompareTable <- DT::renderDT(expr = {
  #   balance <- computeBalance()
  #   if (nrow(balance) == 0) {
  #     return(dplyr::tibble(Note = "No data for the selected combination."))
  #   }
  #
  #   if (input$charCompareType == "Pretty table") {
  #     table <- prepareTable1Comp(balance)
  #     if (nrow(table) > 0) {
  #       table <- table %>%
  #         dplyr::arrange(.data$sortOrder) %>%
  #         dplyr::select(-.data$sortOrder) %>%
  #         addShortName(cohort,
  #                      cohortIdColumn = "cohortId1",
  #                      shortNameColumn = "shortName1") %>%
  #         addShortName(cohort,
  #                      cohortIdColumn = "cohortId2",
  #                      shortNameColumn = "shortName2") %>%
  #         dplyr::relocate(.data$shortName1, .data$shortName2) %>%
  #         dplyr::select(-.data$cohortId1, -.data$cohortId2)
  #     } else {
  #       return(dplyr::tibble(Note = "No data for covariates that are part of pretty table."))
  #     }
  #     table <- standardDataTable(table)
  #   } else {
  #     table <- balance %>%
  #       dplyr::select(
  #         .data$cohortId1,
  #         .data$cohortId2,
  #         .data$covariateName,
  #         .data$conceptId,
  #         .data$mean1,
  #         .data$sd1,
  #         .data$mean2,
  #         .data$sd2,
  #         .data$stdDiff
  #       ) %>%
  #       addShortName(cohort,
  #                    cohortIdColumn = "cohortId1",
  #                    shortNameColumn = "shortName1") %>%
  #       addShortName(cohort,
  #                    cohortIdColumn = "cohortId2",
  #                    shortNameColumn = "shortName2") %>%
  #       dplyr::relocate(.data$shortName1, .data$shortName2) %>%
  #       dplyr::select(-.data$cohortId1, -.data$cohortId2) %>%
  #       dplyr::arrange(desc(abs(.data$stdDiff)))
  #     table <- standardDataTable(data = table)
  #   }
  #   return(table)
  # }, server = TRUE)
  #
  # output$charComparePlot <- ggiraph::renderggiraph(expr = {
  #   data <- computeBalance()
  #   if (nrow(data) == 0) {
  #     return(dplyr::tibble(Note = "No data for the selected combination."))
  #   }
  #   plot <-
  #     plotCohortComparisonStandardizedDifference(
  #       balance = data,
  #       shortNameRef = cohort,
  #       domain = input$domainId
  #     )
  #   return(plot)
  # })
  
  output$databaseInformationTable <- DT::renderDT(expr = {
    table <- database[, c("databaseId", "databaseName", "description")]
    table <- standardDataTable(table)
    return(table)
  }, server = TRUE)
  
  shiny::observeEvent(input$cohortCountsInfo, {
    showInfoBox(title = "Cohort Counts", htmlFileName = "html/cohortCounts.html")
  })
  
  shiny::observeEvent(input$incidenceRateInfo, {
    showInfoBox(title = "Incidence Rate", htmlFileName = "html/incidenceRate.html")
  })
  
  shiny::observeEvent(input$timeDistributionInfo, {
    showInfoBox(title = "Time Distributions", htmlFileName = "html/timeDistribution.html")
  })
  
  shiny::observeEvent(input$includedConceptsInfo, {
    showInfoBox(title = "Included (Source) Concepts",
                htmlFileName = "html/includedConcepts.html")
  })
  
  shiny::observeEvent(input$orphanConceptsInfo, {
    showInfoBox(title = "Orphan (Source) Concepts", htmlFileName = "html/orphanConcepts.html")
  })
  
  shiny::observeEvent(input$conceptSetDiagnosticsInfo, {
    showInfoBox(title = "Concept Set Diagnostics",
                htmlFileName = "html/conceptSetDiagnostics.html")
  })
  
  shiny::observeEvent(input$inclusionRuleStatsInfo, {
    showInfoBox(title = "Inclusion Rule Statistics",
                htmlFileName = "html/inclusionRuleStats.html")
  })
  
  shiny::observeEvent(input$indexEventBreakdownInfo, {
    showInfoBox(title = "Index Event Breakdown", htmlFileName = "html/indexEventBreakdown.html")
  })
  
  shiny::observeEvent(input$visitContextInfo, {
    showInfoBox(title = "Visit Context",
                htmlFileName = "html/visitContext.html")
  })
  
  shiny::observeEvent(input$cohortCharacterizationInfo, {
    showInfoBox(title = "Cohort Characterization",
                htmlFileName = "html/cohortCharacterization.html")
  })
  
  shiny::observeEvent(input$temporalCharacterizationInfo, {
    showInfoBox(title = "Temporal Characterization",
                htmlFileName = "html/temporalCharacterization.html")
  })
  
  shiny::observeEvent(input$cohortOverlapInfo, {
    showInfoBox(title = "Cohort Overlap", htmlFileName = "html/cohortOverlap.html")
  })
  
  shiny::observeEvent(input$compareCohortCharacterizationInfo, {
    showInfoBox(title = "Compare Cohort Characteristics",
                htmlFileName = "html/compareCohortCharacterization.html")
  })
  
  # Cohort labels --------------------------------------------------------------------------------------------
  # targetCohortCount <- shiny::reactive(x = {
  #   targetCohortWithCount <-
  #     getCohortCountResult(
  #       dataSource = dataSource,
  #       cohortIds = cohortId(),
  #       databaseIds = input$database
  #     ) %>%
  #     dplyr::left_join(y = cohort, by = "cohortId") %>%
  #     dplyr::arrange(.data$cohortName)
  #   return(targetCohortWithCount)
  # })
  
  # targetCohortCountHtml <- shiny::reactive(x = {
  #   targetCohortCount <- targetCohortCount()
  #
  #   return(htmltools::withTags(div(
  #     h5(
  #       "Target: ",
  #       targetCohortCount$cohortName,
  #       " ( n = ",
  #       scales::comma(x = targetCohortCount$cohortSubjects),
  #       " )"
  #     )
  #   )))
  # })
  
  selectedCohorts <- shiny::reactive(x = {
    cohorts <- cohort %>%
      dplyr::filter(.data$cohortId %in% cohortSearchForComparison()$cohortId) %>%
      dplyr::arrange(.data$cohortId) %>%
      dplyr::select(.data$cohortName)
    return(apply(cohorts, 1, function(x)
      tags$tr(lapply(x, tags$td))))
  })
  
  output$cohortCountsSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$indexEventBreakdownSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$characterizationSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$temporalCharacterizationSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$inclusionRuleStatSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$cohortOverlapSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$incidenceRateSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$timeDistSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$visitContextSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$cohortCharCompareSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  #Download
  # download_box <- function(exportname, plot){
  #   downloadHandler(
  #     filename = function() {
  #       paste(exportname, Sys.Date(), ".png", sep = "")
  #     },
  #     content = function(file) {
  #       ggplot2::ggsave(file, plot = plot, device = "png", width = 9, height = 7, dpi = 400)
  #     }
  #   )
  # }
  
})