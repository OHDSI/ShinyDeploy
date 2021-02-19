shiny::shinyServer(function(input, output, session) {
  
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
  
  showAllMenuItem <- reactiveVal(FALSE)
  
  output$isHeaderbarVisible <- shiny::reactive(x = {
    return(showAllMenuItem())
  })
  
  shiny::outputOptions(x = output,
                       name = "isHeaderbarVisible",
                       suspendWhenHidden = FALSE)
  
  output$menuItems <- shinydashboard::renderMenu({
    menuList <- list(
      shinydashboard::menuItem(text = "Search", tabName = "search")
    )
    
    menuList[[2]] <- 
      if (exists(x = "aboutText"))
        shinydashboard::menuItem(text = "About", tabName = "about")
    
    if (showAllMenuItem()) {
      menuList[[3]] <- 
        if (exists(x = "cohortCount"))
          addInfo(
            item = shinydashboard::menuItem(text = "Cohort Counts", tabName = "cohortCounts"),
            infoId = "cohortCountsInfo"
          )
      
      menuList[[4]] <- 
        if (exists(x = "incidenceRate"))
          addInfo(
            item = shinydashboard::menuItem(text = "Incidence Rate", tabName = "incidenceRate"),
            infoId = "incidenceRateInfo"
          )
      
      menuList[[5]] <- 
        if (exists(x = "timeDistribution"))
          addInfo(
            item = shinydashboard::menuItem(text = "Time Distributions", tabName = "timeDistribution"),
            infoId = "timeDistributionInfo"
          )
      
      menuList[[6]] <-
        if (exists(x = "inclusionRuleStats"))
          addInfo(
            item = shinydashboard::menuItem(text = "Inclusion Rule Statistics", tabName = "inclusionRuleStats"),
            infoId = "inclusionRuleStatsInfo"
          )
      # menuList[[7]] <-
      #   if (exists(x = "indexEventBreakdown"))
      #     addInfo(
      #       item = shinydashboard::menuItem(text = "Index Event Breakdown", tabName = "indexEventBreakdown"),
      #       infoId = "indexEventBreakdownInfo"
      #     )
      menuList[[8]] <- 
        if (exists(x = "visitContext"))
          addInfo(
            item = shinydashboard::menuItem(text = "Visit Context", tabName = "visitContext"),
            infoId = "visitContextInfo"
          )
      
      menuList[[9]] <-
        if (exists(x = "covariateValue"))
          addInfo(
            shinydashboard::menuItem(text = "Cohort Characterization", tabName = "cohortCharacterization"),
            infoId = "cohortCharacterizationInfo"
          )
      menuList[[10]] <- 
        if (exists(x = "temporalCovariateValue"))
          addInfo(
            shinydashboard::menuItem(text = "Temporal Characterization", tabName = "temporalCharacterization"),
            infoId = "temporalCharacterizationInfo"
          )
      
      menuList[[11]] <- 
        if (exists(x = "cohortOverlap"))
          addInfo(
            shinydashboard::menuItem(text = "Cohort Overlap", tabName = "cohortOverlap"),
            infoId = "cohortOverlapInfo"
          )
    }
    
    menuList[[12]] <-
      shinydashboard::menuItem(text = "Database information", tabName = "databaseInformation")
    
    shinydashboard::sidebarMenu(menuList)
  })
  
  ############### search tab ######################################
  rvCohortSearch <- shiny::reactiveValues()
  # Cohort search results
  cohortSearchResults <- shiny::reactive(x = {
    if (input$searchText != "") {
      searchString <- input$searchText
      searchFieldWeight <- dplyr::tibble(
        searchFields = c("cohortName","referentConceptIdsSearchTerms",
                         "json","logicDescription","phenotypeName",
                         "cohortType"),
        searchPoints = c(5,3,2,1,2,1))
      
      searchStringSplit <-
        stringr::str_split(string = tolower(searchString),
                           pattern = " ")[[1]]
      
      searchInField <- function(searchTable = 'cohort',
                                searchString,
                                searchField,
                                points) {
        data <- searchTable %>% 
          dplyr::filter(stringr::str_detect(string = tolower(.data[[searchField]]),
                                            pattern = tolower(searchString))) %>% 
          dplyr::select(.data$cohortId) %>% 
          dplyr::mutate(points = points)
        return(data)
      }
      
      searchResultByWords <- list()
      for (i in (1:length(searchStringSplit))) {
        word <- searchStringSplit[[i]]
        searchResult <- list()
        for (j in (1:nrow(searchFieldWeight))) {
          searchResult[[j]] <- searchInField(searchTable = cohort,
                                             searchField = searchFieldWeight[j,]$searchFields,
                                             searchString = word,
                                             points = searchFieldWeight[j,]$searchPoints) %>% 
            dplyr::mutate(wordSearched = word)
        }
        searchResultByWords[[i]] <- dplyr::bind_rows(searchResult)
      }
      data <- dplyr::bind_rows(searchResultByWords) %>% 
        dplyr::group_by(.data$cohortId) %>%
        dplyr::summarise(points = sum(points))  %>% 
        dplyr::inner_join(y = cohort, by = "cohortId") %>% 
        dplyr::arrange(dplyr::desc(points)) %>% 
        dplyr::select(-.data$points) %>% 
        dplyr::distinct()
      } else {
      data <- cohort
    }
    return(data)
  })
  
  
  
  rvCohortSearch$noSearchResult <- shiny::reactiveVal(value = FALSE, label = "Search result return indicator")
  output$cohortSearchTableResults <- DT::renderDT(expr = {
    data <- cohortSearchResults()
    if (nrow(data) == 0) {
      rvCohortSearch$noSearchResult(TRUE)
      return(dplyr::tibble(Note = "Search did not result any cohort"))
    }
    if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
      data <- data %>%
        dplyr::select(.data$phenotypeId,
                      .data$phenotypeName,
                      .data$cohortId,
                      .data$cohortName)
    } else {
      data <- data %>%
        dplyr::select(.data$cohortId,
                      .data$cohortName)
    }
    
    table <- standardDataTable(data = data,
                               selectionMode = "multiple")
    return(table)
  }, server = TRUE)

  
  # selection of rows
  cohortSearchResultRecentTwoSelection <- shiny::reactive(x = {
    idx <- input$cohortSearchTableResults_rows_selected
    if (length(idx) > 1  && 
        isFALSE(rvCohortSearch$noSearchResult())) {
      # get the last two rows selected
      lastRowsSelected <- idx[c(length(idx), length(idx) - 1)]
      rvCohortSearch$twoRowsSelected <- TRUE
    } else {
      lastRowsSelected <- idx
      rvCohortSearch$twoRowsSelected <- FALSE
    }
    return(cohortSearchResults()[lastRowsSelected,])
  })
  
  #circeR human readable description
  cohortSearchResultRecentTwoSelectionCirceRDetails <- shiny::reactive(x = {
    shiny::withProgress(
      message = "Calling CirceR. Rendering human readable description.",
      expr = {
    data <- cohortSearchResultRecentTwoSelection()
    if (nrow(cohortSearchResultRecentTwoSelection()) > 0) {
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
  })
  
  # count number of rows selected
  cohortSearchResultNumberOfSelectedRows <- shiny::reactive({
    return(length(input$cohortSearchTableResults_rows_selected))
  })
  output$cohortSearchResultsCountOfSelected <- shiny::reactive({
    return(cohortSearchResultNumberOfSelectedRows())
  })
  shiny::outputOptions(x = output,
                name = "cohortSearchResultsCountOfSelected",
                suspendWhenHidden = FALSE)

  # Details of cohort
  cohortDetailsTextReactive <- shiny::reactive(x = {
    data <- cohortSearchResultRecentTwoSelection()
    if (!is.null(data) && nrow(data) > 0) {
      if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
        phenotypeDetails <- phenotypeDescription %>%
          dplyr::filter(.data$phenotypeId %in% data$phenotypeId) %>% 
          dplyr::select(.data$phenotypeId,
                        .data$overview,
                        .data$presentation,
                        .data$assessment,
                        .data$plan,
                        .data$prognosis,
                        .data$phenotypeSynonyms)
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
        data$prognosis <- ""
        data$phenotypeSynonyms <- ""
      }
      if (is.null(data)) {
        return(NULL)
      } else {
        details <- list()
        colNamesData <- colnames(data)
        if (!'logicDescription' %in% colNamesData) {
          data$logicDescription <- "Not given."
        }
        if (!'referentConceptIdsSearchTerms' %in% colNamesData) {
          data$referentConceptIdsSearchTerms <- 0
        }
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
                tags$td(tags$strong("Phenotype Name: ")),
                tags$td(HTML("&nbsp;&nbsp;")),
                tags$td(paste(data[i, ]$phenotypeName, " (",data[i, ]$phenotypeId,")"))
              ),
              tags$tr(
                tags$td(tags$strong("Phenotype Synonyms: ")),
                tags$td(HTML("&nbsp;&nbsp;")),
                tags$td(paste(data[i, ]$phenotypeSynonyms, " (",data[i, ]$phenotypeSynonyms,")"))
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
    if (is.null(cohortSearchResultRecentTwoSelection())) {
      return(NULL)
    } else {
      details <- list()
      for (i in 1:nrow(cohortSearchResultRecentTwoSelection())) {
        details[[i]] <- getConceptSetDetailsFromCohortDefinition(
          cohortDefinitionExpression =
            RJSONIO::fromJSON(cohortSearchResultRecentTwoSelection()[i,]$json)
        )
      }
      return(details)
    }
  })
  
  output$cohortDetailsTextFirst <- shiny::renderUI(expr = {
    return(cohortDetailsTextReactive()[[1]])
  })
  output$cohortDefinitionJsonFirst <- shiny::renderText({
    cohortSearchResultRecentTwoSelection()[1,]$json
  }) 
  output$cohortDefinitionSqlFirst <- shiny::renderText({
    cohortSearchResultRecentTwoSelection()[1,]$sql
  })
  output$cohortDefinitionDetailsFirst <- shiny::renderUI(expr = {
    cohortSearchResultRecentTwoSelectionCirceRDetails()[1, ]$htmlExpressionCohort %>%
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
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortDetailsTextReactive()[[2]])
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionJsonSecond <- shiny::renderText({
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchResultRecentTwoSelection()[2,]$json)
    } else {
      return(NULL)
    }
  }) 
  output$cohortDefinitionSqlSecond <- shiny::renderText({
    if (!is.null(cohortSearchResultRecentTwoSelection()) &&
        length(cohortSearchResultRecentTwoSelection()) == 2 &&
        !is.null(cohortSearchResultRecentTwoSelection()[[2]])) {
      return(cohortSearchResultRecentTwoSelection()[2,]$sql)
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionDetailsSecond <- shiny::renderUI(expr = {
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchResultRecentTwoSelectionCirceRDetails()[2, ]$htmlExpressionCohort %>%
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
 
  # synchronize the selection of tabset panels when comparing two cohorts
  shiny::observe({
    if (input$cohortDetails == "descriptionFirst") {    
      shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "descriptionSecond")
    } else if (input$cohortDetails == "cohortDefinitionFirst")  {
      shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionSecond")
    } else if (input$cohortDetails == "cohortDefinitionConceptsetFirst") {
      shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionConceptsetSecond")
    } else if (input$cohortDetails == "cohortDefinitionJsonFirst") {
      shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionJsonSecond")
    } else if (input$cohortDetails == "cohortDefinitionSqlFirst") {
      shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionSqlSecond")
    }
    
    if (input$conceptsetExpressionTabFirst == "conceptsetExpressionFirst") {
      shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionSecond")
    } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionJsonFirst") {
      shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptetExpressionJsonSecond")
    } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionResoledFirst") {
      shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionResolvedSecond")
    } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionOptimizedFirst") {
      shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionOptimizedSecond")
    } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionRecommendedFirst") {
      shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionRecommendedSecond")
    }
  })
  
  # enable comparison of two cohorts. When one is selected, default to no comparison.
  shiny::observeEvent(eventExpr = cohortSearchResultNumberOfSelectedRows() != 2,
                      handlerExpr = {
                        shinyWidgets::updatePickerInput(session = session,
                                                        inputId = "compareCohorts",
                                                        selected = "No Comparision")
                      })
  
  
  # selected concept set in a cohort definition.
  cohortConceptSetsSelectedFirst <- shiny::reactive(x = {
    if (is.null(input$cohortDefinitionConceptSetsTableFirst_rows_selected)) {
      return(NULL)
    } else {
      idx <- input$cohortDefinitionConceptSetsTableFirst_rows_selected
      if (length(idx) > 0) {
        if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression) &&
            nrow(cohortConceptSets()[[1]]$conceptSetExpression) > 0) {
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
      } else {
        return(NULL)
      }
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
        data <- cohortConceptSets()[[2]]$conceptSetExpressionDetails
        data <- data %>%
          dplyr::filter(.data$id == cohortConceptSetsSelectedSecond()$id)
        dataTable <- standardDataTable(data = data, selectionMode = "single")
        return(dataTable)
      } else {NULL}
    })
  output$cohortConceptsetExpressionJsonSecond <- shiny::renderText({
    cohortConceptSetsSelectedSecond()$json
  })
  
  
  # resolved concept set expression
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
                                                      conceptList = conceptIds)
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
  
  
  # compare the differences between two cohort definitions using diffr
  output$logicDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
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
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
  
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
  
    file1 <- tempfile()
    writeLines(cohort1$json, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$json, con = file2)
    jsonDiffOutput <- diffr::diffr(
      file1 = file1,
      file2 = file2,
      wordWrap = TRUE,
      before = cohort1$cohortName,
      after = cohort2$cohortName
    )
    unlink(file1)
    unlink(file2)
    return(jsonDiffOutput)
  })
  
  output$sqlDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
  
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
  
    file1 <- tempfile()
    writeLines(cohort1$sql, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$sql, con = file2)
    sqlDiffOutput <- diffr::diffr(
      file1 = file1,
      file2 = file2,
      wordWrap = FALSE,
      before = cohort1$cohortName,
      after = cohort2$cohortName,
      width = "100%"
    )
    unlink(file1)
    unlink(file2)
    return(sqlDiffOutput)
  })

  # phenotype description text
  # output$phenotypeDescriptionText <- shiny::renderUI(expr = {
  #   row <- cohortSearchResultRecentTwoSelection()[1,]
  #   if (is.null(row)) {
  #     return(NULL)
  #   } else {
  #     text <-  row$clinicalDescription
  # 
  #     referentConcept <-
  #       getDetailsForConceptIds(dataSource, row$phenotypeId / 1000)
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
  #   row <- cohortSearchResultRecentTwoSelection()[1,]
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
  # })

  # output$phenotypeEvaluationText <- shiny::renderUI(expr = {
  #   row <- cohortSearchResultRecentTwoSelection()[1,]
  #   if (is.null(row)) {
  #     return(NULL)
  #   } else {
  #     files <-
  #       listFilesInGitHub(phenotypeId = row$phenotypeId,
  #                         subFolder = "evaluation")
  #     if (nrow(files) == 0) {
  #       return("Nothing here (yet)")
  #     } else {
  #       return(HTML(paste(files$html, sep = "<br/>")))
  #     }
  #   }
  # })

  # output$phenotypeNotesText <- shiny::renderUI(expr = {
  #   row <- cohortSearchResultRecentTwoSelection()[1,]
  #   if (is.null(row)) {
  #     return(NULL)
  #   } else {
  #     files <-
  #       listFilesInGitHub(phenotypeId = row$phenotypeId,
  #                         subFolder = "notes")
  #     if (nrow(files) == 0) {
  #       return("Nothing here (yet)")
  #     } else {
  #       return(HTML(paste(files$html, sep = "<br/>")))
  #     }
  #   }
  # })
  

  
  ######## After selecting cohort using button ################
  # shiny header drop down options
  headerFilterOptionsPhenotypeDatabaseCohort <- shiny::reactive(x = {
    idx <- input$cohortSearchTableResults_rows_selected
    if (all(length(cohortsSelectedByActionButton()) > 0,
        length(idx) > 0)) {
      selectedCohortIds <-
        cohortSearchResults()[idx,]$cohortId %>% unique()
    } else {
      return(NULL)
    }
    
    data <- combinationsOfPhenotypeDatabaseCohort %>%
      dplyr::filter(.data$cohortId %in%
                      selectedCohortIds) %>%
      dplyr::left_join(
        y = database %>%
          dplyr::select(.data$databaseId,
                        .data$databaseName),
        by = "databaseId"
      ) %>%
      dplyr::left_join(y = cohort %>%
                         dplyr::select(.data$cohortId,
                                       .data$cohortName),
                       by = "cohortId")
    
    if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
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
    return(data)
  },
  label = "drop down options")
  
  optionsForDropDownDatabase <- shiny::reactive(x = {
    if (length(cohortsSelectedByActionButton()) > 0 &&
        !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
      data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$databaseName) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      return(data)
    }
  })
  optionsForDropDownCohort <- shiny::reactive(x = {
    if (length(cohortsSelectedByActionButton()) > 0 &&
        !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
      data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$cohortName) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      return(data)
    }
  })
  if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
    optionsForDropDownPhenotype <- shiny::reactive(x = {
      if (length(cohortsSelectedByActionButton()) > 0 &&
          !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
        data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
          dplyr::select(.data$phenotypeName) %>%
          dplyr::distinct() %>%
          dplyr::pull()
        return(data)
      }
    })
  }
  

  # cohorts selected by action button
  cohortsSelectedByActionButton <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           idx <- input$cohortSearchTableResults_rows_selected
                           if (length(idx) > 0) {
                             return(cohortSearchResults()[idx,]$cohortId %>% unique())
                           } else {
                             return(NULL)
                           }
                         })
  
  # Pre-fetch data --------------------------------------------------------------------------
  progressBarMessagePreFetchCohortCount <- shiny::reactive(x = {
    length(unique(combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()$cohortId))
  })
  progressBarMessagePreFetchDatabaseCount <- shiny::reactive(x = {
    length(unique(combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()$databaseId))
  })
  progressBarMessagePreFetchTemplateFirst <- shiny::reactive(x = {
    paste0("Working on combination of ", 
           progressBarMessagePreFetchCohortCount(), 
           " cohorts on ", 
           progressBarMessagePreFetchDatabaseCount(), 
           " data sources.")
  })
  cohortCountsPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching cohort count data."
                             ),
                             value = 0,
                             {
                               if (length(cohortsSelectedByActionButton()) != 0) {
                                 data <- getCohortCountResult(dataSource = dataSource,
                                                              cohortIds = cohortsSelectedByActionButton())
                                 return(data)
                               } else {
                                 data <- dplyr::tibble()
                               }
                             }
                           )
                         })
  
  incidenceRateDataPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching incidence rate data."
                             ),
                             value = 0,
                             {
                               if (length(cohortsSelectedByActionButton()) != 0) {
                                 data <- getIncidenceRateResult(dataSource = dataSource,
                                                                cohortIds = cohortsSelectedByActionButton())
                                 return(data)
                               } else {
                                 data <- dplyr::tibble()
                               }
                             }
                           )
                         })
  
  timeDistributionPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching time distribution rate data."
                             ),
                             value = 0,
                             {
                               if (length(cohortsSelectedByActionButton()) != 0) {
                                 data <- getTimeDistributionResult(dataSource = dataSource,
                                                                   cohortIds = cohortsSelectedByActionButton())
                                 return(data)
                               } else {
                                 return(dplyr::tibble())
                               }
                             }
                           )
                         })
  
  inclusionRuleTablePreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching inclusion rule table data."
                             ),
                             value = 0,
                             {
                               if (length(cohortsSelectedByActionButton()) != 0) {
                                 data <- getInclusionRuleStats(dataSource = dataSource,
                                                               cohortIds = cohortsSelectedByActionButton())
                                 return(data)
                               } else {
                                 return(dplyr::tibble())
                               }
                             }
                           )
                         })
  
  # indexEventBreakDownDataPreFetch <-
  #   shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
  #                        valueExpr = {
  #                          shiny::withProgress(
  #                            message = paste0(
  #                              progressBarMessagePreFetchTemplateFirst(),
  #                              "\n",
  #                              "Pre-fetching index event breakdown data."
  #                            ),
  #                            value = 0,
  #                            {
  #                              if (length(cohortsSelectedByActionButton()) != 0) {
  #                                data <- getIndexEventBreakdown(
  #                                  dataSource = dataSource,
  #                                  cohortIds = cohortsSelectedByActionButton(),
  #                                  cohortCounts = cohortCountsPreFetch()
  #                                )
  #                                return(data)
  #                              } else {
  #                                return(dplyr::tibble())
  #                              }
  #                            }
  #                          )
  #                        })
  
  visitContextDataPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching visit context data."
                             ),
                             value = 0,
                             {
                               if (length(cohortsSelectedByActionButton()) != 0) {
                                 data <- getVisitContextResults(
                                   dataSource = dataSource,
                                   cohortIds = cohortsSelectedByActionButton(),
                                   cohortCounts = cohortCountsPreFetch()
                                 )
                                 return(data)
                               } else {
                                 return(dplyr::tibble())
                               }
                             }
                           )
                         })
  characterizationDataPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           if (length(cohortsSelectedByActionButton()) != 0) {
                             data <- getCovariateValueResult(dataSource = dataSource,
                                                             table = "covariateValue",
                                                             cohortIds = cohortsSelectedByActionButton())
                             return(data)
                           } else {
                             return(dplyr::tibble())
                           }
                         })
  
  
  temporalCharacterizationDataPreFetch <-
    shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                         valueExpr = {
                           if (length(cohortsSelectedByActionButton()) != 0) {
                             data <- getCovariateValueResult(dataSource = dataSource,
                                                             table = "temporalCovariateValue",
                                                             cohortIds = cohortsSelectedByActionButton())
                             return(data)
                           } else {
                             return(dplyr::tibble())
                           }
                         })
  
  cohortOverlapPreFetch <- shiny::eventReactive(eventExpr = input$loadSelectedCohorts,
                                                valueExpr = {
    if (length(cohortsSelectedByActionButton()) > 1) {
      combisOfTargetComparator <-
        tidyr::crossing(
          targetCohortId = cohortsSelectedByActionButton(),
          comparatorCohortId = cohortsSelectedByActionButton()
        ) %>%
        dplyr::filter(!.data$targetCohortId == .data$comparatorCohortId) %>%
        dplyr::distinct()
      data <- getCohortOverlapResult(
        dataSource = dataSource,
        targetCohortIds = combisOfTargetComparator$targetCohortId,
        comparatorCohortIds = combisOfTargetComparator$comparatorCohortId
      )
      return(data)
    } else {
      return(dplyr::tibble())
    }
  })

  
  # filter combinations to filter 'PreFetch' data.
  combinationToFilterPreFetchDataBasedOnUserChoice <- shiny::reactive(x = {
    if (!is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
    data <- headerFilterOptionsPhenotypeDatabaseCohort() %>% 
      dplyr::filter(.data$cohortId %in% cohortsSelectedByActionButton()) %>% 
      dplyr::distinct() 
    return(data)
    } else {
      return(NULL)
    }
  })
  combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId <- shiny::reactive(x = {
    if (!is.null(combinationToFilterPreFetchDataBasedOnUserChoice())) {
    return(combinationToFilterPreFetchDataBasedOnUserChoice() %>% 
             dplyr::select(.data$cohortId,
                           .data$databaseId) %>% 
             dplyr::distinct())
    } else {
      return(NULL)
    }
  })


  # observe event that gets trigger when select cohorts button is pressed
  shiny::observeEvent(eventExpr = input$loadSelectedCohorts,
                      handlerExpr = {
                        shiny::withProgress(
                          message = paste0(
                            progressBarMessagePreFetchTemplateFirst(),
                            "\n",
                            "Initiating"
                          ),
                          value = 0,
                          {
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "selectedDatabases",
                              label = "Database",
                              choices = optionsForDropDownDatabase(),
                              selected = optionsForDropDownDatabase()[c(1, 2)]
                            )
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "selectedCohorts",
                              label = "Cohort",
                              choices = optionsForDropDownCohort(),
                              selected = optionsForDropDownCohort()[c(1, 2)]
                            )
                            
                            if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "selectedPhenotypes",
                                choicesOpt = list(style = rep_len("color: black;", 999)),
                                choices = optionsForDropDownPhenotype(),
                                selected = optionsForDropDownPhenotype()[c(1, 2)]
                              )
                            }
                            
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "incidenceRateAgeFilter",
                              label = "Age filter",
                              choices = incidenceRateAgeFilter(),
                              selected = incidenceRateAgeFilter()
                            )
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "incidenceRateGenderFilter",
                              label = "Gender",
                              choices = incidenceRateGenderFilter()[stringr::str_detect(string = incidenceRateGenderFilter(), 
                                                                                              pattern = "Male|Female")] %>% sort(),
                              selected = incidenceRateGenderFilter()
                            )
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "incidenceRateCalendarFilter",
                              label = "Calendar Year",
                              choices = incidenceRateCalendarYearFilter()  %>% sort(),
                              selected = incidenceRateCalendarYearFilter()
                            )
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "temporalCharacterizationAnalysisNameFilter",
                              choices = temporalCharacterizationAnalysisNameFilter() %>% sort(),
                              selected = temporalCharacterizationAnalysisNameFilter()
                            )
                            shinyWidgets::updatePickerInput(
                              session = session,
                              inputId = "temporalCharacterizationDomainFilter",
                              choices = temporalCharacterizationDomainFilter() %>% sort(),
                              selected = temporalCharacterizationDomainFilter()
                            )
                          }
                        )
                        showAllMenuItem(TRUE)
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
  

  # filter pre fetch data
  progressBarMessageFilterCohortCount <- shiny::reactive(x = {
    length(input$selectedCohorts)
  })
  progressBarMessageFilterDatabaseCount <- shiny::reactive(x = {
    length(input$selectedDatabases)
  })
  progressBarMessageFilter <- shiny::reactive(x = {
    paste0("Working on combination of ", 
           progressBarMessageFilterCohortCount(), 
           " cohorts on ", 
           progressBarMessageFilterDatabaseCount(), 
           " data sources.")
  })
  
  selectedCohortIds <- shiny::reactive(x = {
    return(cohort %>% 
             dplyr::filter(.data$cohortName %in% input$selectedCohorts) %>% 
             dplyr::select(.data$cohortId) %>% 
             dplyr::pull())
  })
  selectedDatabaseIds <- shiny::reactive(x = {
    return(database %>% 
             dplyr::filter(.data$databaseName %in% input$selectedDatabases) %>% 
             dplyr::select(.data$databaseId) %>% 
             dplyr::pull())
  })
  # cohort count--------------------------------
  cohortCountsDataFiltered <- reactive({
    shiny::withProgress(
      message = paste0(progressBarMessageFilter(),
                       ". ",
                       "Getting cohort counts data"),
      value = 0,
      {
        filter <-
          combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
            .data$cohortId %in% selectedCohortIds(),
            .data$databaseId %in% selectedDatabaseIds()
          )
        data <- cohortCountsPreFetch() %>%
          dplyr::inner_join(y = filter,
                            by = c("cohortId", "databaseId"))
        return(data)
      }
    )
  })
  output$cohortCountsTable <- DT::renderDT(expr = {
    data <- cohortCountsDataFiltered()
    
    if (nrow(data) > 0) {
      isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
      data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
    }
    if (input$pivotCohortCount == 'None') {
      data <- data
    } else if (input$pivotCohortCount == 'Subjects') {
      if ('phenotypeId' %in% colnames(data)) {
        data <- data %>% 
          dplyr::select(-.data$cohortEntries) %>% 
          tidyr::pivot_wider(id_cols = c(.data$phenotypeId, .data$phenotypeName, .data$cohortId, .data$cohortName),
                             values_from = .data$cohortSubjects,
                             names_from = .data$databaseId)
      } else {
        data <- data %>% 
          dplyr::select(-.data$cohortEntries) %>% 
          tidyr::pivot_wider(id_cols = c(.data$cohortId, .data$cohortName),
                             values_from = .data$cohortSubjects,
                             names_from = .data$databaseId)
      }
    } else if (input$pivotCohortCount == 'Entries') {
      if ('phenotypeId' %in% colnames(data)) {
        data <- data %>% 
          dplyr::select(-.data$cohortSubjects) %>% 
          tidyr::pivot_wider(id_cols = c(.data$phenotypeId, .data$phenotypeName, .data$cohortId, .data$cohortName),
                             values_from = .data$cohortEntries,
                             names_from = .data$databaseId)
      } else {
        data <- data %>% 
          dplyr::select(-.data$cohortSubjects) %>% 
          tidyr::pivot_wider(id_cols = c(.data$cohortId, .data$cohortName),
                             values_from = .data$cohortSubjects,
                             names_from = .data$databaseId)
      }
    }
    dataTable <- standardDataTable(data = data)
    return(dataTable)
  }, server = TRUE)
  
  # Incidence rate --------------------------------------------------------------------------------
  incidenceRateAgeFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      ageFilter <- incidenceRateDataPreFetch() %>%
        dplyr::select(.data$ageGroup) %>%
        dplyr::distinct() %>%
        dplyr::arrange(as.integer(sub(
          pattern = '-.+$', '', x = .data$ageGroup
        ))) %>% 
        dplyr::pull()
      
      ageFilter <-
        ageFilter[!ageFilter == 'All']
    } else {
      ageFilter <- NULL
    }
    return(ageFilter)
  })
  incidenceRateGenderFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      genderFilter <-
        incidenceRateDataPreFetch()$gender %>% unique()
      genderFilter <-
        genderFilter[!genderFilter == 'All']
    } else {
      genderFilter <- NULL
    }
    return(genderFilter)
  })
  incidenceRateCalendarYearFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      calendarYear <-
        incidenceRateDataPreFetch()$calendarYear %>% unique()
      calendarYear <-
        calendarYear[!calendarYear == 'All']
    } else {
      calendarYear <- NULL
    }
    return(calendarYear)
  })
  
  incidenceRateDataFiltered <- reactive({
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        "\n",
        "Getting incidence rate data"
      ),
      value = 0,
      {
        stratifyByAge <- "Age" %in% input$irStratification
        stratifyByGender <- "Gender" %in% input$irStratification
        stratifyByCalendarYear <-
          "Calendar Year" %in% input$irStratification
        filter <-
          combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
            .data$cohortId %in% selectedCohortIds(),
            .data$databaseId %in% selectedDatabaseIds()
          )
        data <- incidenceRateDataPreFetch() %>%
          dplyr::inner_join(y = filter,
                            by = c("cohortId", "databaseId"))
        if (stratifyByAge) {
          data <- data %>%
            dplyr::filter(.data$ageGroup %in% input$incidenceRateAgeFilter)
        } else {
          data <- data %>% 
            dplyr::filter(.data$ageGroup %in% 'All')
        }
        if (stratifyByCalendarYear) {
          data <- data %>%
            dplyr::filter(.data$calendarYear %in% input$incidenceRateCalendarFilter)
        } else {
          data <- data %>% 
            dplyr::filter(.data$calendarYear %in% 'All')
        }
        if (stratifyByGender) {
          data <- data %>%
            dplyr::filter(.data$gender %in% input$incidenceRateGenderFilter)
        } else {
          data <- data %>% 
            dplyr::filter(.data$gender %in% 'All')
        }
        return(data)
      }
    )
  })
  
  output$incidenceRatePlot <- ggiraph::renderggiraph(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                        "\n",
                        "Generating Incidence Rate plot(S)"), 
                        value = 0, {
      data <- incidenceRateDataFiltered()
      if (nrow(data) > 0) {
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
      } else {
        NULL
      }
    })
  })
  
  output$incidenceRateTable <- DT::renderDT(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                        "\n",
                        "Creating incidence rate data table."), 
                        value = 0, {
      data <- incidenceRateDataFiltered()
      if (nrow(data) > 0) {
        isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
        data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        colnames(data) <-
          colnames(data) %>% stringr::str_replace_all(string = .,
                                                      pattern = "Value",
                                                      replacement = "")
      }
      table <- standardDataTable(data)
      return(table)
    })
  }, server = TRUE)
  
  # Time distribution -----------------------------------------------------------------------------
  timeDistributionFiltered <- reactive({
    data <- timeDistributionPreFetch()
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
      .data$cohortId %in% selectedCohortIds(),
      .data$databaseId %in% selectedDatabaseIds()
    )
    if (nrow(data) > 0) {
      data <- timeDistributionPreFetch() %>%
        dplyr::inner_join(y = filter,
                          by = c("cohortId", "databaseId"))
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$timeDistributionPlot <- ggiraph::renderggiraph(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                        "\n",
                        "Generating Time Distribution plot(S)"), 
                        value = 0, {
      data <- timeDistributionFiltered()
      validate(need(nrow(data) > 0, paste0("No data for this combination")))
      plot <- plotTimeDistribution(data = data,
                                   shortNameRef = cohort)
    return(plot)
    })
  })
  output$timeDistributionTable <- DT::renderDT(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                        "\n",
                        "Generating Time Distribution Data Table."), 
                        value = 0, {
      data <- timeDistributionFiltered()
      if (nrow(data) > 0) {
        isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
        data <- data %>% dplyr::relocate(.data$timeMetric)
        data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        colnames(data) <-
          colnames(data) %>% stringr::str_replace_all(string = .,
                                                      pattern = "Value",
                                                      replacement = "")
      }
      table <- standardDataTable(data)
    return(table)
    })
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
    if (nrow(inclusionRuleTablePreFetch()) > 0) {
      filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
        .data$cohortId %in% selectedCohortIds(),
        .data$databaseId %in% selectedDatabaseIds()
      )
      data <- inclusionRuleTablePreFetch() %>%
        dplyr::inner_join(y = filter,
                          by = c("cohortId", "databaseId"))
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$inclusionRuleTable <- DT::renderDT(expr = {
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        ". ",
        "Creating inclusion rule table."
      ),
      value = 0,
      {
        if (nrow(inclusionRuleFiltered()) > 0) {
          isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
          data <- inclusionRuleFiltered()
          data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
          table <- standardDataTable(data)
          return(table)
        } else {
          return(dplyr::tibble("No Inclusion rules data for the selected combination."))
        }
      }
    )
  }, server = TRUE)
  
  
  # Index event breakdown ----------------------------------------------------------------
  # indexEventBreakDownDataFiltered <- reactive({
  #   if (nrow(indexEventBreakDownDataPreFetch()) > 0) {
  #     filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
  #       .data$cohortId %in% selectedCohortIds(),
  #       .data$databaseId %in% selectedDatabaseIds()
  #     )
  #     data <- indexEventBreakDownDataPreFetch() %>%
  #       dplyr::inner_join(y = filter,
  #                         by = c("cohortId", "databaseId"))
  #   } else {
  #     data <- dplyr::tibble()
  #   }
  #   return(data)
  # })
  # output$indexEventBreakDownTable <- DT::renderDT(expr = {
  #   shiny::withProgress(
  #     message = paste0(
  #       progressBarMessageFilter(),
  #       "\n",
  #       "Generating index event breakdown table"
  #     ),
  #     value = 0,
  #     {
  #     isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
  #     data <-
  #       addMetaDataInformationToResults(data = indexEventBreakDownDataFiltered(), isPhenotypeLibraryMode = isPhenotypeLibraryMode) %>%
  #       dplyr::arrange(dplyr::desc(.data$percent))
  #     dataTable <- standardDataTable(data)
  #   return(dataTable)
  #   })
  # }, server = TRUE)
  # 
  
  # Visit Context --------------------------------------------------------------------------------------------
  visitContextDataFiltered <- reactive({
    if (nrow(visitContextDataPreFetch()) > 0) {
      shiny::withProgress(
        message = paste0(
          progressBarMessageFilter(),
          ". ",
          "Getting visit context data. "
        ),
        value = 0,
        {
          filter <-
            combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
              .data$cohortId %in% selectedCohortIds(),
              .data$databaseId %in% selectedDatabaseIds()
            )
          data <- visitContextDataPreFetch() %>%
            dplyr::inner_join(y = filter,
                              by = c("cohortId", "databaseId"))
          
          if (input$pivotVisitContext == 'None') {
            data <- data
          } else if (input$pivotVisitContext == 'Percent') {
            data <- data %>% 
              dplyr::select(-dplyr::contains("subjects"))
          } else if (input$pivotVisitContext == 'Subjects') {
            data <- data %>% 
              dplyr::select(-dplyr::contains("percent"))
          }
        }
      )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$visitContextTable <- DT::renderDT(expr = {
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        "\n",
        "Generating visit context table"
      ),
      value = 0,
      {
        isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
      data <-
        addMetaDataInformationToResults(data = visitContextDataFiltered(), isPhenotypeLibraryMode = isPhenotypeLibraryMode)
      table <- standardDataTable(data)
    })
  }, server = TRUE)
  
  
  # Characterization -----------------------------------------------------------------
  # characterizationDataFiltered <- shiny::reactive(x = {
  #   data <- characterizationDataPreFetch() %>%
  #     dplyr::inner_join(y = combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId(),
  #                       by = c("cohortId", "databaseId"))
  #   return(data)
  #   })
  
  # # Characterization --------------------------------------------------
  # characterizationTableRaw <- shiny::reactive(x = {
  #   data <- characterizationDataFiltered()
  #   data <- addMetaDataInformationToResults(data)
  #   return(data)
  # })
  # 

  # 
  # output$characterizationTablePretty <- DT::renderDT(expr = {
  #   shiny::withProgress(message = 'Loading, Please wait. .', value = 0, {
  #     data <- characterizationTablePretty()
  #     table <- standardDataTable(data)
  #     return(table)
  #   })
  # })
  # output$characterizationTableRaw <- DT::renderDT(expr = {
  #   shiny::withProgress(message = 'Loading, Please wait. .', value = 0, {
  #     data <- characterizationTableRaw()
  #     table <- standardDataTable(data)
  #     return(table)
  #   })
  # })
  # 
  # covariateIdArray <- reactiveVal()
  # covariateIdArray(c())
  # observeEvent(input$rows, {
  #   if (input$rows[[2]] %in% covariateIdArray())
  #     covariateIdArray(covariateIdArray()[covariateIdArray() %in% input$rows[[2]] == FALSE])
  #   else
  #     covariateIdArray(c(covariateIdArray(), input$rows[[2]]))
  # })
  
  
  # Characterization -----------------------------------------------------------------
  characterizationDataFilterOptions <-
    shiny::reactive({
      data <- characterizationDataPreFetch()
      data <- data %>%
        dplyr::select(.data$covariateId) %>%
        dplyr::distinct() %>%
        dplyr::left_join(covariateRef, by = "covariateId") %>%
        dplyr::left_join(
          analysisRef %>%
            dplyr::select(.data$analysisId,
                          .data$analysisName,
                          .data$domainId,
                          .data$isBinary),
          by = "analysisId"
        )
      return(data)
    })
  characterizationAnalysisNameFilter <- shiny::reactive(x = {
    if (nrow(characterizationDataFilterOptions()) > 0) {
      characterizationAnalysisNameFilter <-
        characterizationDataFilterOptions()$analysisName %>% unique()
      return(characterizationAnalysisNameFilter)
    } else {
      return(NULL)
    }
  })
  characterizationDomainFilter <- shiny::reactive(x = {
    if (nrow(characterizationDataFilterOptions()) > 0) {
      characterizationDomainFilter <-
        characterizationDataFilterOptions()$domainId %>% unique()
      return(characterizationDomainFilter)
    } else {
      return(NULL)
    }
  })
  
  characterizationDataFiltered <- shiny::reactive(x = {
    dataFilterOptions <-
      characterizationDataFilterOptions() %>%
      dplyr::filter(
        analysisName %in% input$characterizationAnalysisNameFilter,
        domainId %in% input$characterizationDomainFilter
      )
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
      .data$cohortId %in% selectedCohortIds(),
      .data$databaseId %in% selectedDatabaseIds()
    )
    data <- characterizationDataPreFetch() %>%
      dplyr::inner_join(y = filter,
                        by = c("cohortId", "databaseId")) %>%
      dplyr::inner_join(y = dataFilterOptions,
                        by = c("covariateId" = "covariateId")) %>%
      dplyr::relocate(.data$databaseId,
                      .data$analysisId,
                      .data$analysisName,
                      .data$domainId,
                      .data$covariateId,
                      .data$covariateName,
                      .data$conceptId,
                      .data$conceptName,
                      .data$isBinary) %>% 
      dplyr::arrange(dplyr::desc(.data$mean)) %>% 
      dplyr::distinct()
    return(data)
  })
  
  characterizationTablePretty <- shiny::reactive(x = {
    data <- characterizationDataFiltered()
    if (nrow(data) > 0) {
      isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
      data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
      analysisIds <- prettyAnalysisIds
      table <- data %>%
        prepareTable1() %>%
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
          dplyr::tibble(cohortId = unique(data$cohortId)),
          dplyr::tibble(databaseId = unique(data$databaseId))
        ),
        characteristics %>%
          dplyr::filter(.data$header == 0) %>%
          tidyr::crossing(dplyr::tibble(databaseId = unique(data$databaseId)) %>%
                            tidyr::crossing(
                              dplyr::tibble(cohortId = unique(data$cohortId))
                            )
          ))
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
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  
    output$characterizationTablePrettyDt <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering characterization data table.', value = 0, {
        data <- characterizationTablePretty()
        if (nrow(data) > 0) {
          isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
          data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
          table <- standardDataTable(data = data)
          return(table)
        }
      })
    }, server = TRUE)
  
  
  output$characterizationTableRaw <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering characterization data table.', value = 0, {
        data <- characterizationDataFiltered()
        if (nrow(data) > 0) {
          isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
          data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        }
        table <- standardDataTable(data = data)
        return(table)
      })
    }, server = TRUE)
  
  shiny::observeEvent(eventExpr = {
    (!is.null(input$tabs) && input$tabs == "cohortCharacterization")
  },
  handlerExpr = {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationAnalysisNameFilter",
      choices = characterizationAnalysisNameFilter() %>% sort(),
      selected = characterizationAnalysisNameFilter()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationDomainFilter",
      choices = characterizationDomainFilter() %>% sort(),
      selected = characterizationDomainFilter()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationTablePrettyDtDropDownDatabase",
      choices = characterizationPrettyDatabaseFilter(),
      selected = characterizationPrettyDatabaseFilter()[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationTablePrettyDtDropDownCohort",
      choices = characterizationPrettyCohortFilter(),
      selected = characterizationPrettyDatabaseFilter()[1]
    )
  })
  
  characterizationPrettyDatabaseFilter <- shiny::reactive(x = {
    if (nrow(characterizationTablePretty()) > 0) {
      characterizationPrettyDatabaseFilter <-
        characterizationTablePretty()$databaseId %>% 
        unique() %>% 
        sort()
      return(characterizationPrettyDatabaseFilter)
    } else {
      return(NULL)
    }
  })
  characterizationPrettyCohortFilter <- shiny::reactive(x = {
    if (nrow(characterizationTablePretty()) > 0) {
      characterizationPrettyCohortFilter <-
        characterizationTablePretty()$cohortName %>% 
        unique() %>% 
        sort()
      return(characterizationPrettyCohortFilter)
    } else {
      return(NULL)
    }
  })

  
  # output$characterizationTable <-
  #   DT::renderDT(expr = {
  #     shiny::withProgress(message = 'Rendering characterization data table.', value = 0, {
  #       data <- characterizationDataFiltered()
  #       if (nrow(data) > 0) {
  #         isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
  #         data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
  #       }
  #       data <- data %>% 
  #         dplyr::select(-.data$sd) %>% 
  #         dplyr::rename('percent' = .data$mean) %>% 
  #         dplyr::select(-.data$conceptId) %>% 
  #         dplyr::distinct()
  #       # colnamesData <- colnames(data)
  #       # colnamesData <- colnamesData[stringr::str_detect(string = colnamesData,
  #       #                                                  pattern = "mean|sd|percent", 
  #       #                                                  negate = TRUE)]s
  #       # 
  #       # data <- tidyr::pivot_wider(data = data %>% dplyr::distinct(),
  #       #                            id_cols = colnamesData,
  #       #                            names_from = .data$temporalChoices,
  #       #                            values_from = .data$percent
  #       # )
  #       table <- standardDataTable(data = data)
  #       return(table)
  #     })
  #   }, server = TRUE)
  
  
  
  # Temporal characterization -----------------------------------------------------------------
  temporalCharacterizationDataFilterOptions <-
    shiny::reactive({
      data <- temporalCharacterizationDataPreFetch() %>%
        dplyr::select(.data$timeId,
                      .data$covariateId) %>%
        dplyr::distinct() %>%
        dplyr::left_join(temporalTimeRef, by = "timeId") %>%
        dplyr::left_join(temporalCovariateRef, by = "covariateId") %>%
        dplyr::left_join(
          temporalAnalysisRef %>%
            dplyr::select(.data$analysisId,
                          .data$analysisName,
                          .data$domainId,
                          .data$isBinary),
          by = "analysisId"
        )
      return(data)
    })
  temporalCharacterizationAnalysisNameFilter <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
      temporalCharacterizationAnalysisNameFilter <-
        temporalCharacterizationDataFilterOptions()$analysisName %>% unique()
      return(temporalCharacterizationAnalysisNameFilter)
    } else {
      return(NULL)
    }
  })
  temporalCharacterizationDomainFilter <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
      temporalCharacterizationDomainFilter <-
        temporalCharacterizationDataFilterOptions()$domainId %>% unique()
      return(temporalCharacterizationDomainFilter)
    } else {
      return(NULL)
    }
  })
  
  temporalCharacterizationDataFiltered <- shiny::reactive(x = {
    dataFilterOptions <-
      temporalCharacterizationDataFilterOptions() %>%
      dplyr::filter(
        # temporalChoices %in% input$timeIdChoices,
        analysisName %in% input$temporalCharacterizationAnalysisNameFilter,
        domainId %in% input$temporalCharacterizationDomainFilter
      )
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
      .data$cohortId %in% selectedCohortIds(),
      .data$databaseId %in% selectedDatabaseIds()
    )
    data <- temporalCharacterizationDataPreFetch() %>%
      dplyr::inner_join(y = filter,
                        by = c("cohortId", "databaseId")) %>%
      dplyr::inner_join(y = dataFilterOptions,
                        by = c("timeId" = "timeId",
                               "covariateId" = "covariateId")) %>%
      dplyr::relocate(.data$databaseId,
                      .data$temporalChoices,
                      .data$startDay,
                      .data$endDay,
                      .data$analysisId,
                      .data$analysisName,
                      .data$domainId,
                      .data$covariateId,
                      .data$covariateName,
                      .data$conceptId,
                      .data$conceptName,
                      .data$isBinary) %>% 
      dplyr::arrange(dplyr::desc(.data$mean)) %>% 
      dplyr::distinct()
    return(data)
  })

  
  output$temporalCharacterizationTableRaw <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering temporal characterization data table.', value = 0, {
        data <- temporalCharacterizationDataFiltered()
        if (nrow(data) > 0) {
          isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
          data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        }
        table <- standardDataTable(data = data)
        return(table)
      })
    }, server = TRUE)
  
  
  output$temporalCharacterizationTable <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering temporal characterization data table.', value = 0, {
        data <- temporalCharacterizationDataFiltered()
        if (nrow(data) > 0) {
          isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
          data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        }
        data <- data %>% 
          dplyr::select(-.data$sd) %>% 
          dplyr::rename('percent' = .data$mean) %>% 
          dplyr::select(-.data$timeId) %>% 
          dplyr::distinct()
        colnamesData <- colnames(data)
        colnamesData <- colnamesData[stringr::str_detect(string = colnamesData,
                                                          pattern = "mean|sd|percent|temporalChoices|Day", 
                                                          negate = TRUE)]
        data <- data %>% 
          dplyr::group_by(dplyr::across(.cols = colnamesData)) %>% 
          dplyr::arrange(.data$startDay, .data$endDay, by_group = TRUE) %>% 
          dplyr::ungroup()
        
        data <- tidyr::pivot_wider(data = data %>% dplyr::distinct(),
                                   id_cols = colnamesData,
                                   names_from = .data$temporalChoices,
                                   values_from = .data$percent
                                   )
        table <- standardDataTable(data = data)
        return(table)
      })
    }, server = TRUE)

  
  output$temporalCharacterizationPlotText <- shiny::renderText(expr = {
    text <- "This compares two different cohort definitions, computed on the same data source. Under development."
    return(text)
  })
  
  shiny::reactive(x = {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "temporalCharacterizationPlotCohorts",
      choices = temporalCharacterizationDataFilteredPlotCompareCohorts(),
      selected = temporalCharacterizationDataFilteredPlotCompareCohorts()[c(1,2)]
    )
  })
  
  temporalCharacterizationDataFilteredForPlotting <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFiltered()) > 0) {
      data <- temporalCharacterizationDataFiltered %>% 
        dplyr::filter(.data$cohortId %in% input$temporalCharacterizationPlotCohorts)
      return(data)
    } else {
      return(NULL)
    }
  })
  
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
    if (nrow(cohortOverlapPreFetch()) > 0) {
      filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% dplyr::filter(
        .data$cohortId %in% selectedCohortIds(),
        .data$databaseId %in% selectedDatabaseIds()
      )
      data <- cohortOverlapPreFetch() %>%
        dplyr::inner_join(y = filter,
                          by = c("databaseId" = "databaseId",
                                 "targetCohortId" = "cohortId")) %>%
        dplyr::inner_join(y =filter,
                          by = c("databaseId" = "databaseId",
                                 "comparatorCohortId" = "cohortId"))
      return(data)
    } else {
      return(NULL)
    }
  })
  
  output$cohortOverlapPlot <- ggiraph::renderggiraph(expr = {
    data <- cohortOverlaDataFiltered()
    validate(need(nrow(data) > 0, paste0("No data for this combination")))
    if (!is.null(cohortOverlaDataFiltered())) {
      shiny::withProgress(
        message = paste0(
          progressBarMessageFilter(),
          "\n",
          "Generating Cohort Overlap plot(S)"
        ),
        value = 0,
        {
          plot <- plotCohortOverlap(
            data = data,
            shortNameRef = cohort,
            yAxis = input$overlapPlotType
          )
          return(plot)
        }
      )
    } else {
      return(NULL)
    }
  })
  
  output$cohortOverlapData <-
    DT::renderDT(expr = {
      shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                           ". ",
                                           "Generating cohort overlap table."), 
                          value = 0, {
      data <- cohortOverlaDataFiltered()
      # data <- addMetaDataInformationToResults(data = data)
      table <- standardDataTable(data = data)
      return(table)
    })}, server = TRUE)
  
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
  
  # shiny::observeEvent(input$compareCohortCharacterizationInfo, {
  #   showInfoBox(title = "Compare Cohort Characteristics",
  #               htmlFileName = "html/compareCohortCharacterization.html")
  # })
  
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