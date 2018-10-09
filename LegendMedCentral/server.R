library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output, session) {
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  currentChoices <- reactiveValues()

  session$onSessionEnded(function() {
    writeLines("Closing connection")
    DatabaseConnector::disconnect(connection)
  })

  searchResults <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$structured)) {
      targetIds <- exposures$exposureId[exposures$exposureName == query$target]
      comparatorIds <- exposures$exposureId[exposures$exposureName == query$comparator]
      outcomeIds <- outcomes$outcomeId[outcomes$outcomeName == query$outcome]
      if (length(outcomeIds) == 0) {
        # Don't want negative  or positive controls
        outcomeIds <- outcomes$outcomeId
      }
      databaseIds <- databases$databaseId[databases$databaseId == query$database]
      tcoDbs <- getTcoDbs(connection, targetIds = targetIds, comparatorIds = comparatorIds, outcomeIds = outcomeIds, databaseIds = databaseIds, limit = 100)
      return(tcoDbs)
    } else if (!is.null(query$term)) {
      parts <- strsplit(query$term, " ")[[1]]
      outcomeIds <- c()
      exposureIds <- c()
      databaseIds <- c()
      for (part in parts) {
        outcomeDist <- adist(part, outcomes$outcomeName)
        exposureDist <- adist(part, exposures$exposureName)
        databaseDist <- adist(part, databases$databaseId)
        if (min(outcomeDist) < min(exposureDist)) {
          if (min(databaseDist) < min(outcomeDist)) {
            match <- databases$databaseId[databaseDist == min(databaseDist)]
            writeLines(paste("Matched", part, "to database ID", match))
            databaseIds <- c(databaseIds, match)
          } else {
            match <- outcomes$outcomeId[outcomeDist == min(outcomeDist)]
            writeLines(paste("Matched", part, "to outcome", outcomes$outcomeName[outcomes$outcomeId == match]))
            outcomeIds <- c(outcomeIds, match)
          }
        } else {
          if (min(databaseDist) < min(exposureDist)) {
            match <- databases$databaseId[databaseDist == min(databaseDist)]
            writeLines(paste("Matched", part, "to database ID", match))
            databaseIds <- c(databaseIds, match)
          } else {
            match <- exposures$exposureId[exposureDist == min(exposureDist)]
            writeLines(paste("Matched", part, "to exposure", exposures$exposureName[exposures$exposureId == match]))
            exposureIds <- c(exposureIds, match)
          }
        }
      }
      if (length(outcomeIds) == 0) {
        # Don't want negative  or positive controls
        outcomeIds <- outcomes$outcomeId
      }
      tcoDbs <- getTcoDbsStrict(connection, exposureIds = exposureIds, outcomeIds = outcomeIds, databaseIds = databaseIds)
      return(tcoDbs)
    } else {
    return(NULL)
    } 
  })

  selectedTcoDb <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$targetId)) {
      return(NULL)
    } else {
      tcoDb <- getTcoDbs(connection,
                         targetIds = query$targetId,
                         comparatorIds = query$comparatorId,
                         outcomeIds = query$outcomeId,
                         databaseIds = query$databaseId)
      return(tcoDb)
    }
  })

  output$isSearchPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && is.null(query$term) && is.null(query$structured))
  })
  outputOptions(output, "isSearchPage", suspendWhenHidden = FALSE)

  output$isSearchResultPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && (!is.null(query$term) || !is.null(query$structured)))
  })
  outputOptions(output, "isSearchResultPage", suspendWhenHidden = FALSE)

  output$isAbstractPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(!is.null(query$targetId))
  })
  outputOptions(output, "isAbstractPage", suspendWhenHidden = FALSE)
  
  # setExposureGroupChoices <- function(indicationId) {
  #   if (indicationId == "All") {
  #     filterExposureGroups <- unique(exposureGroups$exposureGroup)
  #   } else {
  #     filterExposureGroups <- unique(exposureGroups$exposureGroup[exposureGroups$indicationId == indicationId])
  #   }
  #   if (is.null(currentChoices$exposureGroups) || !isTRUE(all.equal(currentChoices$exposureGroups, filterExposureGroups))) {
  #     currentChoices$exposureGroups <- filterExposureGroups 
  #     writeLines(paste("Setting exposure groups to ", paste(filterExposureGroups, collapse = ", ")))
  #     updateSelectInput(session = session,
  #                       inputId = "exposureGroup",
  #                       choices = c("All", filterExposureGroups))
  #   }
  # }
  
  # setTcoChoices <- function(indicationId, exposureGroup) {
  setTcoChoices <- function(exposureGroup) {
    # if (indicationId == "All") {
      filteredExposures <- exposures
      filteredOutcomes <- outcomes
    # } else {
    #   filteredExposures <- exposures[exposures$indicationId == indicationId, ]
    #   filteredOutcomes <- outcomes[outcomes$indicationId == indicationId, ]
    # }
    if (exposureGroup == "All") {
      filteredExposures <- filteredExposures
    } else {
      filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    }
    
    if (is.null(currentChoices$exposures) || !isTRUE(all.equal(currentChoices$exposures, filteredExposures$exposureName))) {
      # writeLines(paste("Setting target to ", paste(filteredExposures$exposureName, collapse = ", "), ", selection to", input$target))
      currentChoices$exposures <- filteredExposures$exposureName
      updateSelectInput(session = session,
                        inputId = "target",
                        choices = c("All", unique(filteredExposures$exposureName)))
      updateSelectInput(session = session,
                        inputId = "comparator",
                        choices = c("All", unique(filteredExposures$exposureName)))
    }
    if (is.null(currentChoices$outcomes) || currentChoices$outcomes != filteredOutcomes) {
      currentChoices$outcomes <- filteredOutcomes
      updateSelectInput(session = session,
                        inputId = "outcome",
                        choices = c("All", unique(filteredOutcomes$outcomeName)))
    }
  }
  
  # Maintain contents of search box:
  observe({
    query <- parseQueryString(session$clientData$url_search)
    isolate({
      if (!is.null(query$structured)) {
        # print("Parsing query string")
        updateRadioButtons(session = session,
                           inputId = "queryType",
                           selected = "Structured")
        # updateSelectInput(session = session,
        #                   inputId = "indication",
        #                   selected = query$indication)
        # setExposureGroupChoices(query$indication)
        # writeLines(paste("Setting exposure group selection to ", query$exposureGroup))
        updateSelectInput(session = session,
                          inputId = "exposureGroup",
                          selected = query$exposureGroup)
        # setTcoChoices(query$indication, query$exposureGroup)
        setTcoChoices(query$exposureGroup)
        updateSelectInput(session = session,
                          inputId = "target",
                          selected = query$target)
        updateSelectInput(session = session,
                          inputId = "comparator",
                          selected = query$comparator)
        updateSelectInput(session = session,
                          inputId = "outcome",
                          selected = query$outcome)
        updateSelectInput(session = session,
                          inputId = "database",
                          selected = query$database)
        # print("Done parsing query string")
      } else {
        if (!is.null(query$term))
          updateTextInput(session, "query", value = query$term)
      }
    })
  }, priority = 0)
  
  # observe({
  #   indicationId <- input$indication
  #   writeLines(paste("Indication has been set to", indicationId))
  #   setExposureGroupChoices(indicationId)
  # })

  observe({
    # indicationId <- input$indication
    exposureGroup <- input$exposureGroup
    # writeLines(paste("Indication has been set to", indicationId, ", exposure group selection has been set to", exposureGroup))
    # setTcoChoices(indicationId, exposureGroup)
    setTcoChoices(exposureGroup)
  }, priority = 10)

  output$searchResults <- renderDataTable({
    tcoDbs <- searchResults()
    if (is.null(tcoDbs)) {
      return(NULL)
    } else {
      titles <- createTitle(tcoDbs)
      if (input$queryType == "Free-text") {
        titles <- paste0("<a href = '?targetId=",
                         tcoDbs$targetId,
                         "&comparatorId=",
                         tcoDbs$comparatorId,
                         "&outcomeId=",
                         tcoDbs$outcomeId,
                         "&databaseId=",
                         tcoDbs$databaseId,
                         "&term=",
                         URLencode(input$query),
                         "'>",
                         titles,
                         "</a></br><i>LEGEND version 1.0</i>, October 2018</br>")
      } else {
        titles <- paste0("<a href = '?targetId=",
                         tcoDbs$targetId,
                         "&comparatorId=",
                         tcoDbs$comparatorId,
                         "&outcomeId=",
                         tcoDbs$outcomeId,
                         "&databaseId=",
                         tcoDbs$databaseId,
                         "&structured=true&exposureGroup=",
                         URLencode(input$exposureGroup),
                         "&target=",
                         URLencode(input$target),
                         "&comparator=",
                         URLencode(input$comparator),
                         "&outcome=",
                         URLencode(input$outcome),
                         "&database=",
                         URLencode(input$database),
                         "'>",
                         titles,
                         "</a></br><i>LEGEND version 1.0</i>, October 2018</br>")
      }
      options <- list(pageLength = 15,
                      searching = FALSE,
                      lengthChange = TRUE,
                      paging = TRUE,
                      dom = "<\"top\"ip>rt<\"bottom\"flp><\"clear\">")
      data <- data.frame(title = titles)
      colnames(data) <- "Search results"
      table <- datatable(data,
                         options = options,
                         rownames = TRUE,
                         escape = FALSE,
                         class = "compact")
      return(table)
    }
  })

  output$abstract <- renderUI({
    tcoDb <- selectedTcoDb()
    if (is.null(tcoDb)) {
      return(NULL)
    } else {
      
      # targetName <- uncapitalize(exposures$exposureName[match(tcoDb$targetId, exposures$exposureId)])
      # comparatorName <- uncapitalize(exposures$exposureName[match(tcoDb$comparatorId, exposures$exposureId)])
      # outcomeName <- uncapitalize(outcomes$outcomeName[match(tcoDb$outcomeId, outcomes$outcomeId)])
      # indicationId <- uncapitalize(exposures$indicationId[match(tcoDb$targetId, exposures$exposureId)])
      # 
      # results <- getMainResults(connection,
      #                           targetIds = tcoDb$targetId,
      #                           comparatorIds = tcoDb$comparatorId,
      #                           outcomeIds = tcoDb$outcomeId,
      #                           databaseIds = tcoDb$databaseId)
      # 
      # studyPeriod <- getStudyPeriod(connection = connection,
      #                               targetId = tcoDb$targetId,
      #                               comparatorId = tcoDb$comparatorId,
      #                               databaseId = tcoDb$databaseId)      
      
      authors <- createAuthors()
      
      # abstract <- createAbstract(outcomeName, targetName, comparatorName, tcoDb$databaseId, studyPeriod, results)
      abstract <- createAbstract(connection, tcoDb)
      
      title <- createTitle(tcoDb)
      
      abstract <- div(em("LEGEND version 1.0"),
                      h2(title),
                      h3("Authors"),
                      p(authors),
                      h3("Abstract"),
                      p(abstract),
                      p("NB: This is an", strong("automatically"), "generated abstract.")
                      )
      return(abstract)
    }
  })

  output$pdf <- downloadHandler(filename = function() {
    return("Paper.pdf")
  }, content = function(fileName) {
    tcoDb <- selectedTcoDb()
    tcoDb$indicationId <- exposures$indicationId[match(tcoDb$targetId, exposures$exposureId)]
    title <- createTitle(tcoDb)
    abstract <- createAbstract(connection, tcoDb)
    tempFolder <- tempdir()
    file.copy(c("MyArticle.Rmd",
                "DataPulls.R",
                "PlotsAndTables.R",
                "Table1Specs.csv",
                "blank_template.tex", 
                "pnasresearcharticle.sty", 
                "pnas-markdown.cls",
                "jss.bst",
                "ohdsi.bib"), tempFolder)
    tempOutput <- file.path(tempFolder, "output.pdf")
    withProgress(message = "Generating PDF", value = 0, {
      rmarkdown::render(file.path(tempFolder, "MyArticle.Rmd"),
                        output_file = tempOutput,
                        params = list(targetId = tcoDb$targetId,
                                      comparatorId = tcoDb$comparatorId,
                                      outcomeId = tcoDb$outcomeId,
                                      databaseId = tcoDb$databaseId,
                                      indicationId = tcoDb$indicationId,
                                      title = title,
                                      abstract = abstract,
                                      save = NULL,
                                      load = NULL))
    })
    file.copy(tempOutput, fileName)
    # unlink(tempFolder, recursive = TRUE)
  })
})
