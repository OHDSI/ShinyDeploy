library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output, session) {
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  session$onSessionEnded(function() {
    writeLines("Closing connection")
    DatabaseConnector::disconnect(connection)
  })

  searchResults <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$term)) {
      return(NULL)
    } else {
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
            writeLines(paste("Matched", part, "to outcome ID", match))
            outcomeIds <- c(outcomeIds, match)
          }
        } else {
          if (min(databaseDist) < min(exposureDist)) {
            match <- databases$databaseId[databaseDist == min(databaseDist)]
            writeLines(paste("Matched", part, "to database ID", match))
            databaseIds <- c(databaseIds, match)
          } else {
            match <- exposures$exposureId[exposureDist == min(exposureDist)]
            writeLines(paste("Matched", part, "to exposure ID", match))
            exposureIds <- c(exposureIds, match)
          }
        }
      }
      tcoDbs <- getTcoDbsStrict(connection, exposureIds = exposureIds, outcomeIds = outcomeIds, databaseIds = databaseIds)
      return(tcoDbs)
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

  # Maintain contents of search box:
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$term))
      updateTextInput(session, "query", value = query$term)
  })

  output$isSearchPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && is.null(query$term))
  })
  outputOptions(output, "isSearchPage", suspendWhenHidden = FALSE)

  output$isSearchResultPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && !is.null(query$term))
  })
  outputOptions(output, "isSearchResultPage", suspendWhenHidden = FALSE)

  output$isAbstractPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(!is.null(query$targetId))
  })
  outputOptions(output, "isAbstractPage", suspendWhenHidden = FALSE)


  output$searchResults <- renderDataTable({
    tcoDbs <- searchResults()
    if (is.null(tcoDbs)) {
      return(NULL)
    } else {
      titles <- createTitle(tcoDbs)
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
