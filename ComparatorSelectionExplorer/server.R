# packages
library(shiny)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)

dataModelSpec <- ResultModelManager::loadResultsDataModelSpecifications("resultsDataModel.csv")
qns <- ResultModelManager::createQueryNamespace(connectionDetails = connectionDetails,
                                                usePooledConnection = FALSE,
                                                schema = resultsSchema,
                                                tablePrefix = tablePrefix,
                                                tableSpecification = dataModelSpec)

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output, session) {

  # When user triggers on click event from main reactable
  selectedDatabase <- shiny::reactive({
    input$show_details$index[2]
  })

  # Checkbox input panel on main view
  selectedDatabases <- shiny::reactive({
    input$selectedDatabases
  })

  cohortTable <- shiny::reactive({
    getCohortDefinitions(qns)
  })

  selectedComparator <- shiny::reactive({
    input$show_details$index[1]
  })

  selectedExposure <- shiny::reactive(input$selectedExposure)

  # initial query to get list of cohort definitions
  getCohortDefinitionsWithDbCounts <- shiny::reactive({
    dbSel <- selectedDatabase()
    if (is.null(dbSel) || dbSel == "")
      return(data.frame())

    getCohortDefinitionsTable(qns, databaseId = dbSel)
  })

  databaseSources <- shiny::reactive({
    getDatabaseSources(qns)
  })

  shiny::observe({
    shiny::withProgress({
      dbSources <- databaseSources()
      dbChoices <- dbSources$databaseId
      names(dbChoices) <- dbSources$cdmSourceAbbreviation
      dbChoices <- dbChoices[order(names(dbChoices))]

      updateSelectizeInput(
        session,
        "selectedDatabase",
        choices = dbChoices,
        # default choice: CCAE
        selected = dbChoices[which(names(dbChoices) == "IBM CCAE")],
        server = TRUE)
    }, message = "Loading database sources")
  })

  shiny::observe({
    shiny::withProgress({

      dbSources <- databaseSources()
      dbChoices <- dbSources$databaseId
      names(dbChoices) <- dbSources$cdmSourceAbbreviation
      dbChoices <- dbChoices[order(names(dbChoices))]

      updateSelectInput(
        session,
        "selectedDatabases",
        choices = dbChoices,
        selected = dbChoices
      )
    }, message = "Loading database sources")
  })

  shiny::observe({
    shiny::withProgress({
      dbSources <- databaseSources()
      dbChoices <- dbSources$databaseId

      updateSliderInput(
        session,
        inputId = "minNumDatabases",
        min = 1,
        max = length(dbChoices),
        step = 1)
    }, message = "Loading database sources")
  })


  shiny::observe({
    shiny::withProgress({
      cohortDefinitions <- cohortTable()
      if (nrow(cohortDefinitions)) {

        exposureSelection <- cohortDefinitions$cohortDefinitionId
        names(exposureSelection) <- cohortDefinitions$shortName
        updateSelectizeInput(
          session,
          "selectedExposure",
          choices = exposureSelection,
          selected = 8826,
          server = TRUE)
      }
    }, message = "Loading cohort definitions")
  })

  shiny::observe({
    shiny::withProgress({


      if (length(input$selectedComparatorTypes) == 0) {
        atcSelection <- c(0, 1)
      } else if (input$selectedComparatorTypes == "RxNorm Ingredients") {
        atcSelection <- c(0)
      } else if (input$selectedComparatorTypes == "ATC Classes") {
        atcSelection <- c(1)
      }

      cohortDefinitions <- cohortTable() %>% dplyr::filter(isAtc %in% atcSelection)

      if (nrow(cohortDefinitions)) {
        exposureSelection <- cohortDefinitions$cohortDefinitionId
        names(exposureSelection) <- cohortDefinitions$shortName
        updateSelectizeInput(
          session,
          "selectedExposure",
          choices = exposureSelection,
          server = TRUE)
      }
    }, message = "Loading cohort definitions")
  })


  #### ---- function to get cosine similarity data for single database-target pair ---- ####
  getDbSimilarity <- shiny::reactive({
    # identify target cohort
    targetCohortId <- input$selectedExposure
    validate(need(input$selectedExposure, "must select exposure"))

    shiny::withProgress({
      # identify selected comparator types
      if (length(input$selectedComparatorTypes) == 2L) { atcSelection <- c(0, 1) }
      else if (input$selectedComparatorTypes == "RxNorm Ingredients") { atcSelection <- c(0) }
      else if (input$selectedComparatorTypes == "ATC Classes") { atcSelection <- c(1) }
      # send query to get results data
      resultsData <- getDatabaseSimilarityScores(qns,
                                                 targetCohortId = targetCohortId,
                                                 databaseIds = input$selectedDatabases)
    }, message = "Loading similarity scores", value = 0.5)

    resultsData %>%
      dplyr::filter(.data$isAtc2 %in% atcSelection)

  })

  #### ---- function to get cosine similarity data for target in all databases ---- ####
  getSimilarityAllDatabases <- shiny::reactive({
    # identify target cohort
    targetCohortId <- input$selectedExposure
    validate(need(input$selectedExposure, "must select exposure"))

    shiny::withProgress({
      # identify selected comparator types
      if (length(input$selectedComparatorTypes) == 0) { atcSelection <- c(0, 1) }
      else if (input$selectedComparatorTypes == "RxNorm Ingredients") { atcSelection <- c(0) }
      else if (input$selectedComparatorTypes == "ATC Classes") { atcSelection <- c(1) }

      # send query to get results data
      resultsData <- getCohortSimilarityScores(qns, targetCohortId)
    }, message = "Loading similarity scores", value = 0.5)

    resultsData %>%
    dplyr::filter(.data$isAtc2 %in% atcSelection)

  })

  # displays target name and counts
  selectedCohortInfo <- function() {

    targetId <- input$selectedExposure
    if (targetId == "")
      return("")

    dbName <- databaseSources() %>%
      filter(databaseId == selectedDatabase()) %>%
      select(cdmSourceAbbreviation) %>%
      pull()

    cohortDefinitions <- getCohortDefinitionsWithDbCounts()

    target <- cohortDefinitions[cohortDefinitions$cohortDefinitionId == selectedExposure(),]
    comparator <- cohortDefinitions[cohortDefinitions$cohortDefinitionId == selectedComparator(),]

    numPersons <- format(round(target$numPersons), big.mark = ",")
    targetText <- paste0(target$shortName, " (", numPersons, " persons)")

    numPersons <- format(round(comparator$numPersons), big.mark = ",")
    comparatorText <- paste0(comparator$shortName, " (", numPersons, " persons)")


    return(shiny::fluidRow(
      shiny::column(
        width = 4,
        tags$b(paste("Database:", dbName))
      ),
      shiny::column(
        width = 4,
        tags$b(paste("Target:", targetText)),
      ),
      shiny::column(
        width = 4,
        tags$b(paste("Comparator:", comparatorText))
      )
    ))
  }

  #### ---- single-database cosine similarity reactable ---- ####
  output$cosineSimilarityTbl <- reactable::renderReactable({
    getDbCosineSimilarityTable(qns,
                               databaseId = selectedDatabase(),
                               targetCohortId = input$selectedExposure,
                               comparatorCohortId = selectedComparator(),
                               returnReactable = TRUE)
  })


  getAllDbSimFiltered <- shiny::reactive({
    getSimilarityAllDatabases() %>%
      filter(databaseId %in% input$selectedDatabases) %>%
      arrange(databaseId, cdmSourceAbbreviation, desc(cosineSimilarity)) %>%
      group_by(databaseId, cdmSourceAbbreviation, .add = FALSE) %>%
      mutate(cdmSpecificRank = row_number(),
             comparatorsInCdm = n_distinct(cohortDefinitionId2)) %>%
      ungroup() %>%
      mutate(
        cdmSpecificRankStr = paste(
          prettyNum(cdmSpecificRank, big.mark = ","),
          "of",
          prettyNum(comparatorsInCdm, big.mark = ","))) %>%
      group_by(cohortDefinitionId2) %>%
      filter(n() >= input$minNumDatabases) %>%
      ungroup()
  })


  getResSum <- shiny::reactive({
    resAll <- getAllDbSimFiltered()

    resSum <- resAll %>%
      ungroup() %>%
      group_by(cohortDefinitionId2, shortName, isAtc2, atc3Related, atc4Related) %>%
      summarise(
        nDatabases = n(),
        avg = mean(ifelse(input$avgOn == "Average similarity score", cosineSimilarity, cdmSpecificRank)),
        .groups = "drop") %>%
      arrange(desc(avg * ifelse(input$avgOn == "Average similarity score", 1, -1))) %>%
      mutate(rank = row_number())

    if (input$avgOn == "Average similarity score") {
      resSum <- resAll %>%
        ungroup() %>%
        group_by(cohortDefinitionId2, shortName, isAtc2, atc3Related, atc4Related) %>%
        summarise(
          nDatabases = n(),
          avg = mean(cosineSimilarity),
          .groups = "drop") %>%
        arrange(desc(avg)) %>%
        mutate(rank = row_number())


    } else if (input$avgOn == "Average source-specific rank") {
      resSum <- resAll %>%
        ungroup() %>%
        group_by(cohortDefinitionId2, shortName, isAtc2, atc3Related, atc4Related) %>%
        summarise(
          nDatabases = n(),
          avg = mean(cdmSpecificRank),
          .groups = "drop") %>%
        arrange(avg) %>%
        mutate(rank = row_number())

    }

    return(resSum)
  })

  #### ---- multi-database cosine similarity reactable ---- ####
  output$multiDatabaseSimTable <- reactable::renderReactable({
    resAll <- getAllDbSimFiltered()
    resSum <- getResSum()
    shiny::withProgress({

      outerOnClick <- sprintf("function(rowInfo, column) {
          // Send the click event to Shiny, which will be available in input$show_details
          // Note that the row index starts at 0 in JavaScript, so we add 1
          if(column.id == 'cohortDefinitionId2'){
            Shiny.setInputValue('%s', { index: rowInfo.values.cohortDefinitionId2 }, { priority: 'event' })
          }
        }", session$ns('show_exclusion')
      )

      rt <- reactable::reactable(
        data = select(resSum, isAtc2, shortName, rank, avg, nDatabases, atc3Related, atc4Related, cohortDefinitionId2),
        details = function(index) {
          cohortId <- resSum$cohortDefinitionId2[index]
          detailData <- resAll[resAll$shortName == resSum$shortName[index], c("databaseId", "cdmSourceAbbreviation", "numPersons", "cosineSimilarity", "cdmSpecificRankStr")]
          detailData <- detailData[order(detailData$cdmSourceAbbreviation),]

          selectionJs <- sprintf("

          function(rowInfo, column) {
          // Send the click event to Shiny, which will be available in input$show_details
          // Note that the row index starts at 0 in JavaScript, so we add 1
          if(column.id == 'databaseId'){
            Shiny.setInputValue('%s', { index: [%f, rowInfo.values.databaseId] }, { priority: 'event' })
          }
        }", session$ns('show_details'), cohortId)

          htmltools::div(
            style = "padding: 1rem",
            reactable::reactable(
              data = detailData,
              columns = list(
                "cdmSourceAbbreviation" = reactable::colDef(
                  name = "Data Source",
                  align = "right",
                  vAlign = "center",
                  headerVAlign = "bottom",
                  minWidth = 125),
                "numPersons" = reactable::colDef(
                  name = "Sample Size",
                  align = "center",
                  cell = function(value) { prettyNum(value, big.mark = ",") },
                  vAlign = "center",
                  headerVAlign = "bottom",
                  minWidth = 125),
                "cosineSimilarity" = reactable::colDef(
                  name = "Cohort Similarity Score",
                  cell = function(value) { sprintf(fmtSim, value) },
                  align = "center",
                  vAlign = "center",
                  headerVAlign = "bottom",
                  minWidth = 125),
                "cdmSpecificRankStr" = reactable::colDef(
                  name = "Source-Specific Rank",
                  align = "right",
                  vAlign = "center",
                  headerVAlign = "bottom",
                  minWidth = 125),
                "databaseId" = reactable::colDef(
                  name = "",
                  sortable = FALSE,
                  filterable = FALSE,
                  cell = function() htmltools::tags$button("Explore Comparison")
                )
              ),
              onClick = reactable::JS(selectionJs),
              outlined = TRUE)
          )
        },
        columns = list(
          "cohortDefinitionId2" = reactable::colDef(
            name = "",
            sortable = FALSE,
            filterable = FALSE,
            cell = function() htmltools::tags$button("Recommend Covariates for Exclusion")
          ),
          "isAtc2" = reactable::colDef(
            name = "Type",
            cell = function(value) { ifelse(value == 1, "ATC Class", "RxNorm Ingredient") },
            align = "right",
            vAlign = "center",
            headerVAlign = "bottom",
            minWidth = 125),
          "rank" = reactable::colDef(
            name = "Overall Rank",
            cell = function(value) { prettyNum(value, big.mark = ",") },
            align = "center",
            vAlign = "center",
            headerVAlign = "bottom",
            minWidth = 125),
          "avg" = reactable::colDef(
            name = stringr::str_to_title(input$avgOn),
            cell = function(value) { if (input$avgOn == "Average similarity score") { sprintf(fmtSim, value) } else { format(round(value, 1), nsmall = 1, big.mark = ",") } },
            align = "center",
            vAlign = "center",
            headerVAlign = "bottom",
            minWidth = 125),
          "shortName" = reactable::colDef(
            name = "Name",
            cell = function(value) { ifelse(substr(value, 1, 6) == "RxNorm", gsub("RxNorm - ", "", value), gsub("ATC - ", "", value)) },
            align = "left",
            vAlign = "center",
            headerVAlign = "bottom",
            minWidth = 125),
          "nDatabases" = reactable::colDef(
            name = "Number of Databases",
            cell = function(value) { prettyNum(value, big.mark = ",") },
            align = "center",
            vAlign = "center",
            headerVAlign = "bottom",
            minWidth = 125),
          "atc3Related" = reactable::colDef(
            name = "At Level 3",
            cell = function(value) ifelse(is.na(value) | value == 0, "No", "Yes"),
            align = "center",
            vAlign = "center",
            headerVAlign = "bottom",
            filterable = TRUE),
          "atc4Related" = reactable::colDef(
            name = "At Level 4",
            cell = function(value) ifelse(is.na(value) | value == 0, "No", "Yes"),
            align = "center",
            vAlign = "center",
            headerVAlign = "bottom",
            filterable = TRUE)
        ),
        searchable = TRUE,
        columnGroups = list(
          reactable::colGroup(
            "Comparator",
            c("shortName", "isAtc2")),
          reactable::colGroup(
            "In ATC Class with Target",
            c("atc3Related", "atc4Related"))),
        fullWidth = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultSorted = list(rank = "asc"),
        theme = reactable::reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#eab676",
          cellPadding = "8px 12px",
          searchInputStyle = list(width = "100%")),
        onClick = reactable::JS(outerOnClick),
        showSortIcon = TRUE)
    }, message = "Rendering results", value = 0.7)
    return(rt)
  })

  output$selectedComparator <- shiny::reactive({
    selection <- reactable::getReactableState("cosineSimilarityTbl", name = "selected")
    return(!is.null(selection))
  })

  shiny::observeEvent(input$show_details, {

    tagBox <- div(
      selectedCohortInfo(),
      h3(strong("Domain Similarity Scores")),
      shinycssloaders::withSpinner(reactable::reactableOutput("cosineSimilarityTbl"))
    )

    showModal(
      modalDialog(
        title = "Covariate data",
        shiny::basicPage(
          tags$head(tags$style(".modal-dialog{ width:95%}")),
          tagBox,
          covariateUi("")
        ),
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton("closeModal", "Close")
        )
      ))
  })


  shiny::observeEvent(input$show_exclusion, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Recommend Covariates for Exclusion",
        exclusionCovariateUi(),
        ize = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton("closeModal", "Close")
        )
      )
    )
  })

  getCoOccurrenceData <- shiny::reactive({
    validate(need(input$selectedExposure, "must select exposure"),
             need(input$show_exclusion$index, "must select comparator"))

    validate(need(input$prevInputHighMax > 0, "Threshold Inputs must be between 0 and 100"),
             need(input$prevInputHighMax <= 100.0, "Threshold Inputs must be between 0 and 100"))

    validate(need(input$prevInputHighMin > 0, "Threshold Inputs must be between 0 and 100"),
             need(input$prevInputHighMin <= 100.0, "Threshold Inputs must be between 0 and 100"))

    validate(need(input$prevInputLowMax > 0, "Threshold Inputs must be between 0 and 100"),
             need(input$prevInputLowMax <= 100.0, "Threshold Inputs must be between 0 and 100"))

    validate(need(input$prevInputLowMin > 0, "Threshold Inputs must be between 0 and 100"),
             need(input$prevInputLowMin <= 100.0, "Threshold Inputs must be between 0 and 100"))

    covData <- getCoOccurenceTableData(qns,
                                       databaseIds = input$selectedDatabases,
                                       prevInputHighMax = input$prevInputHighMax/100,
                                       prevInputHighMin = input$prevInputHighMin/100,
                                       prevInputLowMax = input$prevInputLowMax/100,
                                       prevInputLowMin = input$prevInputLowMin/100,
                                       cohortDefinitionId1 = selectedExposure(),
                                       cohortDefinitionId2 = input$show_exclusion$index)
    # return data
    covData
  })

  allCohortDefinitions <- shiny::reactive(getCohortDefinitionsTable(qns, databaseId = selectedDatabases()))

  output$covTableCoOccurrence <- reactable::renderReactable({
    cohortDefinitions <- allCohortDefinitions()
    # get data
    covData <- getCoOccurrenceData()

    # create column names with cohort sample sizes
    targetName <-
      cohortDefinitions$shortName[cohortDefinitions$cohortDefinitionId == selectedExposure()][1]
    comparatorName <-
      cohortDefinitions$shortName[cohortDefinitions$cohortDefinitionId == input$show_exclusion$index][1]

    # subset data and select relevant columns
    tableData <- covData %>%
      filter() %>%
      arrange(desc(abs(stdDiff)))  %>%
      mutate(covariateShortName = gsub("concept co-occurrence:", "", covariateShortName)) %>%
      mutate(conceptId = abs(covariateId)) %>%
      select(conceptId, cdmSourceAbbreviation, covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "cdmSourceAbbreviation" = reactable::colDef(name = "Data Source", align = "right", vAlign = "bottom"),
        "conceptId" = reactable::colDef(name = "Concept Id", align = "right", vAlign = "bottom"),
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = targetName, cell = function(value) { ifelse(value >= 0.01, percent(value, accuracy = 0.1), "<1%") }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = comparatorName, cell = function(value) { ifelse(value >= 0.01, percent(value, accuracy = 0.1), "<1%") }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(
          name = "Std. Diff.",

          cell = function(value, index) {

            if (tableData$mean1[index] >= 0.01 & tableData$mean2[index] >= 0.01) {

              sprintf(fmtSmd, value)

            } else {

              ifelse(tableData$mean1[index] < 0.01, paste0("(\u2265) ", sprintf(fmtSmd, value)), paste0("(\u2264) ", sprintf(fmtSmd, value))) } },
          align = "center",
          vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      filterable = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)

  })


  shiny::observeEvent(input$closeModal, {
    shiny::removeModal()
  })

  shiny::outputOptions(output,
                       "selectedComparator",
                       suspendWhenHidden = FALSE)

  #### ---- step-function plot of cosine similarity by rank ---- ####
  output$stepPlot <- plotly::renderPlotly({
    res <- getAllDbSimFiltered()  %>%
      dplyr::mutate(
        rank = row_number(),
        tooltip = stringr::str_wrap(
          string = paste0(
            shortName,
            " (",
            sprintf(fmtSim, cosineSimilarity),
            ") #",
            prettyNum(cdmSpecificRank, big.mark = ","),
            " of ",
            prettyNum(comparatorsInCdm, big.mark = ",")),
          width = 20, indent = 1, exdent = 1))

    plotly::plot_ly(
      data = res,
      x = ~cdmSpecificRank,
      y = ~cosineSimilarity,
      color = ~cdmSourceAbbreviation,
      type = "scatter",
      mode = "lines",
      text = ~tooltip,
      hovertemplate = "%{text}") %>%
      plotly::layout(
        hovermode = "x unified",
        xaxis = list(title = "Rank"),
        yaxis = list(title = "Cohort Similarity Score"),
        legend = list(orientation = 'h', y = -0.5))

  })

  ##### ---- function to get covariate data for a given comparison ---- ####
  getCovData <- shiny::reactive({
    shiny::validate(shiny::need(input$selectedExposure, "must select exposure"),
                    shiny::need(selectedComparator(), "must select comparator"))

    shiny::withProgress({
      covData <- getPairwiseCovariateData(qns,
                                          databaseId = selectedDatabase(),
                                          cohortDefinitionId1 = input$selectedExposure,
                                          cohortDefinitionId2 = selectedComparator())
    }, message = "Loading covariate data")
    # return data
    covData
  })

  #### ---- scatterplot of covariate prevalence ---- ####
  output$scatterPlot <- plotly::renderPlotly({

    shiny::validate(need(input$selectedExposure, 'must select exposure'),
                    need(selectedComparator(), 'must select comparator'))

    plot <- getCovData() %>%
      dplyr::mutate(
        covariateShortName = gsub("Condition in <=30d prior:", "", covariateShortName),
        covariateShortName = gsub("Condition in >30d prior:", "", covariateShortName),
        covariateShortName = gsub("Drug with start >30d prior:", "", covariateShortName),
        covariateShortName = stringr::str_to_sentence(gsub("<=30d prior|Visit:", "", covariateShortName))) %>%
      dplyr::mutate(
        type = NA,
        type = ifelse(covariateType == "Demographics", "Demographics", type),
        type = ifelse(covariateType == "Presentation", "Presentation", type),
        type = ifelse(covariateType == "Medical history", "Medical History", type),
        type = ifelse(covariateType == "prior meds", "Prior Medications", type),
        type = ifelse(covariateType == "visit context", "Visit Context", type),
        type = factor(type, levels = c("Demographics", "Presentation", "Medical History", "Prior Medications", "Visit Context")),
        tooltip = paste0(
          "<b>",
          stringr::str_wrap(string = covariateShortName, width = 20, indent = 1, exdent = 1),
          "</b>\n",
          "Target: ", ifelse(mean1 < 0.01, "<1%", scales::percent(mean1, accuracy = 0.1)), "\n",
          "Comparator: ", ifelse(mean2 < 0.01, "<1%", scales::percent(mean2, accuracy = 0.1)), "\n",
          "Std. Diff.: ", ifelse(mean1 < 0.01 | mean2 < 0.01,
                                 ifelse(mean1 < 0.01, paste0("(\u2265) ", sprintf(fmtSmd, stdDiff)), paste0("(\u2264) ", sprintf(fmtSmd, stdDiff))),
                                 sprintf(fmtSmd, stdDiff))
        )) %>%
      plotly::plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = ~mean1,
        y = ~mean2,
        color = ~type,
        text = ~tooltip,
        marker = list(opacity = 0.7),
        hovertemplate = "%{text}"
      ) %>%
      plotly::layout(
        xaxis = list(title = "Prevalence in\nTarget Cohort", tickformat = ".0%"),
        yaxis = list(title = "Prevalence in\nComparator Cohort", tickformat = ".0%"),
        legend = list(orientation = 'h', y = -0.5),
        shapes = list(list(
          type = "line",
          x0 = 0,
          x1 = ~max(mean1, mean2),
          xref = "x",
          y0 = 0,
          y1 = ~max(mean1, mean2),
          yref = "y",
          line = list(color = "black", dash = "dot")
        ))
      )
  })

  #### ---- plot of std. diffs. ---- ####
  output$smdPlot <- plotly::renderPlotly({
    shiny::validate(shiny::need(input$selectedExposure, 'must select exposure'),
                    shiny::need(selectedComparator(), 'must select comparator'))

    vline <- function(x = 0, color = "black") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot")
      )
    }

    plot <- getCovData() %>%
      dplyr::mutate(
        covariateShortName = gsub("Condition in <=30d prior:", "", covariateShortName),
        covariateShortName = gsub("Condition in >30d prior:", "", covariateShortName),
        covariateShortName = gsub("Drug with start >30d prior:", "", covariateShortName),
        covariateShortName = stringr::str_to_sentence(gsub("<=30d prior|Visit:", "", covariateShortName))) %>%
      dplyr::mutate(
        type = NA,
        type = ifelse(covariateType == "Demographics", "Demographics", type),
        type = ifelse(covariateType == "Presentation", "Presentation", type),
        type = ifelse(covariateType == "Medical history", "Medical History", type),
        type = ifelse(covariateType == "prior meds", "Prior Medications", type),
        type = ifelse(covariateType == "visit context", "Visit Context", type),
        type = factor(type, levels = (c("Demographics", "Presentation", "Medical History", "Prior Medications", "Visit Context"))),
        tooltip = paste0(
          "<b>",
          stringr::str_wrap(string = covariateShortName, width = 20, indent = 1, exdent = 1),
          "</b>\n",
          "Target: ", ifelse(mean1 < 0.01, "<1%", scales::percent(mean1, accuracy = 0.1)), "\n",
          "Comparator: ", ifelse(mean2 < 0.01, "<1%", scales::percent(mean2, accuracy = 0.1)), "\n",
          "Std. Diff.: ", ifelse(mean1 < 0.01 | mean2 < 0.01,
                                 ifelse(mean1 < 0.01, paste0("(\u2265) ", sprintf(fmtSmd, stdDiff)), paste0("(\u2264) ", sprintf(fmtSmd, stdDiff))),
                                 sprintf(fmtSmd, stdDiff))
        )) %>%
      plotly::plot_ly(
        hovertemplate = "%{text}"
      ) %>%
      plotly::add_markers(
        x = ~stdDiff,
        y = ~jitter(as.numeric(type)),
        color = ~type,
        marker = list(opacity = 0.7),
        text = ~tooltip
      ) %>%
      plotly::layout(
        xaxis = list(title = "Standardized Difference"),
        yaxis = list(title = "", showticklabels = FALSE),
        shapes = list(vline(-0.1), vline(0.1)),
        legend = list(orientation = 'h', y = -0.5))

  })

  inBalanceString <- function(covData) {
    inBalanceCount <- covData %>%
      filter(abs(stdDiff) < 0.1) %>%
      count() %>%
      pull()

    percentBalanced <- round(inBalanceCount / nrow(covData) * 100, 1)
    paste(inBalanceCount, " of", nrow(covData), "covariates", paste0("(", percentBalanced, "%)"),
          "have absolute standardized difference less than 0.1")
  }

  output$covTableDemoBalance <- shiny::renderText({
    covData <- getCovData() %>% filter(covariateType == "Demographics")
    inBalanceString(covData)
  })

  #### ---- "table 1": demographics (default to unsorted) ---- ####
  output$covTableDemo <- reactable::renderReactable({
    renderCovariateReactable(covariateType = "Demographics",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "",
                             stringToSentence = TRUE)
  })

  output$covTablePresBalance <- shiny::renderText({
    covData <- getCovData() %>% filter(covariateType == "Presentation")
    inBalanceString(covData)
  })

  #### ---- "table 1": presentation (default to sort by abs. std. diff.) ---- ####
  output$covTablePres <- reactable::renderReactable({
    # table code
    renderCovariateReactable(covariateType = "Presentation",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "Condition in <=30d prior:")

  })

  output$covTableMhistBalance <- shiny::renderText({
    covData <- getCovData() %>% filter(covariateType == "Medical history")
    inBalanceString(covData)
  })

  #### ---- "table 1": medical history (default to sort by abs. std. diff.) ---- ####
  output$covTableMhist <- reactable::renderReactable({
    # table code
    renderCovariateReactable(covariateType = "Medical history",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "Condition in >30d prior:")
  })

  output$covTablePmedsBalance <- shiny::renderText({
    covData <- getCovData() %>% filter(covariateType == "prior meds")
    inBalanceString(covData)
  })

  #### ---- "table 1": prior meds (default to sort by abs. std. diff.) ---- ####
  output$covTablePmeds <- reactable::renderReactable({
    renderCovariateReactable(covariateType = "prior meds",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "Drug with start >30d prior:")
  })

  output$covTableVisitBalance <- shiny::renderText({
    covData <- getCovData() %>% filter(covariateType == "visit context")
    inBalanceString(covData)
  })


  #### ---- "table 1": visit context (default to unsorted) ---- ####
  output$covTableVisit <- reactable::renderReactable({
    # table code
    renderCovariateReactable(covariateType = "visit context",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "<=30d prior|Visit:")

  })

  output$dataSources <- reactable::renderReactable({
    getDbDataSourcesTable(qns)
  })


  output$covTableIndex <- reactable::renderReactable({
    renderCovariateReactable(covariateType = "Co-occurrence",
                             cohortDefinitionReactive = getCohortDefinitionsWithDbCounts,
                             covariateDataReactive = getCovData,
                             selectedExposure = selectedExposure,
                             selectedComparator = selectedComparator,
                             fmtSmd = fmtSmd,
                             covariateReplaceString = "concept co-occurrence:")
  })

  observeEvent(input$showRankings, {
    shiny::showModal(shiny::modalDialog(title = "Comparator Ranks",
                                        p(""),
                                        shinycssloaders::withSpinner(plotly::plotlyOutput("stepPlot"))))
  })
})
