library(shiny)
library(DT)
library(dplyr)

source("plotsAndTables.R")

shinyServer(function(input, output, session) {
  
  # Effect-size-estimate-based metrics --------------------------------------------------------------------------
  
  exposureId <- reactive(
    exposure %>%
      filter(.data$exposureName == input$exposure) %>%
      pull(.data$exposureId)
  )
  
  baseExposureId <- reactive(
    exposure %>%
      filter(.data$exposureName == input$exposure) %>%
      pull(.data$baseExposureId)
  )
  
  observe({
    timePeriodSubset <- timePeriod %>%
      filter(.data$exposureId == baseExposureId()) %>%
      pull(.data$label)
    if (length(timePeriodSubset) == 0) {
      updateSelectInput(session, "period", choices = "No data for these inputs", selected = "No data for these inputs")
    } else {
      updateSelectInput(session, "period", choices = timePeriodSubset, selected = timePeriodSubset[length(timePeriodSubset)])
    }
  })
  
  periodId <- reactive({
    periodId <- timePeriod %>%
      filter(.data$label == input$period & .data$exposureId == baseExposureId()) %>%
      pull(.data$periodId)
    if (length(periodId) == 0) {
      return(-1)
    } else {
      return(periodId)
    }
  }) 
  
  getAllEstimates <- reactive({
    # Fetching data across methods and periods to compute full grid of controls
    subset <- getEstimates(connection = connectionPool,
                           schema = schema,
                           databaseId = input$database,
                           exposureId = exposureId(),
                           timeAtRisk = input$timeAtRisk)
    return(subset)
  })
  
  # Per period
  
  filterEstimates <- reactive({
    subset <- getAllEstimates() 
    subset <- subset %>%
      filter(.data$method %in% input$method & .data$periodId == periodId())
    if (input$calibrated == "Calibrated") {
      subset$logRr <- subset$calibratedLogRr
      subset$seLogRr <- subset$calibratedSeLogRr
      subset$ci95Lb <- subset$calibratedCi95Lb
      subset$ci95Ub <- subset$calibratedCi95Ub
      subset$p <- subset$calibratedP
    }
    subset <- addTrueEffectSize(subset, negativeControlOutcome, positiveControlOutcome)
    return(subset)
  })
  
  selectedEstimates <- reactive({
    if (is.null(input$performanceMetrics_rows_selected)) {
      return(NULL)
    } 
    subset <- filterEstimates()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    subset <- subset[subset$method == performanceMetrics()$Method[input$performanceMetrics_rows_selected] & 
                       subset$analysisId == performanceMetrics()$'<span title=\"Analysis variant ID\">ID</span>'[input$performanceMetrics_rows_selected], ]
    if (nrow(subset) == 0) {
      return(NULL)
    } 
    return(subset)
  })
  
  output$tableCaption <- renderUI({
    subset <- filterEstimates()
    validate(
      need(nrow(subset) > 0, "No data found for these inputs")
    )
    subset <- unique(subset[, c("exposureId", "outcomeId", "effectSize")])
    ncCount <- sum(subset$effectSize == 1)
    pcCount <- sum(subset$effectSize != 1)
    return(HTML(sprintf("<strong>Table 1</strong> Metrics based on the effect-size estimate (e.g hazard ratio or odds ratio), using %s negative and %s positive controls.", 
                        ncCount, 
                        pcCount)))
  })
  
  performanceMetrics <- reactive({
    subset <- filterEstimates()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    combis <- lapply(split(subset, paste(subset$method, subset$analysisId)), 
                     computeEffectEstimateMetrics, 
                     trueRr = input$trueRr)
    combis <- bind_rows(combis)
    colnames(combis) <- c("Method", 
                          "<span title=\"Analysis variant ID\">ID</span>", 
                          "<span title=\"Area under the receiver operator curve\">AUC</span>", 
                          "<span title=\"Coverage of the 95% confidence interval\">Coverage</span>", 
                          "<span title=\"Geometric mean precision (1/SE^2)\">Mean Precision</span>", 
                          "<span title=\"Mean Squared Error\">MSE</span>", 
                          "<span title=\"Type I Error\">Type I error</span>", 
                          "<span title=\"Type II Error\">Type II error</span>", 
                          "<span title=\"Fraction where estimate could not be computed\">Non-estimable</span>")
    return(combis)
  })
  
  output$performanceMetrics <- renderDataTable({
    selection = list(mode = "single", target = "row")
    options = list(pageLength = 10, 
                   searching = FALSE, 
                   lengthChange = TRUE)
    isolate(
      if (!is.null(input$performanceMetrics_rows_selected)) {
        selection$selected = input$performanceMetrics_rows_selected
        options$displayStart = floor(input$performanceMetrics_rows_selected[1] / 10) * 10 
      }
    )
    data <- performanceMetrics()
    if (nrow(data) == 0) {
      return(data)
    }
    table <- DT::datatable(data, selection = selection, options = options, rownames = FALSE, escape = FALSE) 
    
    colors <- c("#b7d3e6", "#b7d3e6", "#b7d3e6", "#f2b4a9", "#f2b4a9", "#f2b4a9", "#f2b4a9")
    mins <- c(0, 0, 0, 0, 0, 0, 0)
    maxs <- c(1, 1, max(data[, 5]), max(data[, 6]), 1, 1, 1)
    for (i in 1:length(colors)) {
      table <- DT::formatStyle(table = table, 
                               columns = i + 2,
                               background = styleColorBar(c(mins[i], maxs[i]), colors[i]),
                               backgroundSize = '98% 88%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center')
    }
    return(table)
  })
  
  output$estimates <- renderPlot({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      subset$Group <- as.factor(paste("True effect size =", subset$effectSize))
      return(plotScatter(subset))
    }
  })
  
  output$details <- renderText({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      method <- as.character(subset$method[1])
      analysisId <- subset$analysisId[1]
      description <- analysis$description[analysis$method == method & analysis$analysisId == analysisId]
      return(paste0(method , " analysis ", analysisId, ": ", description))
    }
  })
  
  outputOptions(output, "details", suspendWhenHidden = FALSE)
  
  output$rocCurves <- renderPlot({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    } else {
      subset$trueLogRr <- log(subset$effectSize)
      return(plotRocsInjectedSignals(logRr = subset$logRr, trueLogRr = subset$trueLogRr, showAucs = TRUE))
    }
    
  })
  
  output$hoverInfoEstimates <- renderUI({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    } 
    subset$Group <- as.factor(paste("True effect size =", subset$effectSize))
    hover <- input$plotHoverInfoEstimates
    
    point <- nearPoints(subset, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      return(NULL)
    }
    leftPx <- hover$coords_css$x
    topPx <- hover$coords_css$y
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:",
                    leftPx -405,
                    "px; top:",
                    topPx - 50,
                    "px; width:400px;")
    
    estimate <- paste0(formatC(exp(point$logRr), digits = 2, format = "f"),
                       " (",
                       formatC(point$ci95Lb, digits = 2, format = "f"),
                       "-",
                       formatC(point$ci95Ub, digits = 2, format = "f"),
                       ")")
    
    text <- paste0("<b> outcome: </b>", point$outcomeName, "<br/>",
                   "<b> estimate: </b>", estimate, "<br/>")
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(style = style, p(HTML(text)))
    )
  })
  
  observe({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    } 
    if (subset$method[1] == "HistoricalComparator") {
      showTab(inputId = "perPeriodTabSetPanel", target = "Diagnostics")
    } else {
      hideTab(inputId = "perPeriodTabSetPanel", target = "Diagnostics")
    }
  })
  
  monthlyRatesDateRanges <- reactive({
    endDate <- timePeriod %>%
      filter(.data$exposureId == baseExposureId() & .data$label== input$period) %>%
      pull(.data$endDate)
    
    dateRanges <- exposure %>%
      filter(.data$exposureId == exposureId()) %>%
      select(.data$historyStartDate, .data$historyEndDate, .data$startDate) %>%
      mutate(endDate = !!endDate)
    return(dateRanges)
  })
  
  monthlyRates <- reactive({
    dateRanges <- monthlyRatesDateRanges()
    
    monthlyRates <- getMontlyRates(connection = connectionPool, 
                                   schema = schema, 
                                   databaseId = input$database, 
                                   startDate = dateRanges$historyStartDate - 366, 
                                   endDate = dateRanges$endDate)
    return(monthlyRates)
  })
  
  filteredMonthlyRates <- reactive({
    monthlyRates <- monthlyRates()
    dateRanges <- monthlyRatesDateRanges()
    threshold <- input$minRateChange / 100
    
    monthlyRates <- monthlyRates %>%
      filter(.data$outcomeId %in% c(negativeControlOutcome$outcomeId, 
                                    positiveControlOutcome %>% 
                                      filter(.data$exposureId == baseExposureId()) %>%
                                      pull(.data$outcomeId)))
    
    historicIr <- monthlyRates %>%
      filter(.data$endDate > dateRanges$historyStartDate & .data$startDate < dateRanges$historyEndDate) %>%
      group_by(.data$outcomeId) %>%
      summarise(historicIr = sum(.data$outcomes) / sum(.data$days))
    currentIr <- monthlyRates %>%
      filter(.data$endDate > dateRanges$startDate & .data$startDate < dateRanges$endDate) %>%
      group_by(.data$outcomeId) %>%
      summarise(currentIr = sum(.data$outcomes) / sum(.data$days))
    outcomeIds <- inner_join(historicIr, currentIr, by = "outcomeId") %>%
      mutate(delta = abs((.data$currentIr - .data$historicIr) / .data$historicIr)) %>%
      filter(.data$delta > threshold) %>%
      pull(.data$outcomeId)
    ratesSubset <- monthlyRates %>%
      filter(.data$outcomeId %in% outcomeIds) 
    return(ratesSubset)
  })
  
  output$monthlyRates <- renderPlot({
    rates <- filteredMonthlyRates()
    dateRanges <- monthlyRatesDateRanges()
    plotMonthlyRates(monthlyRates = rates, 
                     historyStartDate = dateRanges$historyStartDate,
                     historyEndDate = dateRanges$historyEndDate,
                     startDate = dateRanges$startDate,
                     endDate = dateRanges$endDate,
                     negativeControlOutcome = negativeControlOutcome,
                     positiveControlOutcome = positiveControlOutcome)
  })
  
  # Across periods
  
  filterEstimatesAcrossPeriods <- reactive({
    subset <- getAllEstimates() %>%
      filter(.data$method %in% input$method)
    if (input$calibrated == "Calibrated") {
      subset$rr <- subset$calibratedRr
      subset$logRr <- subset$calibratedLogRr
      subset$seLogRr <- subset$calibratedSeLogRr
      subset$ci95Lb <- subset$calibratedCi95Lb
      subset$ci95Ub <- subset$calibratedCi95Ub
      subset$p <- subset$calibratedP
    }
    subset <- addTrueEffectSize(subset, negativeControlOutcome, positiveControlOutcome)
    return(subset)
  })
  
  performanceMetricsAcrossPeriods <- reactive({
    subset <- filterEstimatesAcrossPeriods()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    combis <- subset %>%
      distinct(.data$method, .data$analysisId) %>%
      arrange(.data$method, .data$analysisId)
    colnames(combis) <- c("Method", 
                          "<span title=\"Analysis variant ID\">ID</span>")
    return(combis)
  })
  
  output$performanceMetricsAcrossPeriods <- renderDataTable({
    selection = list(mode = "single", target = "row")
    options = list(pageLength = 10, 
                   searching = FALSE, 
                   lengthChange = TRUE)
    isolate(
      if (!is.null(input$performanceMetricsAcrossPeriods_rows_selected)) {
        selection$selected = input$performanceMetricsAcrossPeriods_rows_selected
        options$displayStart = floor(input$performanceMetricsAcrossPeriods_rows_selected[1] / 10) * 10 
      }
    )
    data <- performanceMetricsAcrossPeriods()
    if (nrow(data) == 0) {
      return(data)
    }
    table <- DT::datatable(data, selection = selection, options = options, rownames = FALSE, escape = FALSE) 
    return(table)
  })
  
  output$tableAcrossPeriodsCaption <- renderUI({
    subset <- filterEstimatesAcrossPeriods()
    validate(
      need(nrow(subset) > 0, "No data found for these inputs")
    )
    subset <- unique(subset[, c("exposureId", "outcomeId", "effectSize")])
    ncCount <- sum(subset$effectSize == 1)
    pcCount <- sum(subset$effectSize != 1)
    return(HTML(sprintf("<strong>Table 1.b</strong> Metrics based on the effect-size estimate (e.g hazard ratio or odds ratio), using %s negative and %s positive controls.", 
                        ncCount, 
                        pcCount)))
  })
  
  selectedEstimatesAcrossPeriods <- reactive({
    if (is.null(input$performanceMetricsAcrossPeriods_rows_selected)) {
      return(NULL)
    } 
    subset <- filterEstimatesAcrossPeriods()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    subset <- subset[subset$method == performanceMetricsAcrossPeriods()$Method[input$performanceMetricsAcrossPeriods_rows_selected] & 
                       subset$analysisId == performanceMetricsAcrossPeriods()$'<span title=\"Analysis variant ID\">ID</span>'[input$performanceMetricsAcrossPeriods_rows_selected], ]
    if (nrow(subset) == 0) {
      return(NULL)
    } 
    return(subset)
  })
  
  output$detailsAcrossPeriods <- renderText({
    subset <- selectedEstimatesAcrossPeriods()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      method <- as.character(subset$method[1])
      analysisId <- subset$analysisId[1]
      description <- analysis$description[analysis$method == method & analysis$analysisId == analysisId]
      return(paste0(method , " analysis ", analysisId, ": ", description))
    }
  })
  
  outputOptions(output, "detailsAcrossPeriods", suspendWhenHidden = FALSE)
  
  output$estimatesAcrossPeriods <- renderPlot({
    subset <- selectedEstimatesAcrossPeriods()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      subset$Group <- as.factor(paste("True effect size =", subset$effectSize))
      return(plotEstimatesAcrossPeriods(subset))
    }
  })
  
  # Across periods and methods
  
  output$sensSpecAcrossMethods <- renderPlot({
    subset <- filterEstimatesAcrossPeriods()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    # Drop negative controls that weren't powered to be used for positive control synthesis so all on equal power:
    subset <- subset %>%
      filter(.data$outcomeId %in% c(positiveControlOutcome$outcomeId, positiveControlOutcome$negativeControlId))
    # x <<- subset
    plot <- plotSensSpecAcrossMethods(subset, input$trueRr)
    return(plot)
  })
  
  output$analysesDescriptions <- renderDataTable({
    options = list(pageLength = 100, 
                   searching = FALSE, 
                   lengthChange = FALSE)
    data <- analysis %>% 
      filter(.data$method %in% input$method & .data$timeAtRisk == input$timeAtRisk) %>%
      select(.data$method, .data$analysisId, .data$description) %>%
      arrange(.data$method, .data$analysisId)
    
    table <- DT::datatable(data, options = options, colnames = c("Method", "AnalysisId", "Description"), rownames = FALSE, escape = FALSE) 
    return(table)
  })
  
  # MaxSPRT-based metrics --------------------------------------------------------------------------
  exposureId2 <- reactive(
    exposure %>%
      filter(.data$exposureName == input$exposure2) %>%
      pull(.data$exposureId)
  )
  
  
  getAllEstimates2 <- reactive({
    # Fetching data across methods and periods to compute full grid of controls
    subset <- getEstimates(connection = connectionPool,
                           schema = schema,
                           databaseId = input$database2,
                           exposureId = exposureId2(),
                           timeAtRisk = input$timeAtRisk2)
    return(subset)
  })
  
  filterEstimates2 <- reactive({
    subset <- getAllEstimates2() %>%
      filter(.data$method %in% input$method2 & .data$exposureOutcomes >= as.numeric(input$minOutcomes))
    subset <- addTrueEffectSize(subset, negativeControlOutcome, positiveControlOutcome)
    return(subset)
  })
  
  # Per method
  
  selectedEstimates2 <- reactive({
    if (is.null(input$performanceMetrics2_rows_selected)) {
      return(NULL)
    } 
    subset <- filterEstimates2()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    subset <- subset[subset$method == performanceMetrics2()$Method[input$performanceMetrics2_rows_selected] & 
                       subset$analysisId == performanceMetrics2()$'<span title=\"Analysis variant ID\">ID</span>'[input$performanceMetrics2_rows_selected], ]
    if (nrow(subset) == 0) {
      return(NULL)
    } 
    return(subset)
  })
  
  output$table2Caption <- renderUI({
    subset <- filterEstimates2()
    validate(
      need(nrow(subset) > 0, "No data found for these inputs")
    )
    
    subset <- unique(subset[, c("exposureId", "outcomeId", "effectSize")])
    ncCount <- sum(subset$effectSize == 1)
    pcCount <- sum(subset$effectSize != 1)
    return(HTML(sprintf("<strong>Table 2</strong> Metrics based on the MaxSPRT statistics, using %s negative and %s positive controls.", 
                        ncCount, 
                        pcCount)))
  })
  
  performanceMetrics2 <- reactive({
    subset <- filterEstimates2()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    # Drop negative controls that weren't powered to be used for positive control synthesis so all on equal power:
    subset <- subset %>%
      filter(.data$outcomeId %in% c(positiveControlOutcome$outcomeId, positiveControlOutcome$negativeControlId))
    combis <- lapply(split(subset, paste(subset$method, subset$analysisId)), 
                     computeMaxSprtMetrics, 
                     trueRr = input$trueRr2)
    combis <- bind_rows(combis)
    colnames(combis) <- c("Method", 
                          "<span title=\"Analysis variant ID\">ID</span>", 
                          "<span title=\"The first period when 80% sensitivity is achieved\">Period 80% Sens</span>", 
                          "<span title=\"Sensitivity in the period when 80% sensitivity is achieved\">Sensitivity at period</span>", 
                          "<span title=\"Specifiticity in the period when 80% sensitivity is achieved\">Specifiticity at period</span>")
    return(combis)
  })
  
  output$performanceMetrics2 <- renderDataTable({
    selection = list(mode = "single", target = "row")
    options = list(pageLength = 10, 
                   searching = FALSE, 
                   lengthChange = TRUE)
    isolate(
      if (!is.null(input$performanceMetrics2_rows_selected)) {
        selection$selected = input$performanceMetrics2_rows_selected
        options$displayStart = floor(input$performanceMetrics2_rows_selected[1] / 10) * 10 
      }
    )
    data <- performanceMetrics2()
    if (nrow(data) == 0) {
      return(data)
    }
    table <- DT::datatable(data, selection = selection, options = options, rownames = FALSE, escape = FALSE) 
    
    colors <- c("#f2b4a9", "#b7d3e6", "#b7d3e6")
    mins <- c(0, 0.8, min(data[, 5], na.rm = TRUE))
    maxs <- c(max(filterEstimates2()$periodId), 1, 1)
    for (i in 1:length(colors)) {
      table <- DT::formatStyle(table = table,
                               columns = i + 2,
                               background = styleColorBar(c(mins[i], maxs[i]), colors[i]),
                               backgroundSize = '98% 88%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center')
    }
    return(table)
  })
  
  output$details2 <- renderText({
    subset <- selectedEstimates2()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      method <- as.character(subset$method[1])
      analysisId <- subset$analysisId[1]
      description <- analysis$description[analysis$method == method & analysis$analysisId == analysisId]
      return(paste0(method , " analysis ", analysisId, ": ", description))
    }
  })
  
  outputOptions(output, "details2", suspendWhenHidden = FALSE)
  
  
  output$llrs <- renderPlot({
    subset <- selectedEstimates2()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      subset$Group <- as.factor(paste("True effect size =", subset$effectSize))
      vaccinationsSubset <- vaccinations %>%
        filter(.data$databaseId == input$database2 & .data$exposureId == exposureId2())
      return(plotLlrs(subset, vaccinationsSubset, trueRr = input$trueRr2))
    }
  })
  
  output$hoverInfoLlrs <- renderUI({
    subset <- selectedEstimates2()
    if (is.null(subset)) {
      return(NULL)
    } 
    hover <- input$plotHoverInfoLlrs
    if (is.null(hover)) {
      return(NULL)
    }
    if (input$trueRr2 != "Overall") {
      subset <- subset %>%
        filter(.data$effectSize == 1 | .data$effectSize == as.numeric(input$trueRr2))
    }
    subset <- subset %>%
      filter(!is.na(.data$llr))
    subset$Group <- as.factor(paste("True effect size =", subset$effectSize))
    subset$y <- log10(subset$llr + 1)
    hover$mapping$y = "y"
    point <- nearPoints(subset, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      return(NULL)
    }
    leftPx <- hover$coords_css$x
    topPx <- hover$coords_css$y
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:",
                    leftPx -405,
                    "px; top:",
                    topPx - 100,
                    "px; width:400px;")
    
    estimate <- paste0(formatC(point$rr, digits = 2, format = "f"),
                       " (",
                       formatC(point$ci95Lb, digits = 2, format = "f"),
                       "-",
                       formatC(point$ci95Ub, digits = 2, format = "f"),
                       ")")
    
    text <- paste0(sprintf("<b> outcome: </b>%s<br/>", point$outcomeName),
                   sprintf("<b> log likelihood ratio: </b>%0.2f<br/>", point$llr),
                   sprintf("<b> critical value: </b>%0.2f<br/>", point$criticalValue),
                   sprintf("<b> observed: </b>%s<br/>", point$exposureOutcomes),
                   sprintf("<b> expected: </b>%0.2f<br/>", point$exposureOutcomes / point$rr),
                   sprintf("<b> effect-size estimate: </b>%s<br/>", estimate))
    
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(style = style, p(HTML(text)))
    )
  })
  
  output$sensSpec <- renderPlot({
    subset <- selectedEstimates2()
    if (is.null(subset)) {
      return(NULL)
    }  else {
      # Drop negative controls that weren't powered to be used for positive control synthesis so all on equal power:
      subset <- subset %>%
        filter(.data$outcomeId %in% c(positiveControlOutcome$outcomeId, positiveControlOutcome$negativeControlId))
      return(plotSensSpec(subset))
    }
  })
  
  # Across methods
  
  output$sensSpecAcrossMethods2 <- renderPlot({
    subset <- filterEstimates2()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    # Drop negative controls that weren't powered to be used for positive control synthesis so all on equal power:
    subset <- subset %>%
      filter(.data$outcomeId %in% c(positiveControlOutcome$outcomeId, positiveControlOutcome$negativeControlId))
    
    plot <- plotMaxSprtSensSpecAcrossMethods(subset, input$trueRr2)
    return(plot)
  })
  
  output$analysesDescriptions2 <- renderDataTable({
    options = list(pageLength = 100, 
                   searching = FALSE, 
                   lengthChange = FALSE)
    data <- analysis %>% 
      filter(.data$method %in% input$method2 & .data$timeAtRisk == input$timeAtRisk2) %>%
      select(.data$method, .data$analysisId, .data$description) %>%
      arrange(.data$method, .data$analysisId)
    
    table <- DT::datatable(data, options = options, colnames = c("Method", "AnalysisId", "Description"), rownames = FALSE, escape = FALSE) 
    return(table)
  })
  
  # Database information -------------------------------------------------------
  output$databaseInfoPlot <- renderPlot({
    return(plotDbCharacteristics(databaseCharacterization))
  })
  
  output$databaseInfoTable <- renderDataTable({
    
    table <- database %>%
      select(.data$databaseId, .data$databaseName, .data$description, .data$vocabularyVersion)
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = FALSE,
                   ordering = TRUE,
                   paging = FALSE,
                   columnDefs = list(list(width = '30%', targets = 1),
                                     list(width = '60%', targets = 2))
    )
    table <- datatable(table,
                       options = options,
                       colnames = c("ID", "Name", "Description", "Vocabulary"),
                       rownames = FALSE,
                       class = "stripe compact")
    return(table)
  })
  
  # Info buttons -----------------------------------------------------------------
  
  # Effect-size estimate-based metrics tab
  observeEvent(input$vaccineInfo, {
    showModal(modalDialog(
      title = "Vaccine",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(vaccineInfoHtml)
    ))
  })
  
  observeEvent(input$timeAtRiskInfo, {
    showModal(modalDialog(
      title = "Time at risk",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(timeAtRiskInfoHtml)
    ))
  })
  
  observeEvent(input$calibrationInfo, {
    showModal(modalDialog(
      title = "Empirical calibration",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(calibrationInfoHtml)
    ))
  })
  
  observeEvent(input$periodInfo, {
    showModal(modalDialog(
      title = "Time period",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(periodInfoHtml)
    ))
  })
  
  
  observeEvent(input$databaseInfo, {
    showModal(modalDialog(
      title = "Databases",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(databaseInfoHtml)
    ))
  })
  
  observeEvent(input$trueRrInfo, {
    showModal(modalDialog(
      title = "True effect size",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(trueRrInfoHtml)
    ))
  })
  
  observeEvent(input$methodsInfo, {
    showModal(modalDialog(
      title = "Methods",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(methodsInfoHtml)
    ))
  })
  
  # MaxSPRT-based metrics tab
  observeEvent(input$vaccineInfo2, {
    showModal(modalDialog(
      title = "Vaccine",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(vaccineInfoHtml)
    ))
  })
  
  observeEvent(input$timeAtRiskInfo2, {
    showModal(modalDialog(
      title = "Time at risk",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(timeAtRiskInfoHtml)
    ))
  })
  
  observeEvent(input$databaseInfo2, {
    showModal(modalDialog(
      title = "Databases",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(databaseInfoHtml)
    ))
  })
  
  observeEvent(input$trueRrInfo2, {
    showModal(modalDialog(
      title = "True effect size",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(trueRrInfoHtml)
    ))
  })
  
  observeEvent(input$methodsInfo2, {
    showModal(modalDialog(
      title = "Methods",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(methodsInfoHtml)
    ))
  })
  
  observeEvent(input$minimumOutcomesInfo2, {
    showModal(modalDialog(
      title = "Minimum outcomes",
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(minimumOutcomesInfoHtml)
    ))
  })
  
})

