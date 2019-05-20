library(shiny)
library(DT)

source("plots.R")

shinyServer(function(input, output, session) {
  
  observe({
    if (input$evalType == "Comparative effect estimation") {
      choices = methods$method[methods$comparative == TRUE]
    } else {
      choices = methods$method
    }
    updateCheckboxGroupInput(session, "method", choices = choices, selected = choices)
  })
  
  filterEstimates <- reactive({
    subset <- estimates[estimates$database == input$database, ]
    if (input$mdrr != "All") {
      subset <- subset[!is.na(subset$mdrrTarget) & subset$mdrrTarget < as.numeric(input$mdrr), ]
      if (input$evalType == "Comparative effect estimation") {
        subset <- subset[!is.na(subset$mdrrComparator) & subset$mdrrComparator < as.numeric(input$mdrr), ]
      }
    }
    subset <- subset[subset$method %in% input$method, ]
    if (input$stratum != "All") {
      subset <- subset[subset$stratum == input$stratum, ]
    }
    if (input$calibrated == "Calibrated") {
      subset$logRr <- subset$calLogRr
      subset$seLogRr <- subset$calSeLogRr
      subset$ci95Lb <- subset$calCi95Lb
      subset$ci95Ub <- subset$calCi95Ub
      subset$p <- subset$calP
    }
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
    subset <- unique(subset[, c("targetId", "comparatorId", "oldOutcomeId", "targetEffectSize")])
    ncCount <- sum(subset$targetEffectSize == 1)
    pcCount <- sum(subset$targetEffectSize != 1)
    return(HTML(paste0("<strong>Table S.1</strong> Metrics based on ", ncCount, " negative and ", pcCount, " positive controls")))
  })
  
  performanceMetrics <- reactive({
    subset <- filterEstimates()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    combis <- unique(subset[, c("method", "analysisId")])
    if (input$trueRr == "Overall") {
      computeMetrics <- function(i) {
        forEval <- subset[subset$method == combis$method[i] & subset$analysisId == combis$analysisId[i], ]
        nonEstimable <- round(mean(forEval$seLogRr >= 99), 2)
        # forEval <- forEval[forEval$seLogRr < 99, ]
        roc <- pROC::roc(forEval$targetEffectSize > 1, forEval$logRr, algorithm = 3)
        auc <- round(pROC::auc(roc), 2)
        mse <- round(mean((forEval$logRr - log(forEval$trueEffectSize))^2), 2)
        coverage <- round(mean(forEval$ci95Lb < forEval$trueEffectSize & forEval$ci95Ub > forEval$trueEffectSize), 2)
        meanP <- round(-1 + exp(mean(log(1 + (1/(forEval$seLogRr^2))))), 2)
        type1 <- round(mean(forEval$p[forEval$targetEffectSize == 1] < 0.05), 2)
        type2 <- round(mean(forEval$p[forEval$targetEffectSize > 1] >= 0.05), 2)
        return(c(auc = auc, coverage = coverage, meanP = meanP, mse = mse, type1 = type1, type2 = type2, nonEstimable = nonEstimable))
      }
      combis <- cbind(combis, as.data.frame(t(sapply(1:nrow(combis), computeMetrics))))
    } else {
      # trueRr <- input$trueRr
      computeMetrics <- function(i) {
        if (input$trueRr == "> 1") {
          forEval <- subset[subset$method == combis$method[i] & subset$analysisId == combis$analysisId[i] & subset$targetEffectSize > 1, ]
        } else {
          forEval <- subset[subset$method == combis$method[i] & subset$analysisId == combis$analysisId[i] & subset$targetEffectSize == input$trueRr, ]
        }
        nonEstimable <- round(mean(forEval$seLogRr >= 99), 2)
        # forEval <- forEval[forEval$seLogRr < 99, ]
        mse <- round(mean((forEval$logRr - log(forEval$trueEffectSize))^2), 2)
        coverage <- round(mean(forEval$ci95Lb < forEval$trueEffectSize & forEval$ci95Ub > forEval$trueEffectSize), 2)
        meanP <- round(-1 + exp(mean(log(1 + (1/(forEval$seLogRr^2))))), 2)
        if (input$trueRr == "1") {
          auc <- NA
          type1 <- round(mean(forEval$p < 0.05), 2)  
          type2 <- NA
        } else {
          if (input$trueRr == "> 1") {
            negAndPos <- subset[subset$method == combis$method[i] & subset$analysisId == combis$analysisId[i] & (subset$targetEffectSize > 1 | subset$targetEffectSize == 1), ]
          } else {
            negAndPos <- subset[subset$method == combis$method[i] & subset$analysisId == combis$analysisId[i] & (subset$targetEffectSize == input$trueRr | subset$targetEffectSize == 1), ]
          }
          roc <- pROC::roc(negAndPos$targetEffectSize > 1, negAndPos$logRr, algorithm = 3)
          auc <- round(pROC::auc(roc), 2)
          type1 <- NA
          type2 <- round(mean(forEval$p[forEval$targetEffectSize > 1] >= 0.05), 2)  
        }
        return(c(auc = auc, coverage = coverage, meanP = meanP, mse = mse, type1 = type1, type2 = type2, nonEstimable = nonEstimable))
      }
      combis <- cbind(combis, as.data.frame(t(sapply(1:nrow(combis), computeMetrics))))
    }
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
      subset$Group <- as.factor(paste("True hazard ratio =", subset$targetEffectSize))
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
      description <- analysisRef$description[analysisRef$method == method & analysisRef$analysisId == analysisId]
      return(paste0(method , " analysis ", analysisId, ": ", description))
    }
  })
  
  outputOptions(output, "details", suspendWhenHidden = FALSE)
  
  observeEvent(input$showSettings, {
    subset <- selectedEstimates()
    method <- as.character(subset$method[1])
    analysisId <- subset$analysisId[1]
    description <- analysisRef$description[analysisRef$method == method & analysisRef$analysisId == analysisId]
    details <- analysisRef$details[analysisRef$method == method & analysisRef$analysisId == analysisId]
    showModal(modalDialog(
      title = paste0(method , " analysis. ", analysisId, ": ", description),
      pre(details),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  output$rocCurves <- renderPlot({
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    } else {
      subset$trueLogRr <- log(subset$targetEffectSize)
      return(plotRocsInjectedSignals(logRr = subset$logRr, trueLogRr = subset$trueLogRr, showAucs = TRUE))
    }
    
  })
  
  output$hoverInfoEstimates <- renderUI({
    # Hover-over adapted from https://gitlab.com/snippets/16220
    subset <- selectedEstimates()
    if (is.null(subset)) {
      return(NULL)
    } 
    subset$Group <- as.factor(paste("True hazard ratio =", subset$targetEffectSize))
    hover <- input$plotHoverInfoEstimates
    
    point <- nearPoints(subset, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 125, "px; top:", top_px - 150, "px; width:250px;")
    
    
    # actual tooltip created as wellPanel
    estimate <- paste0(formatC(exp(point$logRr), digits = 2, format = "f"),
                       " (",
                       formatC(point$ci95Lb, digits = 2, format = "f"),
                       "-",
                       formatC(point$ci95Ub, digits = 2, format = "f"),
                       ")")
    
    if (point$comparative) {
      text <- paste0("<b> target: </b>", point$targetName, "<br/>", 
                     "<b> comparator: </b>", point$comparatorName, "<br/>")
    } else {
      text <- paste0("<b> exposure: </b>", point$targetName, "<br/>")
    }
    if (point$nesting) {
      text <- paste0(text, "<b> nesting: </b>", point$nestingName, "<br/>")
    } 
    text <- paste0(text, "<b> outcome: </b>", point$outcomeName, "<br/>",
                   "<b> estimate: </b>", estimate, "<br/>")
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(style = style, p(HTML(text)))
    )
  })
  
  overviewData <- reactive({
    computeMetrics <- function(forEval, metric = "Mean precision") {
      if (metric == "AUC")
        y <- round(pROC::auc(pROC::roc(forEval$targetEffectSize > 1, forEval$logRr, algorithm = 3)), 2)
      else if (metric == "Coverage")
        y <- round(mean(forEval$ci95Lb < forEval$trueEffectSize & forEval$ci95Ub > forEval$trueEffectSize), 2)
      else if (metric == "Mean precision (1/SE^2)")
        y <- round(-1 + exp(mean(log(1 + (1/(forEval$seLogRr^2))))), 2)
      else if (metric == "Mean squared error (MSE)")
        y <- round(mean((forEval$logRr - log(forEval$trueEffectSize))^2), 2)
      else if (metric == "Type I error")
        y <- round(mean(forEval$p[forEval$targetEffectSize == 1] < 0.05), 2)
      else if (metric == "Type II error")
        y <- round(mean(forEval$p[forEval$targetEffectSize > 1] >= 0.05), 2)
      else if (metric == "Non-estimable")
        y <- round(mean(forEval$seLogRr >= 99), 2)
      return(data.frame(database = forEval$database[1],
                        method = forEval$method[1],
                        analysisId = forEval$analysisId[1],
                        stratum = forEval$stratum[1],
                        metric = y))
    }
    if (input$mdrrOverview != "All") {
      if (input$evalTypeOverview == "Comparative effect estimation") {
        subset <- estimates[!is.na(estimates$mdrrTarget) & estimates$mdrrTarget <= input$mdrrOverview & estimates$mdrrComparator <= input$mdrrOverview & estimates$comparative == TRUE, ]
      } else {
        subset <- estimates[!is.na(estimates$mdrrTarget) & estimates$mdrrTarget <= input$mdrrOverview, ]
      }
    } else {
      subset <- estimates
    }
    if (input$calibratedOverview == "Calibrated") {
      subset$logRr <- subset$calLogRr
      subset$seLogRr <- subset$calSeLogRr
      subset$ci95Lb <- subset$calCi95Lb
      subset$ci95Ub <- subset$calCi95Ub
      subset$p <- subset$calP
    }
    groups <- split(subset, paste(subset$method, subset$analysisId, subset$database, subset$stratum))
    metrics <- lapply(groups, computeMetrics, metric = input$metric)
    metrics <- do.call("rbind", metrics)
    metrics$stratum <- as.character(metrics$stratum)
    metrics$stratum[metrics$stratum == "Inflammatory Bowel Disease"] <- "IBD"
    metrics$stratum[metrics$stratum == "Acute pancreatitis"] <- "Acute\npancreatitis"
    metrics <- merge(metrics, strataSubset)
    methods <- unique(metrics$method)
    methods <- methods[order(methods)]
    n <- length(methods)
    methods <- data.frame(method = methods,
                          offsetX = ((1:n / (n + 1)) - ((n + 1) / 2) / (n + 1)))
    metrics <- merge(metrics, methods)
    metrics$x <- metrics$x + metrics$offsetX
    metrics$tidyMethod <- as.character(metrics$method)
    metrics$tidyMethod[metrics$tidyMethod == "CaseControl"] <- "Case-control"
    metrics$tidyMethod[metrics$tidyMethod == "CaseCrossover"] <- "Case-crossover"
    metrics$tidyMethod[metrics$tidyMethod == "CohortMethod"] <- "Cohort method"
    metrics$tidyMethod[metrics$tidyMethod == "SelfControlledCaseSeries"] <- "Self-controlled case series (SCCS)"
    metrics$tidyMethod[metrics$tidyMethod == "SelfControlledCohort"] <- "Self-controlled cohort (SCC)"
    metrics <- metrics[metrics$metric != 0, ]
  })
  
  output$overviewPlot <- renderPlot({
    data <- overviewData()
    plotOverview(data, input$metric, strataSubset, input$calibratedOverview)
  })
  output$overviewPlotCaption <- renderUI({
    subset <- filterEstimates()
    subset <- unique(subset[, c("targetId", "comparatorId", "oldOutcomeId", "targetEffectSize")])
    ncCount <- sum(subset$targetEffectSize == 1)
    pcCount <- sum(subset$targetEffectSize != 1)
    return(HTML(paste0("<strong>Figure S.1</strong> ", input$metric, " per stratum and database. Hover mouse over points for more information.")))
  })
  
  output$hoverOverview <- renderUI({
    data <- overviewData()
    if (is.null(data)) {
      return(NULL)
    } 
    hover <- input$plotHoverOverview
    
    point <- nearPoints(data, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    # Shiny was confused by log y scale used for some:
    if (input$metric %in% c("Mean precision (1/SE^2)", "Mean squared error (MSE)")) {
      y <- log10(hover$y)
    } else {
      y <- hover$y
    }
    top_pct <- (hover$domain$top - y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 251, "px; top:", top_px - 150, "px; width:500px;")
    analysis <- as.character(analysisRef$description[analysisRef$method == point$method & analysisRef$analysisId == point$analysisId])
    text <- paste0(sprintf("<b> Method: </b>%s<br/>", point$tidyMethod),
                   sprintf("<b> Analysis ID: </b>%s<br/>", point$analysisId),
                   sprintf("<b> Description: </b>%s<br/>", analysis),
                   sprintf("<b> %s: </b>%s<br/>", input$metric, point$metric))
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(style = style, p(HTML(text)))
    )
  })
  
})

