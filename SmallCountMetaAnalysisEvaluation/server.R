library(shiny)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Fixed effects ---------------------------------------------------
  
  pivotDataFixed <- function(simParam, subset, dropIfUnique = TRUE) {
    if (dropIfUnique && length(unique(subset[, simParam])) == 1) {
      return(NULL)
    } else {
      temp <- subset
      maxValue <- max(subset[simParam])
      temp$parameterValue <- subset[, simParam]
      temp$jitter <- temp$parameterValue + runif(nrow(subset), -0.02 * maxValue, 0.02 * maxValue)
      temp$simParam <- simParam
      temp[simParamsFixed] <- NULL
      return(temp)
    }
  }
  
  filteredResultsFixed <- reactive({
    subset <- resultsFixed
    subset <- subset[subset$metric %in% input$metricFixed, ]
    subset <- subset[subset$type %in% input$typeFixed, ]
    for (simParam in simParamsFixed) {
      subset <- subset[subset[, simParam] %in% as.numeric(input[[paste0(simParam, "Fixed")]]), ]
    }
    return(subset)
  })
  
  filteredPivotedResultsFixed <- reactive({
    subset <- filteredResultsFixed()
    vizData <- lapply(simParamsFixed, pivotDataFixed, subset = subset)
    vizData <- do.call(rbind, vizData)
    return(vizData)
  })
  
  filteredViolinPivotedResultsFixed <- reactive({
    subset <- filteredResultsFixed()
    vizData <- pivotDataFixed(input$simParamFixedRadioButton, subset, dropIfUnique = FALSE)
    return(vizData)
  })
  
  getReferenceValues <- function(metrics) {
    ref <- data.frame()
    if ("bias" %in% metrics) {
      ref <- rbind(ref, data.frame(value = 0,
                                   metric = "bias")) 
    }
    if ("coverage" %in% metrics) {
      ref <- rbind(ref, data.frame(value = 0.95,
                                   metric = "coverage")) 
    }
    if ("mse" %in% metrics) {
      ref <- rbind(ref, data.frame(value = 0,
                                   metric = "mse")) 
    }
    if ("nonEstimable" %in% metrics) {
      ref <- rbind(ref, data.frame(value = 0,
                                   metric = "nonEstimable")) 
    }
    return(ref)
  }
  
  output$mainPlotFixed  <- renderPlot({
    subset <- filteredPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    } else {
      ref <- getReferenceValues(subset$metric)
      subset$type <- gsub(" ", "\n", subset$type)
      plot <- ggplot2::ggplot(subset, ggplot2::aes(x = jitter, y = value, group = type, color = type)) 
      if (nrow(ref) > 0) {
        plot <- plot + geom_hline(aes(yintercept = value), data = ref, linetype = "dashed")
      }
      plot <- plot + ggplot2::geom_point(alpha = 0.4) +
        ggplot2::facet_grid(metric~simParam, scales = "free", switch = "both") +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank(),
                       strip.placement = "outside",
                       strip.background = ggplot2::element_blank())
      return(plot)
    }
  },
  res = 125,
  height = 800)
  
  output$mainViolinPlotFixed  <- renderPlot({
    subset <- filteredViolinPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    } else {
      ref <- getReferenceValues(subset$metric)
      subset$type <- gsub(" ", "\n", subset$type)
      # subset <- subset[subset$simParam == input$simParamFixedRadioButton, ]
      plot <- ggplot(subset, aes(x = factor(parameterValue), y = value, fill = type)) 
      if (nrow(ref) > 0) {
        plot <- plot + geom_hline(aes(yintercept = value), data = ref, linetype = "dashed")
      }
      plot <- plot + geom_violin(position = position_dodge(0.9), scale = "width", alpha = 0.4) +
        facet_grid(metric~., scales = "free", switch = "both") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              axis.title = element_blank(),
              strip.placement = "outside",
              strip.background = element_blank())
      return(plot)
    }
  },
  res = 125,
  height = 800)
  
  output$hoverInfoPlotFixed <- renderUI({
    subset <- filteredPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    } 
    hover <- input$plotHoverMainPlotFixed
    point <- nearPoints(subset, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 361, "px; top:", top_px - 150, "px; width:250px;")
    
    # Unpivot:
    unpivotedRow <- resultsFixed[resultsFixed[, point$simParam] == point$parameterValue & 
                                   resultsFixed$type == point$type & 
                                   resultsFixed$metric == point$metric & 
                                   resultsFixed$value == point$value, ]
    unpivotedRow <- unpivotedRow[1, ]
    allMetrics <- merge(resultsFixed, unpivotedRow[, c(simParamsFixed, "type")])
    
    lines <- sprintf("<b> Type: </b>%s", point$type)
    lines <- c(lines, "")
    lines <- c(lines, sprintf("<b> %s: </b>%s", simParamsFixed, unpivotedRow[, simParamsFixed]))
    lines <- c(lines, "")
    lines <- c(lines, sprintf("<b> %s: </b>%.2f", allMetrics$metric, allMetrics$value))
    
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(
        style = style,
        p(HTML(paste(lines, collapse = "<br/>")))))
  })
  
  output$mainCaptionFixed <- renderUI({
    subset <- filteredPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    count <- sum(subset$type == subset$type[1] & subset$metric == subset$metric[1]) 
    HTML(sprintf("<strong>Figure S1.2. </strong>Each dot represents one of the %s selected simulation scenarios. The y-axes represent the various metrics 
    as estimated over 1,000 iterations per scenario, and the x-axes represent the various simulation parameters. Color indicates the various tested
                 meta-analysis algorithms.", count))
  })
  
  output$mainViolinCaptionFixed <- renderUI({
    subset <- filteredPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    count <- sum(subset$type == subset$type[1] & subset$metric == subset$metric[1]) 
    HTML(sprintf("<strong>Figure S1.1. </strong>Violin plots showing the performance accross the %s selected simulation scenarios. The y-axes represent the various metrics 
    as estimated over 1,000 iterations per scenario, and the x-axes represent %s. Color indicates the various tested
                 meta-analysis algorithms.", count, input$simParamFixedRadioButton))
  })
  
  output$rankPlotFixed  <- renderPlot({
    subset <- filteredResultsFixed()
    subset <- subset[!grepl("bias", subset$metric), ]
    
    rankMethods <- function(subgroup, descending = TRUE) {
      if (descending) {
        subgroup$rank <- order(-subgroup$value)
      } else {
        subgroup$rank <- order(subgroup$value)
      }
      return(subgroup)
    }
    
    processMetric <- function(metricSubset) {
      metric <- metricSubset$metric[1]
      descending <- grepl("precision", metric)
      if (grepl("coverage", metric)) {
        metricSubset$value <- abs(0.95 - metricSubset$value)
      }
      subgroups <- split(metricSubset, apply(metricSubset[, c(simParamsFixed, "metric")],1,paste,collapse=" "))
      metricSubset <- lapply(subgroups, rankMethods, descending = descending)
      metricSubset <- do.call(rbind, metricSubset)  
      return(metricSubset)
    }
    
    rankedSubset <- lapply(split(subset, subset$metric), processMetric)
    rankedSubset <- do.call(rbind, rankedSubset)
    rankedSubset$type <- gsub(" ", "\n", rankedSubset$type)
    plot <- ggplot2::ggplot(rankedSubset, ggplot2::aes(x = rank)) +
      ggplot2::geom_histogram(binwidth = 1, color = rgb(0, 0, 0.8, alpha = 0), fill = rgb(0, 0, 0.8), alpha = 0.6) +
      ggplot2::scale_x_continuous("Rank (lower is better)", breaks = min(rankedSubset$rank):max(rankedSubset$rank)) +
      ggplot2::scale_y_continuous("Count") +
      ggplot2::facet_grid(type~metric) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank())
    
    return(plot)
  },
  res = 110,
  height = 800)
  
  output$rankCaptionFixed <- renderUI({
    subset <- filteredPivotedResultsFixed()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    text <- "<strong>Figure S1.3. </strong>Histograms of algorithm ranks. Each bar represents the number of simulation scenarios where the algorithm on the 
    right achieved that rank on the metric at the top, compared to the other selected algorithms."
    if (any(grepl("coverage", subset$metric))) {
      text <- paste(text, "For coverage, algorithms were ranked by absolute difference between the estimated coverage and 95 percent.") 
    }
    HTML(text)
  })
  
  # Random Fx ------------------------------------------------------------------
  
  pivotDataRandom <- function(simParam, subset, dropIfUnique = TRUE) {
    if (dropIfUnique && length(unique(subset[, simParam])) == 1) {
      return(NULL)
    } else {
      temp <- subset
      maxValue <- max(subset[simParam])
      temp$parameterValue <- subset[, simParam]
      temp$jitter <- temp$parameterValue + runif(nrow(subset), -0.02 * maxValue, 0.02 * maxValue)
      temp$simParam <- simParam
      temp[simParamsRandom] <- NULL
      return(temp)
    }
  }
  
  filteredResultsRandom <- reactive({
    subset <- resultsRandom
    subset <- subset[subset$metric %in% input$metricRandom, ]
    subset <- subset[subset$type %in% input$typeRandom, ]
    for (simParam in simParamsRandom) {
      subset <- subset[subset[, simParam] %in% as.numeric(input[[paste0(simParam, "Random")]]), ]
    }
    return(subset)
  })
  
  filteredPivotedResultsRandom <- reactive({
    subset <- filteredResultsRandom()
    vizData <- lapply(simParamsRandom, pivotDataRandom, subset = subset)
    vizData <- do.call(rbind, vizData)
    return(vizData)
  })
  
  filteredViolinPivotedResultsRandom <- reactive({
    subset <- filteredResultsRandom()
    vizData <- pivotDataRandom(input$simParamRandomRadioButton, subset, dropIfUnique = FALSE)
    return(vizData)
  })
  
  output$mainPlotRandom  <- renderPlot({
    subset <- filteredPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    } else {
      subset$type <- gsub(" ", "\n", subset$type)
      plot <- ggplot2::ggplot(subset, ggplot2::aes(x = jitter, y = value, group = type, color = type)) +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::facet_grid(metric~simParam, scales = "free", switch = "both") +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank(),
                       strip.placement = "outside",
                       strip.background = ggplot2::element_blank())
      return(plot)
    }
  },
  res = 150,
  height = 800)
  
  output$mainViolinPlotRandom  <- renderPlot({
    subset <- filteredViolinPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    } else {
      ref <- getReferenceValues(subset$metric)
      subset$type <- gsub(" ", "\n", subset$type)
      plot <- ggplot(subset, aes(x = factor(parameterValue), y = value, fill = type)) 
      if (nrow(ref) > 0) {
        plot <- plot + geom_hline(aes(yintercept = value), data = ref, linetype = "dashed")
      }
      plot <- plot + geom_violin(position = position_dodge(0.9), scale = "width", alpha = 0.4) +
        facet_grid(metric~., scales = "free", switch = "both") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              axis.title = element_blank(),
              strip.placement = "outside",
              strip.background = element_blank())
      return(plot)
    }
  },
  res = 125,
  height = 800)
  
  output$hoverInfoPlotRandom <- renderUI({
    subset <- filteredPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    } 
    hover <- input$plotHoverMainPlotRandom
    point <- nearPoints(subset, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 361, "px; top:", top_px - 150, "px; width:250px;")
    
    # Unpivot:
    unpivotedRow <- resultsRandom[resultsRandom[, point$simParam] == point$parameterValue & 
                                    resultsRandom$type == point$type & 
                                    resultsRandom$metric == point$metric & 
                                    resultsRandom$value == point$value, ]
    unpivotedRow <- unpivotedRow[1, ]
    allMetrics <- merge(resultsRandom, unpivotedRow[, c(simParamsRandom, "type")])
    
    lines <- sprintf("<b> Type: </b>%s", point$type)
    lines <- c(lines, "")
    lines <- c(lines, sprintf("<b> %s: </b>%s", simParamsRandom, unpivotedRow[, simParamsRandom]))
    lines <- c(lines, "")
    lines <- c(lines, sprintf("<b> %s: </b>%.2f", allMetrics$metric, allMetrics$value))
    
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(
        style = style,
        p(HTML(paste(lines, collapse = "<br/>")))))
  })
  
  output$mainCaptionRandom <- renderUI({
    subset <- filteredPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    count <- sum(subset$type == subset$type[1] & subset$metric == subset$metric[1]) 
    HTML(sprintf("<strong>Figure S2.2. </strong>Each dot represents one of the %s selected simulation scenarios. The y-axes represent the various metrics 
    as estimated over 1,000 iterations per scenario, and the x-axes represent the various simulation parameters. Color indicates the various tested
                 meta-analysis algorithms.", count))
  })
  
  output$mainViolinCaptionRandom <- renderUI({
    subset <- filteredPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    count <- sum(subset$type == subset$type[1] & subset$metric == subset$metric[1]) 
    HTML(sprintf("<strong>Figure S2.1. </strong>Violin plots showing the performance accross the %s selected simulation scenarios. The y-axes represent the various metrics 
    as estimated over 1,000 iterations per scenario, and the x-axes represent %s. Color indicates the various tested
                 meta-analysis algorithms.", count, input$simParamRandomRadioButton))
  })
  
  output$rankPlotRandom  <- renderPlot({
    subset <- filteredResultsRandom()
    subset <- subset[!grepl("bias", subset$metric), ]
    
    rankMethods <- function(subgroup, descending = TRUE) {
      if (descending) {
        subgroup$rank <- order(-subgroup$value)
      } else {
        subgroup$rank <- order(subgroup$value)
      }
      return(subgroup)
    }
    
    processMetric <- function(metricSubset) {
      metric <- metricSubset$metric[1]
      descending <- grepl("precision", metric)
      if (grepl("coverage", metric)) {
        metricSubset$value <- abs(0.95 - metricSubset$value)
      }
      subgroups <- split(metricSubset, apply(metricSubset[, c(simParamsRandom, "metric")],1,paste,collapse=" "))
      metricSubset <- lapply(subgroups, rankMethods, descending = descending)
      metricSubset <- do.call(rbind, metricSubset)  
      return(metricSubset)
    }
    
    rankedSubset <- lapply(split(subset, subset$metric), processMetric)
    rankedSubset <- do.call(rbind, rankedSubset)
    rankedSubset$type <- gsub(" ", "\n", rankedSubset$type)
    plot <- ggplot2::ggplot(rankedSubset, ggplot2::aes(x = rank)) +
      ggplot2::geom_histogram(binwidth = 1, color = rgb(0, 0, 0.8, alpha = 0), fill = rgb(0, 0, 0.8), alpha = 0.6) +
      ggplot2::scale_x_continuous("Rank (lower is better)", breaks = min(rankedSubset$rank):max(rankedSubset$rank)) +
      ggplot2::scale_y_continuous("Count") +
      ggplot2::facet_grid(type~metric) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank())
    
    return(plot)
  },
  res = 110,
  height = 800)
  
  output$rankCaptionRandom <- renderUI({
    subset <- filteredPivotedResultsRandom()
    if (nrow(subset) == 0) {
      return(NULL)
    }
    text <- "<strong>Figure S2.3. </strong>Histograms of algorithm ranks. Each bar represents the number of simulation scenarios where the algorithm on the 
    right achieved that rank on the metric at the top, compared to the other selected algorithms."
    if (any(grepl("coverage", subset$metric))) {
      text <- paste(text, "For coverage, algorithms were ranked by absolute difference between the estimated coverage and 95 percent.") 
    }
    HTML(text)
  })  
})
