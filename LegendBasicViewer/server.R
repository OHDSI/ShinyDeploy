library(shiny)
library(DT)

mainColumns <- c("description",
                 "databaseId",
                 "rr",
                 "ci95Lb",
                 "ci95Ub",
                 "p",
                 "calibratedRr",
                 "calibratedCi95Lb",
                 "calibratedCi95Ub",
                 "calibratedP")

mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>",
                     "<span title=\"Data source\">Data source</span>",
                     "<span title=\"Hazard ratio (uncalibrated)\">HR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>",
                     "<span title=\"Two-sided p-value (uncalibrated)\">P</span>",
                     "<span title=\"Hazard ratio (calibrated)\">Cal.HR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>",
                     "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")

shinyServer(function(input, output, session) {

  connection <- DatabaseConnector::connect(connectionDetails)

  session$onSessionEnded(function() {
    writeLines("Closing connection")
    DatabaseConnector::disconnect(connection)
  })

  observe({
    indicationId <- input$indication
    updateSelectInput(session = session,
                      inputId = "exposureGroup",
                      choices = unique(exposureGroups$exposureGroup[exposureGroups$indicationId == indicationId]))
  })

  observe({
    indicationId <- input$indication
    exposureGroup <- input$exposureGroup
    includeCombis <- input$includeCombis
    filteredExposures <- exposures[exposures$indicationId == indicationId, ]
    if (!includeCombis) {
      filteredExposures <- filteredExposures[filteredExposures$combi == 0, ]
    }
    # filteredOutcomes <- outcomes[outcomes$indicationId == indicationId, ]
    filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    updateSelectInput(session = session,
                      inputId = "target",
                      choices = unique(filteredExposures$exposureName))
    updateSelectInput(session = session,
                      inputId = "comparator",
                      choices = unique(filteredExposures$exposureName))
  })
  
  resultSubset <- reactive({
    targetId <- unique(exposures$exposureId[exposures$exposureName == input$target &
                                              exposures$exposureGroup == input$exposureGroup])
    comparatorId <- unique(exposures$exposureId[exposures$exposureName == input$comparator &
                                                  exposures$exposureGroup == input$exposureGroup])
    if (length(targetId) == 0 || length(comparatorId) == 0) {
      return(NULL)
    }
    outcomeId <- unique(outcomes$outcomeId[outcomes$outcomeName == input$outcome])
    analysisIds <- analyses$analysisId[analyses$description %in% input$analysis]
    databaseIds <- input$database
    if (length(analysisIds) == 0) {
      analysisIds <- -1
    }
    if (length(databaseIds) == 0) {
      databaseIds <- "none"
    }
    results <- getMainResults(connection = connection,
                              targetIds = targetId,
                              comparatorIds = comparatorId,
                              outcomeIds = outcomeId,
                              databaseIds = databaseIds,
                              analysisIds = analysisIds)
    results <- merge(results, analyses)
   return(results)
  })

  selectedRow <- reactive({
    idx <- input$mainTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      subset <- resultSubset()
      if (nrow(subset) == 0) {
        return(NULL)
      }
      row <- subset[idx, ]
      row$psStrategy <- gsub("^PS ", "", gsub(", .*$", "", analyses$description[analyses$analysisId == row$analysisId]))
      return(row)
    }
  })

  output$rowIsSelected <- reactive({
    return(!is.null(selectedRow()))
  })
  outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

  output$isMetaAnalysis <- reactive({
    row <- selectedRow()
    isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
    if (isMetaAnalysis) {
      hideTab("detailsTabsetPanel", "Attrition")
      hideTab("detailsTabsetPanel", "Population characteristics")
      # hideTab("detailsTabsetPanel", "Propensity scores")
      # hideTab("detailsTabsetPanel", "Covariate balance")
      hideTab("detailsTabsetPanel", "Kaplan-Meier")
      showTab("detailsTabsetPanel", "Forest plot")
    } else {
      hideTab("detailsTabsetPanel", "Forest plot")
      showTab("detailsTabsetPanel", "Attrition")
      showTab("detailsTabsetPanel", "Population characteristics")
      # showTab("detailsTabsetPanel", "Propensity scores")
      # showTab("detailsTabsetPanel", "Covariate balance")
      showTab("detailsTabsetPanel", "Kaplan-Meier") 
    }
    return(isMetaAnalysis)
  })
  outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
  
  balance <- reactive({
     row <- selectedRow()
     if (is.null(row)) {
       return(NULL)
     } else {
       analysisId <- row$analysisId
       if (analysisId %in% c(1, 3)) {
         # Only computed balance for ITT windows
         analysisId <- analysisId + 1
       }
       writeLines("Fetching covariate balance")
       balance <- getCovariateBalance(connection = connection,
                                      targetId = row$targetId ,
                                      comparatorId = row$comparatorId,
                                      databaseId = row$databaseId,
                                      analysisId = analysisId)
       return(balance)
     }
  })

  output$mainTable <- renderDataTable({
    table <- resultSubset()
    if (is.null(table) || nrow(table) == 0) {
      return(NULL)
    }
    # table <- merge(table, analyses)
    table <- table[, mainColumns]
    table$rr <- prettyHr(table$rr)
    table$ci95Lb <- prettyHr(table$ci95Lb)
    table$ci95Ub <- prettyHr(table$ci95Ub)
    table$p <- prettyHr(table$p)
    table$calibratedRr <- prettyHr(table$calibratedRr)
    table$calibratedCi95Lb <- prettyHr(table$calibratedCi95Lb)
    table$calibratedCi95Ub <- prettyHr(table$calibratedCi95Ub)
    table$calibratedP <- prettyHr(table$calibratedP)
    colnames(table) <- mainColumnNames
    options = list(pageLength = 15,
                   searching = FALSE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE)
    selection = list(mode = "single", target = "row")
    table <- datatable(table,
                       options = options,
                       selection = selection,
                       rownames = FALSE,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    return(table)
  })

  output$powerTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after %s, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
      return(HTML(sprintf(text, input$target, input$comparator, row$psStrategy)))
    } else {
      return(NULL)
    }
  })

  output$powerTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      if (row$databaseId %in% metaAnalysisDbIds) {
        results <- getMainResults(connection = connection,
                                  targetIds = row$targetId,
                                  comparatorIds = row$comparatorId,
                                  outcomeIds = row$outcomeId,
                                  analysisIds = row$analysisId)
        table <- preparePowerTable(results, analyses, showDatabaseId = TRUE)
        table$description <- NULL
        table$databaseId[table$databaseId == "Meta-analysis"] <- "Summary"
        colnames(table) <- c("Source", 
                             "Target subjects",
                             "Comparator subjects",
                             "Target years",
                             "Comparator years",
                             "Target events",
                             "Comparator events",
                             "Target IR (per 1,000 PY)",
                             "Comparator IR (per 1,000 PY)",
                             "MDRR")
        # table$i2 <- c(rep("", nrow(table) - 1), sprintf("%.2f", as.numeric(row$i2)))
      } else {
        table <- preparePowerTable(row, analyses)
        table$description <- NULL
        colnames(table) <- c("Target subjects",
                             "Comparator subjects",
                             "Target years",
                             "Comparator years",
                             "Target events",
                             "Comparator events",
                             "Target IR (per 1,000 PY)",
                             "Comparator IR (per 1,000 PY)",
                             "MDRR")
      }
      return(table)
    }
  })

  output$timeAtRiskTableCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after %s."
      return(HTML(sprintf(text, input$target, input$comparator, row$psStrategy)))
    } 
  })

  output$timeAtRiskTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      followUpDist <- getCmFollowUpDist(connection = connection,
                                        targetId = row$targetId,
                                        comparatorId = row$comparatorId,
                                        outcomeId = row$outcomeId,
                                        databaseId = row$databaseId,
                                        analysisId = row$analysisId)
      table <- prepareFollowUpDistTable(followUpDist)
      return(table)
    }
  })

  output$attritionPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      attrition <- getAttrition(connection = connection,
                                targetId = row$targetId,
                                comparatorId = row$comparatorId,
                                outcomeId = row$outcomeId,
                                databaseId = row$databaseId,
                                analysisId = row$analysisId)
      plot <- drawAttritionDiagram(attrition)
      return(plot)
    }
  })

  output$attritionPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 1.</strong> Attrition diagram, showing the Number of subjectsin the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after various stages in the analysis."
      return(HTML(sprintf(text, input$target, input$comparator)))
    }
  })

  output$table1Caption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Table 2.</strong> Select characteristics before and after %s, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
      return(HTML(sprintf(text, row$psStrategy, input$target, input$comparator)))
    }
  })

  output$table1Table <- renderDataTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      chars <- getCovariateBalance(connection = connection,
                                   targetId = row$targetId,
                                   comparatorId = row$comparatorId,
                                   outcomeId = row$outcomeId,
                                   databaseId = row$databaseId,
                                   analysisId = row$analysisId)
      table1 <- prepareTable1(balance = chars,
                             beforeLabel = paste("Before" , row$psStrategy),
                             afterLabel = paste("After" , row$psStrategy))

      container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 3, "Characteristic"),
            th(colspan = 3, class = "dt-center", paste("Before", row$psStrategy)),
            th(colspan = 3, class = "dt-center", paste("After", row$psStrategy))
          ),
          tr(
            lapply(table1[1, 2:ncol(table1)], th)
          ),
          tr(
            lapply(table1[2, 2:ncol(table1)], th)
          )
        )
      ))
      options <- list(columnDefs = list(list(className = 'dt-right',  targets = 1:6)),
                      searching = FALSE,
                      ordering = FALSE,
                      paging = FALSE,
                      bInfo = FALSE)
      table1 <- datatable(table1[3:nrow(table1), ],
                          options = options,
                          rownames = FALSE,
                          escape = FALSE,
                          container = container,
                          class = "stripe nowrap compact")
      return(table1)
    }
  })

  output$psDistPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      if (row$databaseId %in% metaAnalysisDbIds) {
        ps <- getPs(connection = connection,
                    targetIds = row$targetId,
                    comparatorIds = row$comparatorId)
      } else {
        ps <- getPs(connection = connection,
                    targetIds = row$targetId,
                    comparatorIds = row$comparatorId,
                    databaseId = row$databaseId)
      }
      plot <- plotPs(ps, input$target, input$comparator)
      return(plot)
    }
  })

  output$balancePlot <- renderPlot({
    bal <- balance()
    if (is.null(bal)) {
      return(NULL)
    } else {
      row <- selectedRow()
      writeLines("Plotting covariate balance")
      plot <- plotCovariateBalanceScatterPlot(balance = bal,
                                              beforeLabel = paste("Before", row$psStrategy),
                                              afterLabel = paste("After", row$psStrategy),
                                              showCovariateCountLabel = TRUE,
                                              showMaxLabel = TRUE)
      return(plot)
    }
  })

  output$balancePlotCaption <- renderUI({
    bal <- balance()
    if (is.null(bal)) {
      return(NULL)
    } else {
      row <- selectedRow()
      text <- "<strong>Figure 3.</strong> Covariate balance before and after %s. Each dot represents
      the standardized difference of means for a single covariate before and after %s on the propensity
      score. Move the mouse arrow over a dot for more details."
      return(HTML(sprintf(text, row$psStrategy, row$psStrategy)))
    }
  })

  output$hoverInfoBalanceScatter <- renderUI({
    bal <- balance()
    if (is.null(bal)) {
      return(NULL)
    } else {
      row <- selectedRow()
      hover <- input$plotHoverBalanceScatter
      point <- nearPoints(bal, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:",
                      left_px - 251,
                      "px; top:",
                      top_px - 150,
                      "px; width:500px;")
      beforeMatchingStdDiff <- formatC(point$beforeMatchingStdDiff, digits = 2, format = "f")
      afterMatchingStdDiff <- formatC(point$afterMatchingStdDiff, digits = 2, format = "f")
      div(
        style = "position: relative; width: 0; height: 0",
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Covariate: </b>", point$covariateName, "<br/>",
                        "<b> Std. diff before ",tolower(row$psStrategy),": </b>", beforeMatchingStdDiff, "<br/>",
                        "<b> Std. diff after ",tolower(row$psStrategy),": </b>", afterMatchingStdDiff)))
        )
      )
    }
  })

  output$systematicErrorPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      controlResults <- getControlResults(connection = connection,
                                          targetId = row$targetId,
                                          comparatorId = row$comparatorId,
                                          analysisId = row$analysisId,
                                          databaseId = row$databaseId)

      plot <- plotScatter(controlResults)
      return(plot)
    }
  })

  output$kaplanMeierPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      km <- getKaplanMeier(connection = connection,
                                   targetId = row$targetId,
                                   comparatorId = row$comparatorId,
                                   outcomeId = row$outcomeId,
                                   databaseId = row$databaseId,
                                   analysisId = row$analysisId)
      plot <- plotKaplanMeier(kaplanMeier = km,
                              targetName = input$target,
                              comparatorName = input$comparator)
      return(plot)
    }
  }, res = 100)

  output$kaplanMeierPlotPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 5.</strong> Kaplan Meier plot, showing survival as a function of time. This plot
      is adjusted for the propensity score %s: The target curve (<em>%s</em>) shows the actual observed survival. The
      comparator curve (<em>%s</em>) applies reweighting to approximate the counterfactual of what the target survival
      would look like had the target cohort been exposed to the comparator instead. The shaded area denotes
      the 95 percent confidence interval."
      return(HTML(sprintf(text, row$psStrategy, input$target, input$comparator)))
    }
  })

  output$subgroupTableCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Table 4.</strong> Subgroup interactions. For each subgroup, the number of subject within the subroup
      in the target (<em>%s</em>) and comparator (<em>%s</em>) cohorts are provided, as well as the hazard ratio ratio (HRR)
      with 95 percent confidence interval and p-value (uncalibrated and calibrated) for interaction of the main effect with
      the subgroup."
      return(HTML(sprintf(text, input$target, input$comparator)))
    }
  })
  
  output$forestPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      results <- getMainResults(connection = connection,
                                targetIds = row$targetId,
                                comparatorIds = row$comparatorId,
                                outcomeIds = row$outcomeId,
                                analysisIds = row$analysisId)
      plot <- plotForest(results)
      return(plot)
    }
  })
  
  output$forestPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 6.</strong> Forest plot showing the per-database and summary hazard ratios (and 95 percent confidence
      intervals) comparing %s to %s for the outcome of %s, using %s. Estimates are shown both before and after empirical 
      calibration. The I2 is computed on the uncalibrated estimates."
      return(HTML(sprintf(text, input$target, input$comparator, input$outcome, row$psStrategy)))
    }
  })
  
  output$balanceSummaryPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      analysisId <- row$analysisId
      if (analysisId %in% c(1, 3)) {
        # Only computed balance for ITT windows
        analysisId <- analysisId + 1
      }
      balanceSummary <- getCovariateBalanceSummary(connection = connection,
                                                   targetId = row$targetId,
                                                   comparatorId = row$comparatorId,
                                                   analysisId = analysisId)
      plot <- plotCovariateBalanceSummary(balanceSummary,
                                          threshold = 0.1,
                                          beforeLabel = paste("Before", row$psStrategy),
                                          afterLabel = paste("After", row$psStrategy)) 
      return(plot)
    }
  }, res = 100)
  
  output$balanceSummaryPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 7.</strong> Covariate balance before and after %s. The y axis represents
      the standardized difference of mean before and after %s on the propensity
      score. The whiskers show the minimum and maximum values across covariates. The box represents the 
      interquartile range, and the middle line represents the median. The dashed lines indicate a standardized
      difference of 0.1."
      return(HTML(sprintf(text, row$psStrategy, row$psStrategy)))
    }
  })
  
  output$systematicErrorSummaryPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      negativeControls <- getNegativeControlEstimates(connection = connection,
                                                      targetId = row$targetId,
                                                      comparatorId = row$comparatorId,
                                                      analysisId =  row$analysisId)
      plot <- plotEmpiricalNulls(negativeControls) 
      return(plot)
    }
  }, res = 100)
  
  
})
