library(shiny)
library(DT)

mainColumns <- c("analysisDescription", 
                 "databaseId", 
                 "rr", 
                 "ci95lb",
                 "ci95ub",
                 "p")

mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>", 
                     "<span title=\"Data source\">Data source</span>", 
                     "<span title=\"Odds ratio \">OR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interva;\">LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval \">UB</span>", 
                     "<span title=\"Two-sided p-value\">P</span>")

shinyServer(function(input, output, session) {

  observe({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
    tcoSubset <- tcos[tcos$targetId == targetId & tcos$comparatorId == comparatorId, ]
    outcomes <- outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId %in% tcoSubset$outcomeId]
    updateSelectInput(session = session,
                      inputId = "outcome",
                      choices = unique(outcomes))
  })
  
  
  # subsetting =================================================================
  resultSubset <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
    analysisIds <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$analysis]
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
    # results <- results[order(results$analysisId), ]
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
      row$psStrategy <- gsub("^PS ", "", gsub(", .*$", "", cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == row$analysisId]))
      return(row)
    }
  })
  
  output$rowIsSelected <- reactive({
    return(!is.null(selectedRow()))
  })
  outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)
  
  
  # main table =================================================================
  output$mainTable <- renderDataTable({
    table <- resultSubset()
    if (is.null(table) || nrow(table) == 0) {
      return(NULL)
    }
    table$description <- cohortMethodAnalysis$description[match(table$analysisId, cohortMethodAnalysis$analysisId)]
    table <- table[, mainColumns]
    table$rr <- prettyHr(table$rr)
    table$ci95lb <- prettyHr(table$ci95lb)
    table$ci95ub <- prettyHr(table$ci95ub)
    table$p <- prettyHr(table$p)
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
  
  # forest plot ================================================================  
  forestPlotData <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
    databaseIds <- input$database
    analysisIds <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$analysis]
    results <- getForestPlotData(connection = connection,
                                 targetIds = targetId,
                                 comparatorIds = comparatorId,
                                 outcomeIds = outcomeId,
                                 databaseIds = databaseIds,
                                 analysisIds = analysisIds)
    return(results)
  })
  
  forestPlot <- reactive({
    results <- forestPlotData()
    plot <- plotForest(results)
    return(plot)
  })
  
  output$forestPlot <- renderPlot({
    return(forestPlot())
  }, res = 100)
  
  output$forestPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 6.</strong> Forest plot showing the per-analysis odds ratios (and 95 percent confidence intervals) 
      comparing <em>%s</em> to <em>%s</em> for the outcome of <em>%s</em>. Summary estimate is not reported where I2 > 0.4."
      return(HTML(sprintf(text, input$target, input$comparator, input$outcome)))
    }
  })
  
  
  # validation metrics =========================================================
  validationData <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
    databaseIds <- input$database
    results <- getValidationResults(connection = connection,
                                    targetId = targetId,
                                    comparatorId = comparatorId,
                                    outcomeId = outcomeId,
                                    databaseIds = databaseIds)
    return(results)
  })
  
  validationTable <- reactive({
    table <- validationData()
    table <- prepareValidationTable(table)
    return(table)
  })
  
  output$validationTable <- renderTable({
    return(validationTable())
  }, digits = 5)
  
  output$validationTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1.</strong> VALIDATION INFO HERE, include 2x2 table, sens spec results target (<em>%s</em>)
      comparator (<em>%s</em>)."
      return(HTML(sprintf(text, input$target, input$comparator)))
    } else {
      return(NULL)
    }
  })
  
  # counts, power tables =======================================================
  output$countsTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1a.</strong> Exposure by outcome contingency table, event odds in the target (<em>%s</em>) and
      comparator (<em>%s</em>) groups, as  well as the minimum detectable odds ratio (MDOR).
      Note that the odds do not account for any adjustment."
      return(HTML(sprintf(text, input$target, input$comparator)))
    } else {
      return(NULL)
    }
  })
  
  output$countsTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
        tables <- prepareCountsTable(row, cohortMethodAnalysis)
        countsTable <- tables[[1]]
        colnames(countsTable) <- c("Target exposures",
                                   "Comparator exposures",
                                   "Total")
        return(countsTable)
    }
  }, align = "r", rownames = TRUE)
  
  output$powerTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      tables <- prepareCountsTable(row, cohortMethodAnalysis)
      powerTable <- tables[[2]]
      colnames(powerTable) <- c("Target odds",
                                "Comparator odds",
                                "MDOR")
      return(powerTable)
    }
  }, align = "r")
  
  # md counts tables ===========================================================
  output$mdCountsTableCaption1 <- renderUI({
    text <- "<strong>Table 1.</strong> Exposure [target (<em>%s</em>) vs comparator (<em>%s</em>)] by outcome contingency table
             <i>without</i> QBA correction for outcome misclassification."
    return(HTML(sprintf(text, input$target, input$comparator)))
  })
  
  mdCountsTable1a <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdTarget]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdComparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$mdOutcome]
    analysisId <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$mdAnalysis]
    databaseId <- input$mdDatabase
    row <- cohortMethodResult[cohortMethodResult$analysisId == analysisId &
                                cohortMethodResult$targetId == targetId &
                                cohortMethodResult$comparatorId == comparatorId &
                                cohortMethodResult$outcomeId == outcomeId &
                                cohortMethodResult$databaseId == databaseId, ]
      tables <- prepareCountsTable(row, cohortMethodAnalysis)
      #browser()
      mdCountsTable1a <- tables[[1]]
      colnames(mdCountsTable1a) <- c(#"Outcomes",
                                     "Target exposures",
                                     "Comparator exposures",
                                     "Total")
      return(mdCountsTable1a)
  })
  
  output$mdCountsTable1a <- renderTable({
    return(mdCountsTable1a())
  }, align = "r", rownames = TRUE)
  
  mdCountsTable1b <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdTarget]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdComparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$mdOutcome]
    analysisId <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$mdAnalysis]
    databaseId <- input$mdDatabase
    row <- cohortMethodResult[cohortMethodResult$analysisId == analysisId &
                                cohortMethodResult$targetId == targetId &
                                cohortMethodResult$comparatorId == comparatorId &
                                cohortMethodResult$outcomeId == outcomeId &
                                cohortMethodResult$databaseId == databaseId, ]
    tables <- prepareCountsTable(row, cohortMethodAnalysis)
    mdCountsTable1b <- tables[[3]]
    colnames(mdCountsTable1b) <- c("Target odds",
                                   "Comparator odds",
                                   "OR",
                                   "LB",
                                   "UB")
    return(mdCountsTable1b)
  })
  
  output$mdCountsTable1b <- renderTable({
    return(mdCountsTable1b())
  }, align = "r")
  
  output$mdCountsTableCaption2 <- renderUI({
    text <- "<strong>Table 2.</strong> Exposure [target (<em>%s</em>) vs comparator (<em>%s</em>)] by outcome contingency table
             <i>with</i> simple QBA correction for outcome misclassification."
    return(HTML(sprintf(text, input$target, input$comparator)))
  })
  
  mdCountsTable2a <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdTarget]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdComparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$mdOutcome]
    analysisId <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$mdAnalysis]
    databaseId <- input$mdDatabase
    row <- cohortMethodResult[cohortMethodResult$analysisId == analysisId &
                                cohortMethodResult$targetId == targetId &
                                cohortMethodResult$comparatorId == comparatorId &
                                cohortMethodResult$outcomeId == outcomeId &
                                cohortMethodResult$databaseId == databaseId, ]
    tables <- prepareMdCountsTable(row,
                                   cohortMethodAnalysis,
                                   sens = input$sens,
                                   spec = input$spec)
    
    mdCountsTable2a <- tables[[1]]
    colnames(mdCountsTable2a) <- c("Target exposures",
                                   "Comparator exposures",
                                   "Total")
    return(mdCountsTable2a)
  })
  
  output$mdCountsTable2a <- renderTable({
    return(mdCountsTable2a())
  }, align = "r", rownames = TRUE)
  
  mdCountsTable2b <- reactive({
    targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdTarget]
    comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$mdComparator]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$mdOutcome]
    analysisId <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$mdAnalysis]
    databaseId <- input$mdDatabase
    row <- cohortMethodResult[cohortMethodResult$analysisId == analysisId &
                                cohortMethodResult$targetId == targetId &
                                cohortMethodResult$comparatorId == comparatorId &
                                cohortMethodResult$outcomeId == outcomeId &
                                cohortMethodResult$databaseId == databaseId, ]
    tables <- prepareMdCountsTable(row,
                                   cohortMethodAnalysis,
                                   sens = input$sens,
                                   spec = input$spec)
    
    mdCountsTable2b <- tables[[2]]
    colnames(mdCountsTable2b) <- c("Target odds",
                                   "Comparator odds",
                                   "OR",
                                   "LB",
                                   "UB")
    return(mdCountsTable2b)
  })
  
  output$mdCountsTable2b <- renderTable({
    return(mdCountsTable2b())
  }, align = "r")
  
  # md forest plot =============================================================
  mdForestPlotData <- reactive({
    mdCountsTable1b <- mdCountsTable1b()
    mdCountsTable2b <- mdCountsTable2b()
    mdOrTable <- rbind(mdCountsTable1b, mdCountsTable2b)
    return(mdOrTable)
  })

  mdForestPlot <- reactive({
    results <- mdForestPlotData()
    plot <- plotMdForest(results)
    return(plot)
  })

  output$mdForestPlot <- renderPlot({
    return(mdForestPlot())
  }, res = 100)
  
  
  # attrition ==================================================================
  attritionPlot <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      attrition <- getAttrition(connection = connection,
                                targetId = targetId,
                                comparatorId = comparatorId,
                                outcomeId = outcomeId,
                                databaseId = row$databaseId,
                                analysisId = row$analysisId)
      plot <- drawAttritionDiagram(attrition)
      return(plot)
    }
  })
  
  output$attritionPlot <- renderPlot({
    return(attritionPlot())
  })
  
  output$attritionPlotCaption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Figure 1.</strong> Attrition diagram, showing the Number of subjects in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after various stages in the analysis."
      return(HTML(sprintf(text, input$target, input$comparator)))
    }
  })
  
  
  # covariate balance ==========================================================
  balance <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      balance <- getCovariateBalance(connection = connection,
                                     targetId = targetId,
                                     comparatorId = comparatorId,
                                     databaseId = row$databaseId,
                                     analysisId = row$analysisId,
                                     outcomeId = outcomeId)
      return(balance)
    }
  })
  
  
  # table 1 ====================================================================
  output$table1Caption <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      text <- "<strong>Table 2.</strong> Select characteristics before and after propensity score adjustment, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
      return(HTML(sprintf(text, input$target, input$comparator)))
    }
  })
  
  output$table1Table <- renderDataTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      bal <- balance()
      if (nrow(bal) == 0) {
        return(NULL)
      }
      if (input$charType == "Pretty") {
        table1 <- prepareTable1(balance = bal,
                                beforeLabel = paste("Before PS adjustment"),
                                afterLabel = paste("After PS adjustment"))
        
        container <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 3, "Characteristic"),
              th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
              th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
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
      } else {
        table1 <- prepareRawTable1(bal)
        container <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 3, "Characteristic"),
              th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
              th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
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
                        searching = TRUE,
                        ordering = TRUE,
                        paging = TRUE,
                        bInfo = TRUE)
        table1 <- datatable(table1[3:nrow(table1), ],
                            options = options,
                            rownames = FALSE,
                            escape = FALSE,
                            container = container,
                            class = "stripe compact")
        table1 <- table1 %>% formatRound(c(2, 3, 5, 6), 1)
      }
      return(table1)
    }
  })
  
  
  # balance plot ===============================================================
  balancePlot <- reactive({
    bal <- balance()
    
    cmAnalysisId <- bal$cmAnalysisId[1]
    
    if (is.null(bal) || nrow(bal) == 0 || cmAnalysisId %in% c(1, 2, 5, 6, 9, 10)) {
      return(NULL)
    } else {
      plot <- plotCovariateBalanceScatterPlot(balance = bal,
                                              beforeLabel = "Before PS adjustment",
                                              afterLabel = "After PS adjustment")
      return(plot)
    }
  })
  
  output$balancePlot <- renderPlot({
    return(balancePlot())
  })
  
  output$balancePlotCaption <- renderUI({
    bal <- balance()
    if (is.null(bal) || nrow(bal) == 0) {
      return(NULL)
    } else {
      row <- selectedRow()
      text <- "<strong>Figure 3.</strong> Covariate balance before and after propensity score adjustment. Each dot represents
      the standardizes difference of means for a single covariate before and after propensity score adjustment on the propensity
      score. Move the mouse arrow over a dot for more details."
      return(HTML(sprintf(text)))
    }
  })
  
  output$hoverInfoBalanceScatter <- renderUI({
    bal <- balance()
    cmAnalysisId <- bal$cmAnalysisId[1]
    if (is.null(bal) || nrow(bal) == 0 || cmAnalysisId %in% c(1, 2, 5, 6, 9, 10)) {
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
                        "<b> Std. diff before PS adjustment: </b>", beforeMatchingStdDiff, "<br/>",
                        "<b> Std. diff after PS adjustment: </b>", afterMatchingStdDiff)))
        )
      )
    }
  })
  
  # ps model ===================================================================
  output$propensityModelTable <- renderDataTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      model <- getPropensityModel(connection = connection,
                                  targetId = targetId,
                                  comparatorId = comparatorId,
                                  databaseId = row$databaseId,
                                  analysisId = row$analysisId)
      table <- preparePropensityModelTable(model)
      options = list(columnDefs = list(list(className = 'dt-right',  targets = 0)),
                     pageLength = 15,
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
    }
  })
  
  # ps dist ====================================================================
  psDistPlot <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      ps <- getPs(connection = connection,
                  targetId = targetId,
                  comparatorId = comparatorId,
                  analysisId = row$analysisId,
                  databaseId = row$databaseId)
      if (is.null(ps) | nrow(ps) == 0) {
        return(NULL)
      } else {
        plot <- plotPs(ps, input$target, input$comparator)
        return(plot)
      }
    }
  })
  
  output$psDistPlot <- renderPlot({
    return(psDistPlot())
  })
  
  # contour plot ==============================================================
  output$contourPlotCaption <- renderUI({
    text <- "<strong>Contour plot.</strong> QBA-corrected odds ratio distribution across values of sensitivity and specificity. The data point where sensitivity = specificity = 1 represents to observed odds ration which is subjcet to phenptype error. Blue lines with associated data points display the QBA-corrected odds ration contour for the 25%ile, 50%ile, and 75%ile of the distribution."
    return(HTML(text))
  })
  
  contourPlotData <- reactive({
    contourData <- getContourData(connection = connection,
                                  contourData = gridSpaceResults,
                                  incidence = input$incidence,
                                  or = input$or)
    return(contourData)
  })
  
  contourPlot <- reactive({
    contourData <- contourPlotData()
    plot <- drawContourPlot(contourData)
    return(plot[[1]])
  })
  
  output$contourPlot <- renderPlot({
    return(contourPlot())
  }, res = 100)
  
  
  output$contourResultCaption <- renderUI({
    text <- "<strong>Contour Results</strong> DESCRIBE COLUMNS"
    return(HTML(text))
  })
  
  contourResults <- reactive({
    contourData <- contourPlotData()
    table <- drawContourPlot(contourData)[[2]]
    table <- prepareContourDataTable(table)
    return(table)
  })
  
  output$contourResults <- renderTable({
    return(contourResults())
  })
  
  
})