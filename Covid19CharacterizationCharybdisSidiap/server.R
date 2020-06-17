library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)
source("PlotsAndTables.R")

truncateStringDef <- function(columns, maxChars) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > %s ?\n
        '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;\n
     }", maxChars, maxChars))
  )
}

minCellCountDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }")
  )
}

minCellPercentDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return (100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
    return '<' + Math.abs(100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
  }")
  )
}

minCellRealDef <- function(columns, digits = 1) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }", digits, digits))
  )
}

styleAbsColorBar <- function(maxValue, colorPositive, colorNegative, angle = 90) {
  JS(sprintf("isNaN(parseFloat(value))? '' : 'linear-gradient(%fdeg, transparent ' + (%f - Math.abs(value))/%f * 100 + '%%, ' + (value > 0 ? '%s ' : '%s ') + (%f - Math.abs(value))/%f * 100 + '%%)'", 
             angle, maxValue, maxValue, colorPositive, colorNegative, maxValue, maxValue))
}

shinyServer(function(input, output, session) {
  
  cohortIdList <- reactive({
    return(unlist(cohortXref[cohortXref$targetId %in% targetCohortIdList() & cohortXref$strataId %in% strataCohortIdList(),c("cohortId")]))
  })
  
  targetCohortIdList <- reactive({
    return(targetCohort$targetId[targetCohort$targetName %in% input$targetCohortList])
  })
  
  strataCohortIdList <- reactive({
    return(strataCohort$strataId[strataCohort$strataName %in% input$strataCohortList])
  })
  
  targetCohortName <- reactive({
    cohort <- cohortXref[cohortXref$cohortId == cohortId(), c("targetName", "strataId", "strataName")]
    fullCohortName <- cohort$targetName[1]
    if (cohort$strataId[1] > 0) {
      fullCohortName <- paste(fullCohortName, cohort$strataName[1])
    }
    return(fullCohortName)
  })
  
  comparatorCohortName <- reactive({
    cohort <- cohortXref[cohortXref$cohortId == comparatorCohortId(), c("targetName", "strataId", "strataName")]
    fullCohortName <- cohort$targetName[1]
    if (cohort$strataId[1] > 0) {
      fullCohortName <- paste(fullCohortName, cohort$strataName[1])
    }
    return(fullCohortName)
  })
  
  cohortId <- reactive({
    return(unlist(cohortXref[cohortXref$targetId == targetId() & cohortXref$strataName == input$strataCohort,c("cohortId")]))
  })
  
  targetId <- reactive({
    return(targetCohort$targetId[targetCohort$targetName == input$targetCohort])
  })

  comparatorCohortId <- reactive({
    return(unlist(cohortXref[cohortXref$targetId == comparatorTargetId() & cohortXref$strataName == input$comparatorStrataCohort,c("cohortId")]))
  })
  
  comparatorTargetId <- reactive({
    return(targetCohort$targetId[targetCohort$targetName == input$comparatorCohort])
  })
  
  windowId <- reactive({
    return(timeWindow[timeWindow$name %in% input$timeWindowFilter,c("windowId")])
  })
  
  covariateAnalysisId <- reactive({
    return(domain[domain$name %in% input$domainFilter,c("covariateAnalysisId")])
  })
  
  output$cohortCountsTable <- renderDataTable({
    data <- cohortCount[cohortCount$databaseId %in% input$databases & cohortCount$cohortId %in% cohortIdList(), ]
    if (nrow(data) == 0) {
      return(NULL)
    }
    databaseIds <- unique(data$databaseId)
    table <- data[data$databaseId == databaseIds[1], c("cohortId", "cohortSubjects")]
    colnames(table)[2] <- paste(colnames(table)[2], databaseIds[1], sep = "_")
    if (length(databaseIds) > 1) {
      for (i in 2:length(databaseIds)) {
        temp <- data[data$databaseId == databaseIds[i], c("cohortId", "cohortSubjects")]
        colnames(temp)[2] <- paste(colnames(temp)[2], databaseIds[i], sep = "_")
        table <- merge(table, temp, all = TRUE)
      }
    }
    table <- dplyr::inner_join(cohortXref, table, by="cohortId")
    table$cohortId <- NULL
    table$targetId <- NULL
    table$strataId <- NULL
    table$cohortType <- NULL
    table <- table[order(table$targetName),]
    
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Cohort'),
          th(rowspan = 2, 'Strata'),
          lapply(databaseIds, th, colspan = 1, class = "dt-center")
        ),
        tr(
          lapply(rep(c("Subjects"), length(databaseIds)), th)
        )
      )
    ))

    sortCallback <- c(
      "var dt = table.table().node();",
      "$(dt).on('order.dt', function(e, ctx, order) {",
        "console.log(order);",
        " if (Array.isArray(order) && order.length > 0) {",
        "    console.log(order[0]);",
        "    col = order[0].col;",
        "    if (col < 2) {",
        "      var api = new $.fn.DataTable.Api(this);",
        "      var orderingArr = [];",
        "      for (var i=0 ; i<order.length ; i++) {",
        "        orderingArr.push(order[i].col);",
        "      }",
        #"      api.rowGroup().dataSrc(orderingArr);",
        "    }",
        "  }",
        "})"
    )
    
    
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   info = TRUE,
                   #dom = 'tip',
                   scrollX = TRUE,
                  # rowGroup = list(dataSrc = 0),
                   columnDefs = list(
                      #list(targets = c(0), visible = 0),
                      minCellCountDef(2:(length(databaseIds) - 1))
                     )
                   )
    extensions = c('RowGroup')
    
    dataTable <- datatable(table,
                           callback = JS(sortCallback),
                           options = options,
                           rownames = FALSE,
                           container = sketch, 
                           escape = FALSE,
                           #extensions = extensions,
                           class = "stripe nowrap compact")
    return(dataTable)
  })

  output$cohortName <- renderUI({ 
    return(htmltools::withTags(
        div(class="cohort-heading",
          h4(targetCohortName())
        )
      ))
  })
  
  output$comparisonName <- renderUI({
    targetCount <- cohortCount[cohortCount$cohortId == cohortId() & cohortCount$databaseId == input$database, c("cohortSubjects")] 
    comparatorCount <- cohortCount[cohortCount$cohortId == comparatorCohortId() & cohortCount$databaseId == input$database, c("cohortSubjects")] 
    return(htmltools::withTags(
        div(class="cohort-heading",
            h4("Target: ", targetCohortName(), " (n=", targetCount$cohortSubjects, ")"),
            h4("Comparator: ", comparatorCohortName(), " (n=", comparatorCount$cohortSubjects, ")"))
        ))
  })
  
  output$characterizationTable <- renderDataTable({
    data <- covariateValue[covariateValue$cohortId == cohortId() & covariateValue$databaseId %in% input$databases, ]
    counts <- cohortCount[cohortCount$cohortId == cohortId() & cohortCount$databaseId %in% input$databases, ] 
    data$cohortId <- NULL
    databaseIds <- unique(data$databaseId)
    databaseIdsWithCounts <- merge(databaseIds, counts, by.x="x", by.y="databaseId")
    databaseIdsWithCounts <- dplyr::rename(databaseIdsWithCounts, databaseId="x")
    table <- data[data$databaseId == databaseIds[1], c("covariateId", "mean")]
    colnames(table)[2] <- paste(colnames(table)[2], databaseIds[1], sep = "_")
    if (length(databaseIds) > 1) {
      for (i in 2:length(databaseIds)) {
        temp <- data[data$databaseId == databaseIds[i], c("covariateId", "mean")]
        colnames(temp)[2] <- paste(colnames(temp)[2], databaseIds[i], sep = "_")
        table <- merge(table, temp, all = TRUE)
      }
    }
    columnDefs <- list(
      truncateStringDef(0, 150),
      minCellPercentDef(1:length(databaseIds))
    )
    covariateFiltered <- getFilteredCovariates()
    table <- merge(covariateFiltered, table)    
    table$covariateAnalysisId <- NULL
    table$covariateId <- NULL
    table$windowId <- NULL
    table <- table[order(table$covariateName), ]
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   deferRender = TRUE,
                   columnDefs = columnDefs
    )
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 3, 'Covariate Name'),
          lapply(databaseIdsWithCounts$databaseId, th, colspan = 1, class = "dt-center no-border no-padding")
        ),
        tr(
          lapply(paste0("(n = ", format(databaseIdsWithCounts$cohortSubjects, big.mark = ","), ")"), th, colspan = 1, class = "dt-center no-padding")
        ),
        tr(
          lapply(rep(c("Proportion"), length(databaseIds)), th, colspan = 1)
        )
      )
    ))
    table <- datatable(table,
                       options = options,
                       rownames = FALSE,
                       container = sketch,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table,
                         columns = 1:length(databaseIds),
                         background = styleColorBar(c(0,1), "lightblue"),
                         backgroundSize = "98% 88%",
                         backgroundRepeat = "no-repeat",
                         backgroundPosition = "center")
    return(table)
  })
  
  computeBalance <- reactive({
    if (cohortId() == comparatorCohortId()) {
      return(data.frame())
    }
    covariateFiltered <- covariateFiltered <- getFilteredCovariates()
    covs1 <- covariateValue[covariateValue$cohortId == cohortId() & covariateValue$databaseId == input$database, ]
    covs2 <- covariateValue[covariateValue$cohortId == comparatorCohortId() & covariateValue$databaseId == input$database, ]
    covs1 <- merge(covs1, covariateFiltered)
    covs2 <- merge(covs2, covariateFiltered)
    balance <- compareCohortCharacteristics(covs1, covs2)
    balance$absStdDiff <- abs(balance$stdDiff)
    return(balance)
  })
  
  output$charCompareTable <- renderDataTable({
    balance <- computeBalance()
    if (nrow(balance) == 0) {
      return(NULL)
    }

    columnDefs <- list(
      truncateStringDef(0, 150),
      minCellPercentDef(c(1,3)),
      minCellRealDef(c(2,4), 2)
    )
    table <- balance
    table <- table[order(table$covariateName), ]
    table <- table[, c("covariateName", "mean1", "sd1", "mean2", "sd2", "stdDiff")]
    colnames(table) <- c("Covariate name", "Mean Target", "SD Target", "Mean Comparator", "SD Comparator", "StdDiff")
    
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   deferRender = TRUE,
                   columnDefs = columnDefs
    )
    table <- datatable(table,
                       options = options,
                       rownames = FALSE,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table,
                         columns = c(2,4),
                         background = styleColorBar(c(0,1), "lightblue"),
                         backgroundSize = "98% 88%",
                         backgroundRepeat = "no-repeat",
                         backgroundPosition = "center")
    table <- formatStyle(table = table,
                         columns = 6,
                         background = styleAbsColorBar(1, "lightblue", "pink"),
                         backgroundSize = "98% 88%",
                         backgroundRepeat = "no-repeat",
                         backgroundPosition = "center")
    table <- formatRound(table, c(3, 5, 6), digits = 2)
    return(table)
  })
  
  output$charComparePlot <- renderPlot({
    balance <- computeBalance()
    if (nrow(balance) == 0) {
      return(NULL)
    }
    balance$mean1[is.na(balance$mean1)] <- 0
    balance$mean2[is.na(balance$mean2)] <- 0
    plot <- ggplot2::ggplot(balance, ggplot2::aes(x = mean1, y = mean2, color = absStdDiff)) +
      ggplot2::geom_point(alpha = 0.3, shape = 16, size = 2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0) +             
      ggplot2::scale_x_continuous("Mean Target", limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Mean Comparator", limits = c(0, 1)) +
      ggplot2::scale_color_gradient("Absolute\nStd. Diff.", low = "blue", high = "red", space = "Lab", na.value = "red")
    return(plot)
  }, res = 100)
  
  output$hoverInfoCharComparePlot <- renderUI({
    balance <- computeBalance()
    balance$mean1[is.na(balance$mean1)] <- 0
    balance$mean2[is.na(balance$mean2)] <- 0
    if (nrow(balance) == 0) {
      return(NULL)
    } else {
      hover <- input$plotHoverCharCompare
      point <- nearPoints(balance, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      text <- paste(point$covariateName, 
                    "",
                    sprintf("<b>Mean Target: </b> %0.2f", point$mean1),
                    sprintf("<b>Mean Comparator: </b> %0.2f", point$mean2), 
                    sprintf("<b>Std diff.: </b> %0.2f", point$stdDiff), 
                    sep = "<br/>")
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      if (hover$x > 0.5) {
        xOffset <- -505
      } else {
        xOffset <- 5
      }
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:",
                      left_px + xOffset,
                      "px; top:",
                      top_px - 150,
                      "px; width:500px;")
      div(
        style = "position: relative; width: 0; height: 0",
        wellPanel(
          style = style,
          p(HTML(text))
        )
      )
    }
  }) 
  
  output$databaseInformationTable <- renderDataTable({

    table <- database[, c("databaseId", "databaseName", "description")]
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
                       colnames = c("ID", "Name", "Description"),
                       rownames = FALSE,
                       class = "stripe compact")
    return(table)
  })
  
  getFilteredCovariates <- function() {
    return(covariate[covariate$windowId %in% windowId() & covariate$covariateAnalysisId %in% covariateAnalysisId(),])
  }

  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  observeEvent(input$cohortCountsInfo, {
    showInfoBox("Cohort Counts", "html/cohortCounts.html")
  })
  
  observeEvent(input$cohortCharacterizationInfo, {
    showInfoBox("Cohort Characterization", "html/cohortCharacterization.html")
  })
  
  observeEvent(input$compareCohortCharacterizationInfo, {
    showInfoBox("Compare Cohort Characteristics", "html/compareCohortCharacterization.html")
  })
})
