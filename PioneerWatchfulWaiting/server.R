library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)
library(data.table)
source("PlotsAndTables.R")
source("utilities.R")
source("survplot_core.R")
source("ggsurvtable.R")


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

getCovariateDataSubset <- function(cohortId, databaseList, comparatorCohortId = NULL) {
  if (usingDbStorage()) {
    return(getCovariateValue(connPool, cohortId = cohortId, databaseList = databaseList, comparatorCohortId = comparatorCohortId))
  } else {
    return(covariateValue[covariateValue$cohortId %in% c(cohortId, comparatorCohortId) & covariateValue$databaseId %in% databaseList, ])
  }
}

getDataTableSettings <- function() {
  dtSettings <- list(
    options = list(pageLength = 25,
                   lengthMenu = c(25, 50, 100, -1),
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   info = TRUE,
                   scrollX = TRUE),
    extensions = list() #list('Buttons') #'RowGroup'
  )
                     
  return(dtSettings)
}

renderBorderTag <-  function() {
  return(htmltools::withTags(
    div(class="cohort-heading")
  ))
}

showTermsOfUseModal <- function() {
  showModal(
    modalDialog(
      title="Terms of Use", 
      includeMarkdown("md/terms_of_use.md"),
      footer = tagList(
        actionButton("termsOfUseReject", "Reject", style="color: white", class="btn-danger"),
        actionButton("termsOfUseAccept", "Accept", style="color: white", class="btn-success")
      )
    )
  )
}

TERMS_OF_USE_ACCEPTED <- "accepted"

shinyServer(function(input, output, session) {
  # Terms Of Use Modal -------------------
  observe({
    # Show this modal whenever the user has not accepted the terms of use
    if (!is.null(input$jscookie)) {
      if (input$jscookie != TERMS_OF_USE_ACCEPTED) {
        showTermsOfUseModal()
      }
    }
  })
  
  # Used for testing cookie set/removal
  # observeEvent(input$cookieGetVal, {
  #   if (!is.null(input$jscookie)) {
  #     writeLines(input$jscookie)
  #   } else {
  #     writeLines("NULL")
  #   }
  # })
  # 
  # observeEvent(input$cookieRmVal, {
  #   writeLines("Cookie removed")
  #   session$sendCustomMessage("rmCookie", "")
  #   writeLines("----------------")
  # })
  
  observeEvent(input$termsOfUseReject, {
    session$sendCustomMessage("alert", "You must accept the terms of use to use the application.")
  })
  
  observeEvent(input$termsOfUseAccept, {
    writeLines("Set cookie")
    session$sendCustomMessage("setCookie", TERMS_OF_USE_ACCEPTED)
    removeModal()
  })
  
  
  
  # Filter Options -----
  cohortIdTimeToEvent <- reactive({
    return(unlist(cohortXref[cohortXref$targetId %in% targetCohortIdTimeToEvent() 
                             & cohortXref$strataName %in% input$strataTimeToEvent,c("cohortId")]))
  })
  
  targetCohortIdTimeToEvent <- reactive({
    return(targetCohort$targetId[targetCohort$targetName %in% input$targetTimeToEvent])
  })
  

  cohortIdMetricsDistribution <- reactive({
    return(unlist(cohortXref[cohortXref$targetId %in% targetCohortIdMetricsDistribution() 
                             & cohortXref$strataName %in% input$strataMetricsDistribution, c("cohortId")]))
  })
  
  targetCohortIdMetricsDistribution <- reactive({
    return(targetCohort$targetId[targetCohort$targetName %in% input$targetMetricsDistribution])
  })
  
  
  cohortIdList <- reactive({
    return(unlist(cohortXref[cohortXref$targetId %in% targetCohortIdList() & cohortXref$strataName %in% input$strataCohortList,c("cohortId")]))
  })
  
  targetCohortIdList <- reactive({
    return(targetCohort$targetId[targetCohort$targetName %in% input$targetCohortList])
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
  
  output$cohortName <- renderUI({ 
    return(htmltools::withTags(
      div(h4(targetCohortName()))
    ))
  })
  
  output$comparisonName <- renderUI({
    targetCount <- cohortCount[cohortCount$cohortId == cohortId() & cohortCount$databaseId == input$database, c("cohortSubjects")] 
    comparatorCount <- cohortCount[cohortCount$cohortId == comparatorCohortId() & cohortCount$databaseId == input$database, c("cohortSubjects")] 
    return(htmltools::withTags(
      div(h4("Target: ", targetCohortName(), " (n=", targetCount, ")"),
          h4("Comparator: ", comparatorCohortName(), " (n=", comparatorCount, ")"))
    ))
  })
  
  # Cohort Info ---------
  output$borderCohortInfo <- renderUI({
    return(renderBorderTag())
  })
  
  getCohortInfoTable <- reactive({
    data <- cohortInfo
    atlasCohortUrl <- "https://pioneer-atlas.thehyve.net/#/cohortdefinition/"
    githubCohortUrl <- "https://github.com/ohdsi-studies/PioneerWatchfulWaiting/tree/master/inst/sql/sql_server/"
    data$url <- ifelse(data$circeDef == TRUE, 
                       paste0(atlasCohortUrl, data$atlasId),
                       paste0(githubCohortUrl, data$cohortId, ".sql"))
    data <- data[, c("name", "url")]
    return(data)
  })
  
  output$cohortInfoTable <- renderDataTable({
    table <- getCohortInfoTable()
    table$url <- paste0("<a href='", table$url, "' target='_blank'>", table$url, "</a>")
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th('Cohort Name'),
          th('Definition')
        )
      )
    ))
    
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   info = TRUE,
                   scrollX = TRUE
    )
    dataTable <- datatable(table,
                           options = options,
                           rownames = FALSE,
                           container = sketch, 
                           escape = FALSE,
                           class = "stripe nowrap compact")
    return(dataTable)
    
  })
  
  output$dlCohortInfo <- downloadHandler('cohort_info.csv', content = function(file) {
    table<-getCohortInfoTable()
    write.csv(table, file, row.names = FALSE, na = "")
  })
  
  
  
  
  # timeToEvent-----------------
  
  output$survivalHeader <- renderText({
    paste(input$targetTimeToEvent, input$strataTimeToEvent, sep=" ")
  })
  
  output$dlTimeToEvent <- downloadHandler(
    filename = function() {
      "timeToEvent.csv"
    },
    content = function(file) {
      target_id <- cohortCount[cohortCount$databaseId %in% input$databasesTimeToEvent &
                                 cohortCount$cohortId %in% cohortIdTimeToEvent(), ][[1]]
      targetIdTimeToEventData <- cohortTimeToEvent %>% dplyr::filter(targetId == target_id,
                                                                     databaseId == input$databasesTimeToEvent)
      write.csv(targetIdTimeToEventData, file, row.names = FALSE, na = "")
    }
  ) 
  
  output$TimeToEventDeath <- renderPlot({
    target_id <- cohortCount[cohortCount$databaseId %in% input$databasesTimeToEvent &
                             cohortCount$cohortId %in% cohortIdTimeToEvent(), ][[1]]
    target_id_entries_num <- sum(cohortCount[cohortCount$cohortId == target_id, "cohortEntries"])
    
    if (length(target_id) == 0 | target_id_entries_num <= 100 | is.null(input$KMPlot)){
      plot <- ggplot2::ggplot()
      return(plot)
    }
    
    targetIdTimeToEventData <- cohortTimeToEvent %>% dplyr::filter(targetId == target_id,
                                                               databaseId == input$databasesTimeToEvent)
    
    accumulatedData <- data.table(time = c(), surv = c(), n.censor = c(), 
                                  n.event = c(), upper = c(), lower = c())
    for(plotName in input$KMPlot){
      oId <- KMIds$id[KMIds$name == plotName]
      data <- targetIdTimeToEventData %>% dplyr::filter(outcomeId == oId)
      if (length(data) > 0){
        data <- as.data.frame(data[, c('time', 'surv', 'n.risk',  'n.censor', 'n.event', 'upper', 'lower')])
        data$strata <- plotName
      }
      else{
        data <- targetIdTimeToEventData %>% dplyr::filter(outcomeId == -1)
      }
      accumulatedData <- rbind(accumulatedData, data)
    }

    color_map <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    names(color_map) <- KMIds$name
   
    plot <- ggsurvplot_core(accumulatedData,
                            risk.table = "nrisk_cumcensor",
                            palette = color_map,
                            legend.labs = input$KMPlot,
                            cmap = color_map,
                            conf.int = TRUE,
                            legend.title = 'Event',
                            ylim = c(min(accumulatedData$lower), 1),
                            ggtheme = ggplot2::theme_bw()
                  )

    return(plot)
  })
  
  
  
  # Metrics Distribution
  output$metricsHeader <- renderText({
    paste(input$targetMetricsDistribution, input$strataMetricsDistribution, sep=" ")
  })
  
  
  getMetricsTable <- reactive ({
    target_id <- cohortCount[cohortCount$databaseId %in% input$databasesMetricsDistribution 
                             & cohortCount$cohortId %in% cohortIdMetricsDistribution(), ][[1]]
    metricsTable <- metricsDistribution %>% dplyr::filter(cohortDefinitionId == target_id, 
                                                          databaseId == input$databasesMetricsDistribution)
    names(metricsTable)[names(metricsTable) == 'iqr'] <- 'IQR'
    return(metricsTable[,c('analysisName', 'IQR', 'minimum', 'q1', 'median', 'q3', 'maximum')])
  })
  
  
  output$metricsTable <- renderDataTable(
    getMetricsTable()
  )
  
  
  # Cohort Counts ---------
  output$borderCohortCounts <- renderUI({
    return(renderBorderTag())
  })
  
  getCohortCountsTable <- reactive({
    data <- cohortCount[cohortCount$databaseId %in% input$databases & cohortCount$cohortId %in% cohortIdList(), ]
    table <- dplyr::inner_join(cohortXref, data, by="cohortId")
    table <- table[order(table$targetName),]
    return(table)
  })
  
  getCohortCountsTablePivotedByDB <- reactive({
    columnsToInclude <- c("cohortId","targetId","targetName","strataId","strataName","cohortType", "cohortSubjects")
    subjectIndex <-  match("cohortSubjects", columnsToInclude)
    data <- getCohortCountsTable()
    databaseIds <- unique(data$databaseId)
    databaseIds <- sort(databaseIds)
    table <- data[data$databaseId == databaseIds[1], ..columnsToInclude]
    colnames(table)[subjectIndex] <- paste(colnames(table)[2], databaseIds[1], sep = "_")
    if (length(databaseIds) > 1) {
      for (i in 2:length(databaseIds)) {
        temp <- data[data$databaseId == databaseIds[i], ..columnsToInclude]
        colnames(temp)[subjectIndex] <- paste(colnames(temp)[subjectIndex], databaseIds[i], sep = "_")
        table <- merge(table, temp, all = TRUE)
      }
    }
    return(list(table = table, databaseIds = databaseIds))
  })

  output$cohortCountsTable <- renderDataTable({
    cohortCountsByDB <- getCohortCountsTablePivotedByDB()
    databaseIds <- cohortCountsByDB$databaseIds
    table <- cohortCountsByDB$table
    table$cohortId <- NULL
    table$targetId <- NULL
    table$strataId <- NULL
    table$cohortType <- NULL

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
    columnDefs = list(
      #list(targets = c(0), visible = 0),
      minCellCountDef(2:(length(databaseIds) + 1))
    )
    dtSettings <- getDataTableSettings();
    dtSettings$options <- append(dtSettings$options, list(columnDefs = columnDefs))

    dataTable <- datatable(table,
                           # callback = JS(sortCallback),
                           rownames = FALSE,
                           container = sketch,
                           escape = FALSE,
                           options = dtSettings$options,
                           extensions = dtSettings$extensions,
                           class = "stripe nowrap compact")
    return(dataTable)
  })

  output$dlCohortCountsByDb <- downloadHandler(
    filename = function() {
      'cohort_counts_by_db.csv'
    },
    content = function(file) {
      table<-getCohortCountsTablePivotedByDB()$table
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )

  output$dlCohortCountsFlat <- downloadHandler(
    filename = function() {
      'cohort_counts.csv'
    },
    content = function(file) {
      table<-getCohortCountsTable()
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )

  # Cohort Characterization -------
  output$borderCharacterization <- renderUI({
    return(renderBorderTag())
  })
  
  getCharacterizationTable <- reactive({
    data <- getCovariateDataSubset(cohortId(), input$databases)
    covariateFiltered <- getFilteredCovariates()
    table <- merge.data.table(as.data.table(covariateFiltered), as.data.table(data))
    table$cohortName <- targetCohortName()
    return(table[,c("cohortId","cohortName","covariateId","covariateName","covariateAnalysisId","windowId","databaseId","mean")])
  })
  
  getCharacterizationTablePivotedByDB <- reactive({
    columnsToInclude <- c("cohortId","cohortName","covariateId","covariateName","covariateAnalysisId","windowId","mean")
    meanColumnIndex <-  match("mean", columnsToInclude)
    data <- getCharacterizationTable()
    counts <- cohortCount[cohortCount$cohortId == cohortId() & cohortCount$databaseId %in% input$databases, ] 
    databaseIds <- unique(data$databaseId)
    databaseIdsWithCounts <- merge(databaseIds, counts, by.x="x", by.y="databaseId")
    databaseIdsWithCounts <- dplyr::rename(databaseIdsWithCounts, databaseId="x")
    table <- data[data$databaseId == databaseIdsWithCounts$databaseId[1], ..columnsToInclude]
    colnames(table)[meanColumnIndex] <- paste(colnames(table)[meanColumnIndex], databaseIdsWithCounts$databaseId[1], sep = "_")
    if (nrow(databaseIdsWithCounts) > 1) {
      for (i in 2:nrow(databaseIdsWithCounts)) {
        temp <- data[data$databaseId == databaseIdsWithCounts$databaseId[i], ..columnsToInclude]
        colnames(temp)[meanColumnIndex] <- paste(colnames(temp)[meanColumnIndex], databaseIdsWithCounts$databaseId[i], sep = "_")
        table <- merge.data.table(table, temp, by = columnsToInclude[-length(columnsToInclude)], all = TRUE)
      }
    }
    table <- table[order(table$covariateName), ]
    return(list(table = table, databaseIdsWithCounts = databaseIdsWithCounts))
  })
  
  output$characterizationTable <- renderDataTable({
    characterizationByDB <- getCharacterizationTablePivotedByDB()
    databaseIdsWithCounts <- characterizationByDB$databaseIdsWithCounts
    table <- characterizationByDB$table
    
    columnDefs <- list(
      truncateStringDef(0, 150),
      minCellPercentDef(1:nrow(databaseIdsWithCounts))
    )
    table$cohortId <- NULL
    table$cohortName <- NULL
    table$covariateId <- NULL
    table$covariateAnalysisId <- NULL
    table$windowId <- NULL
    dtSettings <- getDataTableSettings();
    dtSettings$options <- append(dtSettings$options, list(columnDefs = columnDefs))
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
          lapply(paste0(databaseIdsWithCounts$databaseId, "_pct"), th, colspan = 1)
        )
      )
    ))
    table <- datatable(table,
                       rownames = FALSE,
                       container = sketch,
                       escape = FALSE,
                       options = dtSettings$options,
                       extensions = dtSettings$extensions,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table,
                         columns = 1:nrow(databaseIdsWithCounts)+1,
                         background = styleColorBar(c(0,1), "lightblue"),
                         backgroundSize = "98% 88%",
                         backgroundRepeat = "no-repeat",
                         backgroundPosition = "center")
    return(table)
  })
  
  output$dlCharacterizationByDb <- downloadHandler(
    filename = function() {
      "characterization_by_db.csv"
    },
    content = function(file) {
      table <- getCharacterizationTablePivotedByDB()$table
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )
  
  output$dlCharacterizationFlat <- downloadHandler(
    filename = function() {
      "characterization.csv"
    },
    content = function(file) {
      table <- getCharacterizationTable()
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )
  
  # Cohort Comparison --------------
  output$borderCharCompare <- renderUI({
    return(renderBorderTag())
  })
  
  computeBalance <- reactive({
    if (cohortId() == comparatorCohortId()) {
      return(data.table())
    }
    covariateFiltered <- getFilteredCovariates()
    covariateValue <- getCovariateDataSubset(cohortId(), input$database, comparatorCohortId())
    covs1 <- covariateValue[covariateValue$cohortId == cohortId() & covariateValue$databaseId == input$database, ]
    covs2 <- covariateValue[covariateValue$cohortId == comparatorCohortId() & covariateValue$databaseId == input$database, ]
    covs1 <- merge(covs1, covariateFiltered)
    covs2 <- merge(covs2, covariateFiltered)
    balance <- compareCohortCharacteristics(covs1, covs2)
    balance$absStdDiff <- abs(balance$stdDiff)
    # Rename columns
    colnames(balance) <- c("covariateId","covariateName","covariateAnalysisId","windowId","targetMean","targetSD","comparatorMean","comparatorSD","sd","stdDiff","absStdDiff")
    balance$targetCohortId <- cohortId()
    balance$targetCohortName <- targetCohortName()
    balance$comparatorCohortId <- comparatorCohortId()
    balance$comparatorCohortName <- comparatorCohortName()
    balance$databaseId <- input$database
    balance <- balance[order(balance$covariateName),]
    return(balance)
  })
  
  output$charCompareTable <- renderDataTable({
    balance <- computeBalance()
    if (nrow(balance) == 0) {
      return(NULL)
    }
    
    table <- balance[, c("covariateName", "targetMean", "targetSD", "comparatorMean", "comparatorSD", "stdDiff")]
    colnames(table) <- c("Covariate name", "Mean Target", "SD Target", "Mean Comparator", "SD Comparator", "StdDiff")
    columnDefs <- list(
      truncateStringDef(0, 150),
      minCellPercentDef(c(1,3)),
      minCellRealDef(c(2,4), 2)
    )
    dtSettings <- getDataTableSettings();
    dtSettings$options <- append(dtSettings$options, list(columnDefs = columnDefs))
    table <- datatable(table,
                       rownames = FALSE,
                       escape = FALSE,
                       options = dtSettings$options,
                       extensions = dtSettings$extensions,
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
    balance$targetMean[is.na(balance$targetMean)] <- 0
    balance$comparatorMean[is.na(balance$comparatorMean)] <- 0
    plot <- ggplot2::ggplot(balance, ggplot2::aes(x = targetMean, y = comparatorMean, color = absStdDiff)) +
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
    balance$targetMean[is.na(balance$targetMean)] <- 0
    balance$comparatorMean[is.na(balance$comparatorMean)] <- 0
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
                    sprintf("<b>Mean Target: </b> %0.2f", point$targetMean),
                    sprintf("<b>Mean Comparator: </b> %0.2f", point$comparatorMean), 
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
  
  output$dlCharCompare <- downloadHandler(
    filename = function() {
      "characterization_compare.csv"
    },
    content = function(file) {
      table <- computeBalance()
      table <- table[,c("targetCohortId","targetCohortName","comparatorCohortId","comparatorCohortName","covariateId","covariateName","covariateAnalysisId","windowId","databaseId","targetMean","targetSD","comparatorMean","comparatorSD","stdDiff")]
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )

  # Database Info ------------------
  output$borderDatabaseInformation <- renderUI({
    return(renderBorderTag())
  })
  
  output$databaseInformationTable <- renderDataTable({

    table <- database[, c("databaseId", "databaseName", "description", "termsOfUse")]
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = FALSE,
                   ordering = TRUE,
                   paging = FALSE,
                   columnDefs = list(list(width = '10%', targets = 0),
                                     list(width = '20%', targets = 1),
                                     list(width = '35%', targets = 2))
    )
    table <- datatable(table,
                       options = options,
                       colnames = c("ID", "Name", "Description", "Terms of Use"),
                       rownames = FALSE,
                       class = "stripe compact")
    return(table)
  })
  
  output$dlDatabaseInformation <- downloadHandler(
    filename = function() {
      "database_info.csv"
    },
    content = function(file) {
      table <- database[, c("databaseId", "databaseName", "description")]
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )
  
  
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  # Info boxes -------
  observeEvent(input$cohortCountsInfo, {
    showInfoBox("Cohort Counts", "html/cohortCounts.html")
  })
  
  observeEvent(input$cohortCharacterizationInfo, {
    showInfoBox("Cohort Characterization", "html/cohortCharacterization.html")
  })
  
  observeEvent(input$compareCohortCharacterizationInfo, {
    showInfoBox("Compare Cohort Characteristics", "html/compareCohortCharacterization.html")
  })
  
  observeEvent(input$dlCohortCountsInfo, {
    showInfoBox("Download", "html/download.html")
  })
  
  observeEvent(input$dlCharacterizationInfo, {
    showInfoBox("Download", "html/download.html")
  })

  # Helper functions ------
  getFilteredCovariates <- function() {
    return(covariate[covariate$windowId %in% bit64::as.integer64(windowId()$windowId) & covariate$covariateAnalysisId %in% covariateAnalysisId()$covariateAnalysisId,c("covariateId","covariateName","covariateAnalysisId","windowId")])
  }
})
