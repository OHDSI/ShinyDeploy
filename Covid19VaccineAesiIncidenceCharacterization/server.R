shiny::shinyServer(function(input, output, session) {
  
  filteredSexGroups <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {
    list(input$sexFilter_open,
         input$tabs)
  }, handlerExpr = {
    if (isFALSE(input$sexFilter_open) || !is.null(input$tabs)) {
      result <- input$sexFilter
      filteredSexGroups(result)
    }
  })
  
  
  filteredAgeGroups <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {
    list(input$ageFilter_open,
         input$tabs)
  }, handlerExpr = {
    if (isFALSE(input$ageFilter_open) || !is.null(input$tabs)) {
      result <- input$ageFilter
      filteredAgeGroups(result)
    }
  })
  
  filteredDatabaseIds <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {
    list(input$databaseFilter_open,
         input$tabs)
  }, handlerExpr = {
    if (isFALSE(input$databaseFilter_open) || !is.null(input$tabs)) {
      result <- input$databaseFilter
      filteredDatabaseIds(result)
    }
  })
  
  IRFilteredPlotdata <- shiny::reactive({
    validate(need(length(filteredSexGroups()) > 0, "No gender selected"))
    validate(need(length(filteredAgeGroups()) > 0, "No age groups selected"))
    validate(need(length(filteredDatabaseIds()) > 0, "No databases selected"))
    
    data <- ir_for_plot
    if(!is.null(filteredSexGroups())) {
      data <- data %>% 
        dplyr::filter(.data$sex_group %in% filteredSexGroups())
    }
    if(!is.null(filteredAgeGroups())) {
      data <- data %>% 
        dplyr::filter(.data$age_group %in% filteredAgeGroups())
    }
    if(!is.null(filteredDatabaseIds())) {
      data <- data %>% 
        dplyr::filter(.data$db_name %in% filteredDatabaseIds())
    }
    return(data)
  })
  
  output$outputPlot <- renderPlot({
    shiny::withProgress(message = "Building Plot. . .", {
      validate(need((nrow(IRFilteredPlotdata()) > 0), paste0("Data is not loaded.")))
      
      if ('Meta-Analysis' %in% filteredDatabaseIds()) {
        presentMetaAnalysisInPlot <- TRUE
      } else {
        presentMetaAnalysisInPlot <- FALSE
      }
      
     p0 <- plotIRv3(outcomeCohortDefinitionId, "Common",data = IRFilteredPlotdata(), metaAnalysis = presentMetaAnalysisInPlot)
      
     p1 <-
        plotIRv3(outcomeCohortDefinitionId, "Common",data = IRFilteredPlotdata(), metaAnalysis = presentMetaAnalysisInPlot) + theme_my(base_size = 9) +
        scale_y_continuous(
          trans = 'log10',
          limits = c(0.1, 10000),
          breaks = c(0.1, 1, 10, 100, 1000, 10000)
        ) +
        theme(legend.position = "none")
      p2 <-
        plotIRv3(outcomeCohortDefinitionId,  "Rare",data = IRFilteredPlotdata(), metaAnalysis = presentMetaAnalysisInPlot) + theme_my(base_size = 10) +
        scale_y_continuous(
          trans = 'log10',
          limits = c(.1, 1000),
          breaks = c(0.1, 1, 10, 100, 1000, 10000)
        ) +
        theme(legend.position = "none")
      p3 <-
        plotIRv3(outcomeCohortDefinitionId,  "Very rare",data = IRFilteredPlotdata(), metaAnalysis = presentMetaAnalysisInPlot) + theme_my(base_size =
                                                                       10) +
        scale_y_continuous(
          trans = 'log10',
          limits = c(.05, 1000),
          breaks = c(0.05, 0.1, 1, 10, 100, 1000, 1000)
        ) +
        theme(legend.position = "none")
      
      legend <- cowplot::get_legend(p0 +  guides(color = guide_legend(nrow = 15)) +
                                      theme(legend.position = "right"))
      pcol <- cowplot::plot_grid(p1, p2, p3, nrow = 3)  #,hjust=-1
      
      p123_v3 <-
        cowplot::plot_grid(
          pcol,
          legend,
          rel_widths = c(1, .15),
          axis = 'l',
          labels = "AUTO",
          label_y = -8
        )
    })
    return(p123_v3)
  })
  
  # observeEvent(eventExpr = input$sexFilter,handlerExpr = {
  #   if(!is.null(input$sexFilter)) {
  #     ir_for_plot_data(ir_for_plot_data()[ir_for_plot_data()$sex_group %in% input$sexFilter,])
  #   }
  # })
  
  
  
  output$resultTable <- DT::renderDT({
    data <- IRFilteredPlotdata() %>% 
      dplyr::select(
        .data$outcomeName,
        .data$databaseName,
        .data$age_group,
        .data$sex_group,
        .data$numOutcomes,
        .data$personYears,
        .data$IR_P_100000py
      ) %>% 
      dplyr::rename("Outcome" = "outcomeName",
                    "Data Source" = "databaseName",
                    "Age" = "age_group",
                    "Sex" = "sex_group",
                    "Incidence Rate/100,000 py" = "IR_P_100000py",
                    "Person Years" = "personYears",
                    "Case Count" = "numOutcomes") %>% 
      dplyr::mutate_if(is.numeric, ~round(., 2))
    table <- standardDataTable(data)
    return(table)
  })
  observe({
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "sexFilter",
                   choices = sexGroups,
                   selected = sexGroups
                 )
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "ageFilter",
                   choices = as.vector(ageGroups),
                   selected = as.vector(ageGroups)
                 )
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "databaseFilter",
                   choices = db_names,
                   selected = db_names
                 )
               })
  
  output$dataSourceTable <- DT::renderDT({
    data <- dataSource
    dataTable <- standardDataTable(data)
    return(dataTable)
  })
  
  output$cohortTable <- DT::renderDT({
    data <- cohort %>% 
      dplyr::select(.data$phenotype,.data$cohortId,.data$cohortName,.data$link) %>% 
      dplyr::mutate(cohortName = paste0("<a href='",.data$link,"'>",.data$cohortName,"</a>")) %>% 
      dplyr::select(-.data$link)
    colnames(data) <- camelCaseToTitleCase(colnames(data))
    table <- standardDataTable(data)
    return(table)
  }, selection = "single")
  
  selectedCohortDefinitionRow <- reactive({
    idx <- input$cohortTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      subset <- cohort
      if (nrow(subset) == 0) {
        return(NULL)
      }
      row <- subset[idx[1],]
      return(row)
    }
  })
  
  output$cohortDefinitionRowIsSelected <- reactive({
    return(!is.null(selectedCohortDefinitionRow()))
  })
  
  outputOptions(output,
                "cohortDefinitionRowIsSelected",
                suspendWhenHidden = FALSE)
  
  output$cohortDetailsText <- shiny::renderUI({
    row <- selectedCohortDefinitionRow()
    if (!'logicDescription' %in% colnames(row)) {
      row$logicDescription <- row$cohortName
    }
    if (is.null(row)) {
      return(NULL)
    } else {
      tags$table(
        style = "margin-top: 5px;",
        tags$tr(
          tags$td(tags$strong("Cohort ID: ")),
          tags$td(HTML("&nbsp;&nbsp;")),
          tags$td(row$cohortId)
        ),
        tags$tr(
          tags$td(tags$strong("Cohort Name: ")),
          tags$td(HTML("&nbsp;&nbsp;")),
          tags$td(row$cohortName)
        ),
        tags$tr(
          tags$td(tags$strong("Logic: ")),
          tags$td(HTML("&nbsp;&nbsp;")),
          tags$td(row$logicDescription)
        )
      )
    }
  })
  
  cohortDefinitionCirceRDetails <- shiny::reactive(x = {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering human readable cohort description using CirceR (may take time)", value = 0)
    data <- selectedCohortDefinitionRow()
    if (nrow(selectedCohortDefinitionRow()) > 0) {
      details <- list()
      circeExpression <-
        CirceR::cohortExpressionFromJson(expressionJson = data$json)
      circeExpressionMarkdown <-
        CirceR::cohortPrintFriendly(circeExpression)
      circeConceptSetListmarkdown <-
        CirceR::conceptSetListPrintFriendly(circeExpression$conceptSets)
      details <- data
      details$circeConceptSetListmarkdown <-
        circeConceptSetListmarkdown
      details$htmlExpressionCohort <-
        convertMdToHtml(circeExpressionMarkdown)
      details$htmlExpressionConceptSetExpression <-
        convertMdToHtml(circeConceptSetListmarkdown)
      
      details <- dplyr::bind_rows(details)
    } else {
      return(NULL)
    }
    return(details)
  })
  
  output$cohortDefinitionText <- shiny::renderUI(expr = {
    cohortDefinitionCirceRDetails()$htmlExpressionCohort %>%
      shiny::HTML()
  })
  
  
  
  cohortDefinistionConceptSetExpression <- shiny::reactive({
    row <- selectedCohortDefinitionRow()
    if (is.null(row)) {
      return(NULL)
    }
    
    expression <- RJSONIO::fromJSON(row$json, digits = 23)
    if (is.null(expression)) {
      return(NULL)
    }
    
    expression <-
      getConceptSetDetailsFromCohortDefinition(cohortDefinitionExpression = expression)
    
    return(expression)
  })
  
  output$conceptsetExpressionTable <- DT::renderDataTable(expr = {
    data <- cohortDefinistionConceptSetExpression()
    if (is.null(data)) {
      return(NULL)
    }
    
    if (!is.null(data$conceptSetExpression) &&
        nrow(data$conceptSetExpression) > 0) {
      data <- data$conceptSetExpression %>%
        dplyr::select(.data$id, .data$name)
      
    } else {
      return(NULL)
    }
    dataTable <- standardDataTable(data = data)
    return(dataTable)
  }, server = TRUE)
  
  cohortDefinitionConceptSetExpressionRow <- shiny::reactive(x = {
    idx <- input$conceptsetExpressionTable_rows_selected
    if (length(idx) == 0 || is.null(idx)) {
      return(NULL)
    }
    if (!is.null(cohortDefinistionConceptSetExpression()$conceptSetExpression) &&
        nrow(cohortDefinistionConceptSetExpression()$conceptSetExpression) > 0) {
      data <-
        cohortDefinistionConceptSetExpression()$conceptSetExpression[idx, ]
      if (!is.null(data)) {
        return(data)
      } else {
        return(NULL)
      }
    }
  })
  
  output$conceptSetExpressionRowSelected <- shiny::reactive(x = {
    return(!is.null(cohortDefinitionConceptSetExpressionRow()))
  })
  shiny::outputOptions(x = output,
                       name = "conceptSetExpressionRowSelected",
                       suspendWhenHidden = FALSE)
  
  cohortDefinitionConceptSets <- shiny::reactive(x = {
    if (is.null(cohortDefinitionConceptSetExpressionRow())) {
      return(NULL)
    }
    
    data <-
      cohortDefinistionConceptSetExpression()$conceptSetExpressionDetails
    data <- data %>%
      dplyr::filter(.data$id == cohortDefinitionConceptSetExpressionRow()$id)
    data <- data %>%
      dplyr::select(
        .data$conceptId,
        .data$conceptName,
        .data$standardConcept,
        .data$invalidReason,
        .data$conceptCode,
        .data$domainId,
        .data$vocabularyId,
        .data$conceptClassId
      )
    return(data)
  })
  
  output$cohortDefinitionConceptSetsTable <-
    DT::renderDataTable(expr = {
      data <- cohortDefinitionConceptSets()
      if (is.null(cohortDefinitionConceptSets())) {
        return(NULL)
      }
      dataTable <- standardDataTable(data = data)
      return(dataTable)
    }, server = TRUE)
  
  output$cohortConceptsetExpressionJson <- shiny::renderText({
    if (is.null(cohortDefinitionConceptSetExpressionRow())) {
      return(NULL)
    }
    cohortDefinitionConceptSetExpressionRow()$json
  })
  

  
  output$cohortDefinitionJson <- shiny::renderText({
    row <- selectedCohortDefinitionRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      row$json
    }
  })
  
  output$cohortDefinitionSql <- shiny::renderText({
    row <- selectedCohortDefinitionRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      circeExpression <-
        CirceR::cohortExpressionFromJson(expressionJson = row$json)
      circeoptions <-
        CirceR::createGenerateOptions(cohortId = row$cohortId)
      sql <-
        CirceR::buildCohortQuery(expression = circeExpression, options = circeoptions)
      return(sql)
    }
  })
  
})
