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
      
     p0 <- plotIRv3(outcomeCohortDefinitionId, "Common",data = IRFilteredPlotdata())
      
     p1 <-
        plotIRv3(outcomeCohortDefinitionId, "Common",data = IRFilteredPlotdata()) + theme_my(base_size = 9) +
        scale_y_continuous(
          trans = 'log10',
          limits = c(0.1, 10000),
          breaks = c(0.1, 1, 10, 100, 1000, 10000)
        ) +
        theme(legend.position = "none")
      p2 <-
        plotIRv3(outcomeCohortDefinitionId,  "Rare",data = IRFilteredPlotdata()) + theme_my(base_size = 10) +
        scale_y_continuous(
          trans = 'log10',
          limits = c(.1, 1000),
          breaks = c(0.1, 1, 10, 100, 1000, 10000)
        ) +
        theme(legend.position = "none")
      p3 <-
        plotIRv3(outcomeCohortDefinitionId,  "Very rare",data = IRFilteredPlotdata()) + theme_my(base_size =
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
    colnames(data) <- camelCaseToTitleCase(colnames(data))
    return(data)
  })
  
  output$cohortTable <- DT::renderDT({
    data <- cohort
    colnames(cohort) <- camelCaseToTitleCase(colnames(cohort))
    return(cohort)
  })
})
