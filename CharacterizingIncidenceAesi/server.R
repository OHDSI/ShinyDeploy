shiny::shinyServer(function(input, output, session) {
  
  IRFilteredPlotdata <- shiny::reactive({
    data <- ir_for_plot
    if(!is.null(input$sexFilter)) {
      data <- data %>% 
        dplyr::filter(.data$sex_group %in% input$sexFilter)
    }
    if(!is.null(input$ageFilter)) {
      data <- data %>% 
        dplyr::filter(.data$age_group %in% input$ageFilter)
    }
    if(!is.null(input$databaseFilter)) {
      data <- data %>% 
        dplyr::filter(.data$db_name %in% input$databaseFilter)
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
        .data$incidenceRateP100py
      ) %>% 
      dplyr::rename("Outcome"= "outcomeName",
                    "Database"= "databaseName",
                    "Age"= "age_group",
                    "Sex"= "sex_group",
                    "Incidence Rate"= "incidenceRateP100py")
    return(data)
  })
  observeEvent(eventExpr = list(db_names, ageGroups, sexGroups),
               handlerExpr = {
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "sexFilter",
                   choices = sexGroups,
                   selected = sexGroups
                 )
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "ageFilter",
                   choices = as.vector(ageGroups) %>%  sort(),
                   selected = as.vector(ageGroups) %>%  sort()
                 )
                 shinyWidgets::updatePickerInput(
                   session = session,
                   inputId = "databaseFilter",
                   choices = db_names,
                   selected = db_names
                 )
               })
})
