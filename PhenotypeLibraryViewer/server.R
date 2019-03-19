# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(ggplot2)
library(knitr)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Server Definition
shinyServer(function(input, output) {

  # Inspect Tab
  output$inspect_tab <- renderUI({
    box(
      title = "Metric Distributions", status = "warning", solidHeader = TRUE, width = NULL,
      renderPlot(
        ggplot(
          getDummyValidationPlotData(), aes(x = Metric, y = Values, fill = Name)
        ) +
          geom_boxplot() +
          theme_minimal() +
          theme(text = element_text(size = 20)) +
          theme(
            legend.position = "bottom",
            legend.direction = "vertical"
          )
      )
    )
  })

  # Temp function for demo purposes only
  getDummyValidationStats <- function(name) {
    set.seed(length(name))
    return(
      data.frame(
        Percent = runif(20, 0, 100),
        Metric = factor(
          c(
            rep("Sensitivity", 5),
            rep("Specificity", 5),
            rep("PPV", 5),
            rep("NPV", 5)
          ),
          levels = c("Sensitivity", "Specificity", "PPV", "NPV")
        )
      )
    )
  }

  # Temp function for demo purposes only
  getDummyValidationPlotData <- function() {
    current_selected <- input$phenotype_search

    dummy_data <- data.frame(
      Name = rep(current_selected, 400 * length(current_selected)),
      Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), 100 * length(current_selected)),
      Values = runif(400 * length(current_selected))
    )
    dummy_data$Metric <- sample(dummy_data$Metric)
    dummy_data$Metric <- factor(dummy_data$Metric, levels = c("Sensitivity", "Specificity", "PPV", "NPV"))

    return(dummy_data)
  }

  # Export tab rendering utility function for downloading
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Example_Phenotype_Download.json")
    },
    content = function(file) {
      # TODO: Writing iris data for demo purposes -- obviously needs to be hooked up to a real export
      write.csv(iris, file, na = "")
    }
  )

  # TODO: If possible, render "in place" so writing out a temp file isn't needed.
  # Does rmarkdown::render() have an option to return the output file as a variable instead of writing it to disk?
  getSummaryRmd <- function(name) {

    # Get temporary directory
    temp_d <- tempdir()
    
    # Create name of phenotype file in temp directory
    temp_f <- file.path(temp_d, paste0(name,".html"))
    
    # Render correponding document (for speed, pre-rendering could be required in advance, but for simplicity, authors could leave as Rmd)
    rmarkdown::render(file.path("data","Example_Phenotype_Submission_Template.Rmd"),
      output_file = temp_f
    )

    # Then pass back the rendered HTML file
    return(
      HTML(paste(readLines(temp_f), collapse = "\n"))
    )
  }

  getValidationRmd <- function() {
    
    # Get temporary directory
    temp_d <- tempdir()
    
    name <- "dummy_validation_report"
    
    temp_f <- file.path(temp_d, paste0(name,".html"))

    # Render correponding document (for speed, this could be required in advance, but for simplicity, authors could leave as Rmd)
    rmarkdown::render(file.path("data", "Example_Validation_Submission_Template.Rmd"),
      output_file = temp_f
    )

    # Then pass back the rendered HTML file
    return(
      HTML(paste(readLines(temp_f), collapse = "\n"))
    )
  }

  ### Menu Items

  # Creates the export menuItem whenever at least one phenotype was considered for export
  output$Export_Menu <- renderMenu({
    if (length(input$phenotype_search) > 0) {
      return(menuItem("Export", tabName = "export", icon = icon("file-export")))
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  # Shows the "Compare" menuItem whenever there are at least two phenotypes to compare
  output$getInspectMenu <- renderMenu({
    if (length(input$phenotype_search) > 1) {
      return(
        menuItem("Compare", tabName = "inspect", icon = icon("chart-bar"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  # TODO: Wrap similar Tab code in function calls -- Part 2 of 3

  output$getFirstPhenotype <- renderMenu({
    if (length(input$phenotype_search) >= 1) {
      return(
        menuItem(input$phenotype_search[1], tabName = "tab1", icon = icon("square"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  output$getSecondPhenotype <- renderMenu({
    if (length(input$phenotype_search) >= 2) {
      return(
        menuItem(input$phenotype_search[2], tabName = "tab2", icon = icon("square"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  output$getThirdPhenotype <- renderMenu({
    if (length(input$phenotype_search) >= 3) {
      return(
        menuItem(input$phenotype_search[3], tabName = "tab3", icon = icon("square"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  output$getFourthPhenotype <- renderMenu({
    if (length(input$phenotype_search) >= 4) {
      return(
        menuItem(input$phenotype_search[4], tabName = "tab4", icon = icon("square"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  output$getFifthPhenotype <- renderMenu({
    if (length(input$phenotype_search) >= 5) {
      return(
        menuItem(input$phenotype_search[5], tabName = "tab5", icon = icon("square"))
      )
    } else {
      return(h1("")) # Can't be NULL - using h1() to force refresh
    }
  })

  ### Tabs
  
  # Create an example tabsetPanel() to populate one phenotype selection
  makeExampleBox <- function(name) {
    return(
      tabsetPanel(
        tabPanel("Summary",
                 icon = icon("clipboard"),
                 
                 fluidRow(column(12, box(status = "primary", width = NULL, getSummaryRmd(name))))
        ),
        
        tabPanel("Validation Sets",
                 icon = icon("calculator"),
                 
                 tabsetPanel(
                   tabPanel(
                     "Overview", h1("Validation Overview"),
                     fluidRow(column(12, box(
                       status = "primary", width = NULL, title = "Validations", solidHeader = TRUE,
                       h4("Number of Validation Sets: 5")
                     ))),
                     fluidRow(column(12, box(
                       status = "primary", width = NULL, title = "Metric Distributions", solidHeader = TRUE,
                       renderPlot(
                           ggplot(getDummyValidationStats(name), aes(x = Metric, y = Percent)) +
                           geom_boxplot() +
                           geom_point(shape = 21, color = "black", fill = "orange", size = 5) +
                           theme_classic() +
                           theme(text = element_text(size = 20))
                       )
                     )))
                   ),
                   tabPanel(
                     "Individual Sets",
                     h1("Validation Reports"),
                     fluidRow(column(
                       width = 2, offset = 5,
                       box(
                         status = "primary", width = NULL,
                         # TODO: Can a better widget be used to page through validation sets?
                         # Can we have a "Previous/Next" pager?
                         numericInput("set_num", "Set Number (of 5):", 1, min = 1, max = 5, step = 1, width = 50)
                       )
                     )),
                     fluidRow(column(12, box(
                       status = "primary", width = NULL,
                       getValidationRmd()
                     )))
                   )
                 )
        ),
        tabPanel(
          title = "Export", icon = icon("download"),
          fluidRow(
            box(
              title = "Export Phenotype", status = "primary",
              selectInput("export_type", "Preferred Implementation Method:", choices = c("JSON", "Others...")),
              downloadButton(
                outputId = "downloadData", label = "Click to Download Export", icon = icon("download"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
        )
      ) # End tabsetPanel
    ) # End return
  } # End makeExampleBox
  
  # TODO: Wrap similar Tab code in function calls -- Part 3 of 3

  output$tab1 <- renderUI({
    tabItem(
      tabName = "tab1",
      makeExampleBox(input$phenotype_search[1])
    )
  })

  output$tab2 <- renderUI({
    tabItem(
      tabName = "tab2",
      makeExampleBox(input$phenotype_search[2])
    )
  })

  output$tab3 <- renderUI({
    tabItem(
      tabName = "tab3",
      makeExampleBox(input$phenotype_search[3])
    )
  })

  output$tab4 <- renderUI({
    tabItem(
      tabName = "tab4",
      makeExampleBox(input$phenotype_search[4])
    )
  })

  output$tab5 <- renderUI({
    tabItem(
      tabName = "tab5",
      makeExampleBox(input$phenotype_search[5])
    )
  })

  # Renders a header if there exist menuItems to separate
  output$conditionalHR <- renderUI({
    if (length(input$phenotype_search) > 0) {
      return(hr())
    }
  })

}) # End shinyServer
