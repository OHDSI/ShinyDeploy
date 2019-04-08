# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(ggplot2)
library(knitr)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Utility function to build a markdown file based on the template
buildPhenotypeMarkdown <- function(dat) {

  # Read template
  md_template <- paste0(
    readLines(file.path("data", "Phenotype_Submission_Template.md")),
    collapse = "\n"
  )

  # Perform direct substitutions on most variables
  for (term in names(dat)[!(names(dat) %in% c("Authors_And_Affiliations", "Provenance_Hashes", "Provenance_Reasons"))]) {
    md_template <- gsub(paste0("<", term, ">"), dat[[term]], md_template)
  }

  # Authors and Affiliations
  md_template <- gsub("<Authors_And_Affiliations>", paste(dat$Authors_And_Affiliations[[1]], collapse = "</br> "), md_template)

  # Provenance Table
  # TODO: Replace placeholders
  md_template <- gsub(
    "<Provenance_Hash_Table>",
    paste0(
      paste("", "Title Placeholder", "Link Placeholder", dat$Provenance_Hashes[[1]], dat$Provenance_Reasons[[1]], "", sep = "|"),
      collapse = "\n"
    ),
    md_template
  )

  # Render and return
  html <- markdown::markdownToHTML(text = paste0(md_template, collapse = "\n"), fragment.only = TRUE)
  Encoding(html) <- "UTF-8"
  return(HTML(html))
}

buildValidationMarkdown <- function(dat, phe_title) {

  # Read template
  md_template <- paste0(
    readLines(file.path("data", "Validation_Submission_Template.md")),
    collapse = "\n"
  )
  
  # Perform direct substitutions on most variables
  for (term in names(dat)[!(names(dat) %in% c("Title", "Validators_And_Affiliations"))]) {
    md_template <- gsub(paste0("<", term, ">"), dat[[term]], md_template)
  }

  # Title
  md_template <- gsub("<Title>", phe_title, md_template)

  # Validators and Affiliations
  md_template <- gsub("<Validators_And_Affiliations>", paste(dat$Validators_And_Affiliations[[1]], collapse = "</br> "), md_template)

  # Return rendered md file
  html <- markdown::markdownToHTML(text = paste0(md_template, collapse = "\n"), fragment.only = TRUE)
  Encoding(html) <- "UTF-8"
  return(HTML(html))
}

# Server Definition
shinyServer(function(input, output) {
  
  open(con <- url("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/Gold%20Standard/Index.rds"))
  gold <- readRDS(con)
  close(con)

  #Unpack into the phenotype and validation datasets
  phe <- gold$Phenotype
  val <- gold$Validation
  
  getFilteredChoices <- function() {

    # Begin with all phenotypes selected
    filter_logic <- list(rep(TRUE, nrow(phe)))

    # CDM Dependencies
    if (!("Conditions" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Conditions != "Yes"))
    }

    if (!("Drug Exposures" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Drug_Exposures != "Yes"))
    }

    if (!("Labs" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Labs != "Yes"))
    }

    if (!("Measurements" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Measurements != "Yes"))
    }

    if (!("Notes NLP" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Notes_NLP != "Yes"))
    }

    if (!("Observations" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Observations != "Yes"))
    }

    if (!("Procedures" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Procedures != "Yes"))
    }

    if (!("Visits" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Visits != "Yes"))
    }

    # Demographic Dependencies
    if (!("Age" %in% input$picker_demographics)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Age_Category != "Yes"))
    }

    if (!("Sex" %in% input$picker_demographics)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Gender != "Yes"))
    }

    # Modalities
    if (!("Rule-based (Heuristic)" %in% input$picker_modality)) {
      filter_logic <- c(filter_logic, list(phe$Modality != "Rule-Based"))
    }

    if (!("Computable (Algorithmic)" %in% input$picker_modality)) {
      filter_logic <- c(filter_logic, list(phe$Modality != "Computable"))
    }

    # Validation thresholds
    filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_Sensitivity), 0, phe$Avg_Sensitivity) >= input$knob_sensitivity / 100))
    filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_Specificity), 0, phe$Avg_Specificity) >= input$knob_specificity / 100))
    filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_PPV), 0, phe$Avg_PPV) >= input$knob_ppv / 100))
    filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_NPV), 0, phe$Avg_NPV) >= input$knob_npv / 100))

    # Combine logic
    filter_idx <- Reduce("&", filter_logic)
    phe_sub <- phe[filter_idx, ]

    # Return names of applicable phenotypes
    return(c("", phe_sub$Title))
  }

  # Filtered Phenotypes Search Bar
  output$filtered_phenotypes <- renderUI({

    # Apply filtered phenotypes
    selectizeInput("phenotype_search",
      label = "Select up to 5 phenotypes for comparison:",
      choices = getFilteredChoices(),
      selected = "",
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })

  # TODO: Avoid double call to get FilteredChoices()
  output$number_choices <- renderUI({
    h3(paste("Filters:", length(getFilteredChoices()) - 1, "out of", nrow(phe), "phenotypes selected."))
  })

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

    # Get row of phenotype and validation data that corresponds to this name
    phe_data <- subset(phe, Title == name)
    val_data <- subset(val, Hash == phe_data$Hash)
    
    # Make tabsetPanel
    return(
      tabsetPanel(
        tabPanel("Summary",
          icon = icon("clipboard"),
          fluidRow(column(12, box(status = "primary", width = NULL, buildPhenotypeMarkdown(phe_data))))
        ),

        tabPanel("Validation Sets",
          icon = icon("calculator"),
          tabsetPanel(
            tabPanel(
              "Overview", h1("Validation Overview"),
              fluidRow(column(12, box(
                status = "primary", width = NULL, title = "Validations", solidHeader = TRUE,
                h4(paste("Number of Validation Sets:", nrow(val_data)))
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
                  numericInput("set_num", "Set Number (of 5):", 1, min = 1, max = nrow(val_data), step = 1, width = 80)
                )
              )),
              fluidRow(column(12, box(status = "primary", width = NULL, buildValidationMarkdown(val_data[1, ], phe_data$Title))))
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
  
  # TODO: Consider adding refresh button to reset inputs to initial states and repull data
  
  # observeEvent(input$refreshButton, {
  # ...
  # })
  
}) # End shinyServer
