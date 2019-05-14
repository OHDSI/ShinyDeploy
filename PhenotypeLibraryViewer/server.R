# OHDSI Gold Standard Phenotype Library Viewer

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

  # Unpack into the phenotype and validation datasets
  phe <- gold$Phenotype
  val <- gold$Validation

  getFilteredChoices <- function() {

    # TODO: Fix persistent filter values; if a filter is deselected and others remain, its value persists but should be reset to NULL

    # The following lines print will be useful for debugging this:
    # print("Getting filtered choices:")
    # print(input$knob_sensitivity)
    # print(input$knob_specificity)
    # print(input$knob_ppv)
    # print(input$knob_npv)
    # print(input$knob_accuracy)
    # print(input$knob_f1score)

    # Begin with all phenotypes selected
    filter_logic <- list(rep(TRUE, nrow(phe)))

    # CDM Dependencies
    if (!("Conditions" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Conditions != "Yes"))
    }

    if (!("Drug Exposures" %in% input$picker_cdm)) {
      filter_logic <- c(filter_logic, list(phe$Uses_Drug_Exposures != "Yes"))
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

    # Metric filters, if used
    if (isTruthy(input$knobFilters)) {
      if (!is.null(input$knob_sensitivity)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_Sensitivity), 0, phe$Avg_Sensitivity) >= input$knob_sensitivity / 100))
      }

      if (!is.null(input$knob_specificity)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_Specificity), 0, phe$Avg_Specificity) >= input$knob_specificity / 100))
      }

      if (!is.null(input$knob_ppv)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_PPV), 0, phe$Avg_PPV) >= input$knob_ppv / 100))
      }

      if (!is.null(input$knob_npv)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_NPV), 0, phe$Avg_NPV) >= input$knob_npv / 100))
      }

      if (!is.null(input$knob_f1score)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_F1score), 0, phe$Avg_F1score) >= input$knob_f1score / 100))
      }

      if (!is.null(input$knob_accuracy)) {
        filter_logic <- c(filter_logic, list(ifelse(is.nan(phe$Avg_Accuracy), 0, phe$Avg_Accuracy) >= input$knob_accuracy / 100))
      }
    }

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
      choices = sort(getFilteredChoices()),
      selected = "",
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })

  # TODO: Avoid double call to get FilteredChoices()
  output$number_choices <- renderUI({
    h3(paste("Filters:", length(getFilteredChoices()) - 1, "out of", nrow(phe), "phenotypes selected."))
  })

  makeSingleKnobFilter <- function(filter) {
    switch(filter,
      "Accuracy" =
        knobInput("knob_accuracy", label = h4(filter), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"),
      "F1 Score" =
        knobInput("knob_f1score", label = h4(filter), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"),
      "Negative Predictive Value" =
        knobInput("knob_npv", label = h4("NPV"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"),
      "Positive Predictive Value" =
        knobInput("knob_ppv", label = h4("PPV"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"),
      "Sensitivity" =
        knobInput("knob_sensitivity", label = h4(filter), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"),
      "Specificity" =
        knobInput("knob_specificity", label = h4(filter), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%")
    )
  }

  # Build filter bank
  output$filter_bank <- renderUI({
    if (length(input$knobFilters) == 0) {
      h6("No metric filters are in use.")
    } else { # Else populate the filter bank with the filters the user selected
      switch(length(input$knobFilters),
        fluidRow(
          column(width = 12, makeSingleKnobFilter(input$knobFilters[1]))
        ),
        fluidRow(
          column(width = 6, makeSingleKnobFilter(input$knobFilters[1])),
          column(width = 6, makeSingleKnobFilter(input$knobFilters[2]))
        ),
        fluidRow(
          column(width = 4, makeSingleKnobFilter(input$knobFilters[1])),
          column(width = 4, makeSingleKnobFilter(input$knobFilters[2])),
          column(width = 4, makeSingleKnobFilter(input$knobFilters[3]))
        ),
        fluidPage(
          fluidRow(
            column(width = 6, makeSingleKnobFilter(input$knobFilters[1])),
            column(width = 6, makeSingleKnobFilter(input$knobFilters[2]))
          ),
          fluidRow(
            column(width = 6, makeSingleKnobFilter(input$knobFilters[3])),
            column(width = 6, makeSingleKnobFilter(input$knobFilters[4]))
          )
        ),
        fluidPage(
          fluidRow(
            column(width = 4, makeSingleKnobFilter(input$knobFilters[1])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[2])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[3]))
          ),
          fluidRow(
            column(width = 4, makeSingleKnobFilter(input$knobFilters[4])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[5])),
            column(width = 4, h1(""))
          )
        ),
        fluidPage(
          fluidRow(
            column(width = 4, makeSingleKnobFilter(input$knobFilters[1])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[2])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[3]))
          ),
          fluidRow(
            column(width = 4, makeSingleKnobFilter(input$knobFilters[4])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[5])),
            column(width = 4, makeSingleKnobFilter(input$knobFilters[6]))
          )
        )
      ) # end switch
    } # end else
  })

  # Inspect Tab
  output$inspect_tab <- renderUI({
    box(
      title = "Metric Distributions", status = "warning", solidHeader = TRUE, width = NULL,
      renderPlot(
        ggplot(
          getDummyValidationPlotData("dummy_name"), aes(x = Metric, y = Values, fill = Name)
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
    set.seed(nchar(name))
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
  getDummyValidationPlotData <- function(name) {
    set.seed(nchar(name))
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

  ### Menu Items

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

  # Function to make one item of the validation tab in the navbar menu
  makeValidationSetTabs <- function(val_data, pheTitle) {
    return(
      lapply(1:nrow(val_data), function(x) {
        tabPanel(
          paste0("Set #", x),
          fluidRow(column(12, box(
            status = "primary", width = NULL,
            buildValidationMarkdown(val_data[x, ], pheTitle)
          )))
        )
      })
    )
  }

  makeValidationOverviewTab <- function(val_data, name) {
    return(
      tabPanel(
        title = "Overview", value = paste0("overview_", name),
        h1("Validation Overview"),
        fluidRow(column(12, box(
          status = "primary", width = NULL, title = "Validations", solidHeader = TRUE,
          h4(paste("Number of Validation Sets:", nrow(val_data)))
        ))),
        h3("More to come...")
      )
    )
  }
  
  # Build the graph for the connected components of the currently selected phenotype
  buildVisNetwork <- function(phe_data) {
    
    # Pull data for the current cluster only
    current_cluster <- phe[phe$Hash == phe_data$Hash, "Graph_Cluster"]
    df_cluster <- phe[phe$Graph_Cluster == current_cluster, c("Hash","Title","Provenance_Reasons","Provenance_Hashes")]
    
    # Before unnesting, it's important for empty lists to hold a value, or else it will be dropped from unnest()
    # TODO: Revisit this workaround when "drop" issue is resolved:
    # https://github.com/tidyverse/tidyr/issues/358
    
    # Terminal Nodes - These disappear when unnesting so must be considered separately
    isEmpty <- sapply(df_cluster$Provenance_Reasons, function(x) {length(x) == 0})
    terminal_nodes <- df_cluster[isEmpty,]
    if (nrow(terminal_nodes) > 0) {
      terminal_nodes$Provenance_Hashes <- ""
      terminal_nodes$Provenance_Reasons <- ""
    }
    
    # Get non-terminal nodes
    connected_nodes <- unnest(df_cluster[!isEmpty,])
    connected_nodes$Provenance_Hashes <- unlist(connected_nodes$Provenance_Hashes)
    connected_nodes$Provenance_Reasons <- unlist(connected_nodes$Provenance_Reasons)
    
    # Reconnect terminal nodes with non-terminal nodes
    df_cluster <- as.data.frame(rbind(terminal_nodes, connected_nodes))
    
    # Nodes dataset
    nodes <- unique(data.frame(id = df_cluster$Hash, 
                               group = "Not_Selected", 
                               label = df_cluster$Title, 
                               title = paste0("Hash: \n", df_cluster$Hash),
                               stringsAsFactors = FALSE)
    )
    
    # Sort to display names in alphabetical order on dropdown menu
    nodes <- nodes[order(nodes$label),]
    
    # Distinguish the selected node from the others via the group property
    nodes$group[which(nodes$id == phe_data$Hash)] <- "Selected"
    
    # Edges dataset
    edges <- data.frame(from = df_cluster$Provenance_Hashes, 
                        to = df_cluster$Hash, 
                        title = df_cluster$Provenance_Reasons)
    
    # Create visNetwork
    visNetwork(nodes, edges, width = "100%") %>%
      visNodes(shape = "dot") %>%
      visEdges(arrows ="to") %>%                               
      visGroups(groupname = "Not_Selected", color = "darkblue") %>%
      visGroups(groupname = "Selected", color = "red") %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = TRUE,
                 selectedBy = "group",
                 collapse = TRUE) 
    # These might be useful down the road:
    # %>% visHierarchicalLayout()
    # %>% visConfigure()
  } # End buildVisNetwork

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

        tabPanel("Validation",
          # TODO: Avoid checking nrow(val_data) > 0 twice
          icon = icon(ifelse(nrow(val_data) > 0, "calculator", "exclamation-triangle ")),

          # Check if validation data exists to populate this tab with
          if (nrow(val_data) > 0) {
            navbarPage("",

              # Validation overview tab
              makeValidationOverviewTab(val_data, phe_data$Title),

              # Individual validation set selector
              do.call("navbarMenu", c(makeValidationSetTabs(val_data, phe_data$Title), list(title = "Validation Sets")))
            )
          } else {
            fluidRow(column(12, box(
              status = "danger", width = NULL, title = "Not Validated", solidHeader = TRUE,
              h4("This phenotype has no validation sets associated with it.")
            )))
          }
        ),
        # Provenance Tab
        tabPanel(
          title = "Provenance", icon = icon("project-diagram"),
          fluidRow(
            box(title = "Provenance Diagram", status = "primary", width = NULL,
                renderVisNetwork(
                  buildVisNetwork(phe_data)
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
