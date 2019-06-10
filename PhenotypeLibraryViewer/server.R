# OHDSI Gold Standard Phenotype Library Viewer

####################################################################################################################################
# Utility Functions
####################################################################################################################################

# Complete the author markdown template, given the chapter data
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
} # End buildPhenotypeMarkdown

# Complete the validation markdown template, given the chapter data
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
} # End buildValidationMarkdown

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

# Makes one item of the validation tab in the navbar menu
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

# Makes overview tab to summarize over multiple validation sets
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
  df_cluster <- phe[phe$Graph_Cluster == current_cluster, c("Hash", "Title", "Provenance_Reasons", "Provenance_Hashes")]

  # Before unnesting, it's important for empty lists to hold a value, or else it will be dropped from unnest()
  # TODO: Revisit this workaround when "drop" issue is resolved:
  # https://github.com/tidyverse/tidyr/issues/358

  # Terminal Nodes - These disappear when unnesting so must be considered separately
  isEmpty <- sapply(df_cluster$Provenance_Reasons, function(x) {
    length(x) == 0
  })
  terminal_nodes <- df_cluster[isEmpty, ]
  if (nrow(terminal_nodes) > 0) {
    terminal_nodes$Provenance_Hashes <- ""
    terminal_nodes$Provenance_Reasons <- ""
  }

  # Get non-terminal nodes
  connected_nodes <- unnest(df_cluster[!isEmpty, ])
  connected_nodes$Provenance_Hashes <- unlist(connected_nodes$Provenance_Hashes)
  connected_nodes$Provenance_Reasons <- unlist(connected_nodes$Provenance_Reasons)

  # Reconnect terminal nodes with non-terminal nodes
  df_cluster <- as.data.frame(rbind(terminal_nodes, connected_nodes))

  # Nodes dataset
  nodes <- unique(data.frame(
    id = df_cluster$Hash,
    group = "Not_Selected",
    label = df_cluster$Title,
    title = paste0("Hash: \n", df_cluster$Hash),
    stringsAsFactors = FALSE
  ))

  # Sort to display names in alphabetical order on dropdown menu
  nodes <- nodes[order(nodes$label), ]

  # Distinguish the selected node from the others via the group property
  nodes$group[which(nodes$id == phe_data$Hash)] <- "Selected"

  # Edges dataset
  edges <- data.frame(
    from = df_cluster$Provenance_Hashes,
    to = df_cluster$Hash,
    title = df_cluster$Provenance_Reasons
  )

  # Create visNetwork
  visNetwork(nodes, edges, width = "100%") %>%
    visNodes(shape = "dot") %>%
    visEdges(arrows = "to") %>%
    visGroups(groupname = "Not_Selected", color = "darkblue") %>%
    visGroups(groupname = "Selected", color = "red") %>%
    visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = TRUE,
      selectedBy = "group",
      collapse = TRUE
    )
  # These might be useful down the road:
  # %>% visHierarchicalLayout()
  # %>% visConfigure()
} # End buildVisNetwork

# Create a tabsetPanel() that contains all of the details for the selected chapter
makeChapterPage <- function(name) {

  # Subset data to the selected chapter
  phe_data <- subset(phe, Title == name)
  val_data <- subset(val, Hash == phe_data$Hash)

  # Make tabsetPanel
  return(
    tabsetPanel(

      # Summary Tab
      tabPanel("Summary",
        icon = icon("clipboard"),
        fluidRow(column(12, box(status = "primary", width = NULL, buildPhenotypeMarkdown(phe_data))))
      ),

      # Validation Tab
      tabPanel("Validation",
        # TODO: Avoid checking nrow(val_data) > 0 twice
        icon = icon(ifelse(nrow(val_data) > 0, "calculator", "exclamation-triangle ")),

        # Check if validation data exists to populate this tab with
        if (nrow(val_data) > 0) {
          navbarPage(
            "",

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
          box(
            title = "Provenance Diagram", status = "primary", width = NULL,
            renderVisNetwork(
              buildVisNetwork(phe_data)
            )
          )
        )
      )
    ) # End tabsetPanel
  ) # End return
} # End makeChapterPage

####################################################################################################################################
# shinyServer Definition
####################################################################################################################################
shinyServer(function(input, output, session) {

  # Render the menus for each selected chapter
  lapply(1:10, function(x) {
    output[[paste0("ChapterMenu_Tab", x)]] <- renderMenu({
      # Determine which chapter datatable rows are selected, if any
      rows_selected <- input$chapterTable_rows_selected

      # Only display if there are an appropriate number of chapters selected and a book selected
      if (length(rows_selected) >= x & isTruthy(input$book_search)) {
        chapterData <- subset(phe, Broad_Category_Name == input$book_search, select = "Title")
        menuItem(chapterData[rows_selected[x], ], tabName = paste0("tab", x), icon = icon("square"))
      } else {
        h1("") # Can't be NULL - using h1() to force refresh
      }
    })
  })

  # Tab rendering for all selected tabs
  lapply(1:10, function(x) {
    output[[paste0("tab", x)]] <- renderUI({
      # Only render if the appropriate number of selections has been made
      rows_selected <- input$chapterTable_rows_selected
      if (length(rows_selected) >= x) {
        chapterData <- subset(phe, Broad_Category_Name == input$book_search, select = "Title")
        makeChapterPage(chapterData[rows_selected[x], ])
      }
    })
  })

  # Renders a rule on the sidebar separator if there exist menuItems to separate
  output$conditionalHR1 <- renderUI({
    if (isTruthy(input$chapterTable_rows_selected)) {
      return(hr())
    }
  })

  # Renders the top rule of the book description if a book has been selected
  output$conditionalHR2 <- renderUI({
    if (isTruthy(input$book_search)) {
      tags$hr(style = "border-color: black;")
    }
  })

  # Renders the bottom rule of the book description if a book has been selected
  output$conditionalHR3 <- renderUI({
    if (isTruthy(input$book_search)) {
      tags$hr(style = "border-color: black;")
    }
  })

  # Render a description that described the selected phenotype (book)
  # TODO: Perhaps, these should pull in a clinical description markdown file from the folder directory
  output$bookDescription <- renderUI({
    # Render the book description only if a book has been selected
    if (isTruthy(input$book_search)) {
      h4(paste0("The clinical description for ", input$book_search, " should go here. 
              This can be stored as a markdown file in the Folder (Book) directory on GitHub."))
    }
  })

  # Function to create the datatable depending on what columns the user has selected in the chapterFilters section
  getChapterData <- function() {

    # All checked boxes will be used to determine the columns in the datatable
    # At a minimum, Title is always present - This data elements is not selectable
    selectedColumns <- c(
      "Title",
      input$metadata_boxes,
      input$times_validated_boxes,
      input$metric_boxes,
      input$source_data_boxes,
      input$demographic_boxes
    )

    chapterData <- subset(phe,
      Broad_Category_Name == input$book_search,
      select = selectedColumns
    )

    return(chapterData)
  }

  # Make a box of filters to allow for custom datatable build
  output$chapterFilters <- renderUI({

    # Render only if a book has been selected
    if (isTruthy(input$book_search)) {
      fluidRow(column(
        width = 12,
        box(
          title = "Select Table Features here",
          width = 6,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          column(
            width = 4,

            # Metadata
            prettyCheckboxGroup("metadata_boxes",
              "Chapter Metadata",
              choiceNames = c(
                "Hash",
                "Author(s)",
                "Date of Submission",
                "Algorithm Modality"
              ),
              choiceValues = c(
                "Hash",
                "Authors_And_Affiliations",
                "Date_Of_Submission",
                "Modality"
              ),
              status = "info",
              shape = "square",
              outline = TRUE,
              thick = TRUE,
              animation = "tada"
            )
          ),

          column(
            width = 4,

            # Validation Frequency (Selected by default)
            prettyCheckboxGroup("times_validated_boxes",
              "Validation Frequency:",
              choiceNames = "Times Validated",
              choiceValues = "Times_Validated",
              selected = "Times_Validated",
              status = "info",
              shape = "square",
              outline = TRUE,
              thick = TRUE,
              animation = "tada"
            ),

            # Validation Metrics
            prettyCheckboxGroup("metric_boxes",
              "Validation Metrics:",
              choiceNames = c(
                "Sensitivity",
                "Specificity",
                "Positive Predictive Value",
                "Negative Predictive Value",
                "Accuracy",
                "F1 Score"
              ),
              choiceValues = c(
                "Avg_Sensitivity",
                "Avg_Specificity",
                "Avg_PPV",
                "Avg_NPV",
                "Avg_Accuracy",
                "Avg_F1score"
              ),
              status = "info",
              shape = "square",
              outline = TRUE,
              thick = TRUE,
              animation = "tada"
            )
          ),

          # Data Dependencies
          column(
            width = 4,
            prettyCheckboxGroup("source_data_boxes",
              "Data Dependencies:",
              choiceNames = c(
                "Conditions",
                "Drugs Exposures",
                "Measurements",
                "Notes NLP",
                "Observations",
                "Procedures",
                "Visits"
              ),
              choiceValues = c(
                "Uses_Conditions",
                "Uses_Drug_Exposures",
                "Uses_Measurements",
                "Uses_Notes_NLP",
                "Uses_Observations",
                "Uses_Procedures",
                "Uses_Visits"
              ),
              status = "info",
              shape = "square",
              outline = TRUE,
              thick = TRUE,
              animation = "tada"
            ),

            # Demographic Dependencies
            prettyCheckboxGroup("demographic_boxes",
              "Demographic Dependencies:",
              choiceNames = c(
                "Gender",
                "Age"
              ),
              choiceValues = c(
                "Uses_Gender",
                "Uses_Age_Category"
              ),
              status = "info",
              shape = "square",
              outline = TRUE,
              thick = TRUE,
              animation = "tada"
            )
          )
        )
      ))
    }
  }) # End chapterFilters

  # Render datatable of chapter selections
  output$chapterSelect <- renderUI({

    # Render the chapter menu only if a book has first been selected
    if (isTruthy(input$book_search)) {

      # Acquire chapter data given the currently selected book
      chapterData <- getChapterData()

      # Display datatable
      fluidRow(
        column(
          width = 12,
          box(
            title = NULL,
            width = NULL,
            status = "warning",
            h3("Select one or more Chapters from the Book."),
            h5('A "Chapter" is a cohort definition (and associated metadata) intended to approximate the above health state (book).'),
            h5("Selected chapters will appear on the left. Click to view chapter details."),
            datatable(
              data = chapterData,
              # If there's only one entry, then disable the filter
              filter = ifelse(nrow(chapterData) > 1, "top", "none"),
              elementId = "chapterTable",
              class = "hover",
              rownames = FALSE,
              options = list(
                pageLength = 30,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollCollapse = TRUE,
                autoWidth = FALSE
              )
            )
          )
        )
      )
    }
  }) # End chapterSelect
}) # End shinyServer
