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
  
  # Define variables to be replaced. If NA, then replace with string NA
  Book_Description <- dat$coh_book_clinical_description ; Book_Description <- ifelse(is.na(Book_Description), "NA", Book_Description)
  
  # For tracking the data elements at the time of call
  df_debug <<- dat
  
  md_template <- gsub(pattern = "<Phenotype_Title>", replacement = dat$Book, x = md_template)
  md_template <- gsub(pattern = "<Phenotype_Description>", replacement = Book_Description, x = md_template)
  
  md_template <- gsub(pattern = "<Phenotype_Title_Link>", replacement = gsub(" ", "%20", dat$Book, fixed = TRUE), x = md_template)
  md_template <- gsub(pattern = "<Cohort_Definition_Title_Link>", replacement = gsub(" ", "%20", dat$coh_chapter_title, fixed = TRUE), x = md_template)
  
  md_template <- gsub(pattern = "<Title>", replacement = dat$coh_chapter_title, x = md_template)
  md_template <- gsub("<Authors_And_Affiliations>", dat$Authors_And_Affiliations, x = md_template)
  md_template <- gsub("<Date_Of_Submission>", as.Date(strsplit(dat[["timestamp"]], "-")[[1]][1], "%Y%m%d"), x = md_template)
  md_template <- gsub("<Modality>", dat[["coh_phenotype_modality"]], x = md_template)
  
  md_template <- gsub(pattern = "<Development_Methodology>", replacement = dat$coh_development_process, x = md_template)

  # Perform direct substitutions on most variables
  #for (term in names(dat)[!(names(dat) %in% c("Authors_And_Affiliations", "Provenance_Hashes", "Provenance_Reasons"))]) {
  #  md_template <- gsub(paste0("<", term, ">"), dat[[term]], md_template)
  #}

  # Authors and Affiliations
  #md_template <- gsub("<Authors_And_Affiliations>", paste(dat$Authors_And_Affiliations[[1]], collapse = "</br> "), md_template)

  # Provenance Table
  # TODO: Replace placeholders
  #md_template <- gsub(
  #  "<Provenance_Hash_Table>",
  #  paste0(
  #    paste("", "Title Placeholder", "Link Placeholder", dat$Provenance_Hashes[[1]], dat$Provenance_Reasons[[1]], "", sep = "|"),
  #    collapse = "\n"
  #  ),
  #  md_template
  #)

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
  #for (term in names(dat)[!(names(dat) %in% c("Title", "Validators_And_Affiliations"))]) {
  #  md_template <- gsub(paste0("<", term, ">"), dat[[term]], md_template)
  #}

  # Title
  #md_template <- gsub("<Title>", phe_title, md_template)

  # Validators and Affiliations
  #md_template <- gsub("<Validators_And_Affiliations>", paste(dat$Validators_And_Affiliations[[1]], collapse = "</br> "), md_template)

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
  current_cluster <- phe[phe$coh_chapter_title == phe_data$coh_chapter_title, "Graph_Cluster"]
  df_cluster <- phe[phe$Graph_Cluster == current_cluster, c("coh_chapter_title", "provenance_book", "provenance_chapter", "provenance_rationale")]

  # Before unnesting, it's important for empty lists to hold a value, or else it will be dropped from unnest()
  # TODO: Revisit this workaround when "drop" issue is resolved:
  # https://github.com/tidyverse/tidyr/issues/358

  # Terminal Nodes - These disappear when unnesting so must be considered separately
  #isEmpty <- sapply(df_cluster$Provenance_Reasons, function(x) {
  #  length(x) == 0
  #})
  #terminal_nodes <- df_cluster[isEmpty, ]
  #if (nrow(terminal_nodes) > 0) {
  #  terminal_nodes$Provenance_Hashes <- ""
  #  terminal_nodes$Provenance_Reasons <- ""
  #}
  bool <- c()
  for (i in 1:nrow(df_cluster)){
    if(is.na(df_cluster[i,"provenance_book"])){
      bool <- c(bool,TRUE)
      df_cluster[i,"provenance_book"] <- ""
      df_cluster[i,"provenance_chapter"] <- ""
      df_cluster[i,"provenance_rationale"] <- ""
    } else {
      bool <- c(bool,FALSE)
    }
  }
  
  terminal_nodes <- df_cluster[bool,]

  # Get non-terminal nodes
  connected_nodes <- unnest(df_cluster[!bool, ])
  connected_nodes$provenance_chapter <- unlist(connected_nodes$provenance_chapter)
  connected_nodes$provenance_rationale <- unlist(connected_nodes$provenance_rationale)

  # Reconnect terminal nodes with non-terminal nodes
  df_cluster <- as.data.frame(rbind(terminal_nodes, connected_nodes))
  
  df_cluster$coh_chapter_title <- unlist(df_cluster$coh_chapter_title)
  df_cluster$provenance_book <- unlist(df_cluster$provenance_book)
  df_cluster$provenance_chapter <- unlist(df_cluster$provenance_chapter)
  df_cluster$provenance_rationale <- unlist(df_cluster$provenance_rationale)

  # Nodes dataset
  nodes <- unique(data.frame(
    id = df_cluster$coh_chapter_title,
    group = "Not_Selected",
    label = df_cluster$coh_chapter_title,
    title = paste0("Title: \n", df_cluster$coh_chapter_title),
    stringsAsFactors = FALSE
  ))

  # Sort to display names in alphabetical order on dropdown menu
  nodes <- nodes[order(nodes$label), ]

  # Distinguish the selected node from the others via the group property
  nodes$group[which(nodes$id == phe_data$coh_chapter_title)] <- "Selected"

  # Edges dataset
  edges <- data.frame(
    from = df_cluster$provenance_chapter,
    to = df_cluster$coh_chapter_title,
    title = df_cluster$provenance_rationale
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

# Create a tabsetPanel() that contains all of the details for the selected book/chapter
makeChapterPage <- function(selected_book, selected_chapter){
  
  # Subset data to the selected chapter
  phe_data <- subset(phe, (Book == selected_book) & (coh_chapter_title == selected_chapter))
  val_data <- subset(val, (validation_book_selection == selected_book) & (validation_chapter_selection == selected_chapter))

  # Make tabsetPanel
  return(
    tabsetPanel(

      # Summary Tab
      tabPanel("Summary",
        icon = icon("clipboard"),
        fluidRow(column(12, box(status = "primary", width = NULL, buildPhenotypeMarkdown(phe_data))))
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
            makeValidationOverviewTab(val_data, phe_data$coh_chapter_title),

            # Individual validation set selector
            do.call("navbarMenu", c(makeValidationSetTabs(val_data, phe_data$coh_chapter_title), list(title = "Validation Sets")))
          )
        } else {
          fluidRow(column(12, box(
            status = "danger", width = NULL, title = "Not Validated", solidHeader = TRUE,
            h4("This phenotype has no validation sets associated with it.")
          )))
        }
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
      if (length(rows_selected) >= x) { 
        
        selectedChapterData <- phe[rows_selected[x], ]
        menuItem(selectedChapterData$coh_chapter_title, tabName = paste0("tab", x), icon = icon("square"))
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
      selectedChapterData <- phe[rows_selected[x], ]
      
      if (length(rows_selected) >= x) {
        makeChapterPage(selectedChapterData$Book, selectedChapterData$coh_chapter_title)
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
      "Book",
      "coh_chapter_title",
      input$metadata_boxes,
      input$frequency_boxes,
      input$owner_metric_boxes
    )

    chapterData <- subset(phe,
                          select = selectedColumns)
    names(chapterData)[1:2] <- c("Phenotype", "Cohort Definition")

    return(chapterData)
  }

  # Make a dropdownButton of filters to allow for custom datatable build
  output$chapterFilters <- renderUI({
    
    dropdownButton(
      
      fluidRow(
        column(
          width = 4,
          
          # Metadata
          prettyCheckboxGroup("metadata_boxes",
                              "Chapter Metadata",
                              choiceNames = c(
                                "Author and Affiliation(s)",
                                "Date of Submission",
                                "Algorithm Modality",
                                "OHDSI Forum Handle",
                                "ORCID"
                              ),
                              choiceValues = c(
                                "Authors_And_Affiliations",
                                "timestamp",
                                "coh_phenotype_modality",
                                "ohdsi_forum_handle",
                                "orcid"
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
          prettyCheckboxGroup("frequency_boxes",
                              "Frequency of Use:",
                              choiceNames = c(
                                "Times Validated",
                                "Times Cited",
                                "Times Characterized"
                              ),
                              choiceValues = c(
                                "Times_Validated",
                                "Times_Cited",
                                "Times_Characterized"
                              ),
                              selected = "Times_Validated",
                              status = "info",
                              shape = "square",
                              outline = TRUE,
                              thick = TRUE,
                              animation = "tada"
          ),
          
          # Validation Metrics
          prettyCheckboxGroup("owner_metric_boxes",
                              "Owner Validation Metrics:",
                              choiceNames = c(
                                "CDM Version",
                                "Vocabulary Version",
                                "Validation Modality",
                                "Sensitivity",
                                "Specificity",
                                "PPV",
                                "NPV",
                                "Accuracy",
                                "F1 Score",
                                "Inconclusive"
                              ),
                              choiceValues = c(
                                "coh_cdm_version",
                                "coh_vocab_version",
                                "coh_validation_modality",
                                "coh_sensitivity",
                                "coh_specificity",
                                "coh_ppv",
                                "coh_npv",
                                "coh_accuracy",
                                "coh_f1",
                                "coh_inconclusive"
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
          prettyCheckboxGroup("outside_val_boxes",
                              "Outside Validation Metrics:",
                              choiceNames = c(
                                "Sensitivity",
                                "Specificity",
                                "PPV",
                                "NPV",
                                "Accuracy",
                                "F1 Score",
                                "Inconclusive"
                              ),
                              choiceValues = c(
                                "avg_sensitivity",
                                "avg_specificity",
                                "avg_ppv",
                                "avg_npv",
                                "avg_acc",
                                "avg_f1",
                                "avg_inc"
                              ),
                              status = "info",
                              shape = "square",
                              outline = TRUE,
                              thick = TRUE,
                              animation = "tada"
          )
        )
      )
      
      , circle = TRUE, 
      status = "danger", 
      icon = icon("gear"), 
      width = "1000px",
      tooltip = tooltipOptions(title = "Add/Remove Features")
    )
    
  })

  # Render datatable of chapter selections
  output$chapterSelect <- renderUI({

      # Acquire chapter data given the currently selected book
      chapterData <- getChapterData()
            
      fluidRow(
        datatable(
              data = chapterData,
              filter = "top",
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
  }) # End chapterSelect
}) # End shinyServer
