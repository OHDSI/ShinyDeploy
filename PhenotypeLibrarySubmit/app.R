####################################################################################################################################
# OHDSI Gold Standard Phenotype Library Submission Application
####################################################################################################################################

####################################################################################################################################
# Global
####################################################################################################################################

# Toggle Auth0 - Toggling it off is helpful for development, running locally, and/or debugging
USING_AUTH0 <- TRUE

# Libraries
library(auth0)
library(digest)
library(DT)
library(googledrive)
library(httr)
library(jsonlite)
library(plyr)
library(rhandsontable)
library(rlist)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)

# Enable bookmarking on the server
enableBookmarking(store = "server")

# Point to the Auth0 config file
options(auth0_config_file = "/data/_auth0.yml")

# Authenticate to allow for data export to Google Drive
drive_auth(service_token = "/data/phrasal-period-244717-6a06518b8dc0.json")

# Query GitHub to retrieve official books and chapters
# TODO: In the future, this can be replaced by directly loading the Index file residing on the server.
req <- GET("https://api.github.com/repos/OHDSI/PhenotypeLibrary/git/trees/master?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)

# Construct book/chapter dataframe for reference to the official state of books and chapters in the library
books_and_chapters <- grep("^OHDSI Gold Standard Phenotype Library/Books/[A-Za-z0-9 _]+/[A-Za-z0-9 _]+$", filelist, value = TRUE)
phe <- as.data.frame(str_split_fixed(books_and_chapters, "/", 4)[, 3:4])
names(phe) <- c("Book", "Chapter")

# Number of unique chapters per book
counts <- aggregate(Chapter ~ Book, phe, function(x) length(unique(x)))

# Provenance dataframe for nested chapters and books
prov_check <- phe
prov_check$used <- FALSE

# For display purposes, sub any underscores for spaces
phe$Book <- gsub("_", " ", phe$Book)

####################################################################################################################################
# Constants
####################################################################################################################################

# Books that are part of the official record
ACCEPTED_BOOKS <- sort(unique(phe$Book))

# Books that have been created in the staging area but have yet to be accepted
PROPOSED_BOOKS <- sort(
  drive_ls(path = file.path(
    "OHDSI Gold Standard Phenotype Library",
    "Books"
  ))$name
)

# Give priority to accepted books when there are duplicates
PROPOSED_BOOKS <- PROPOSED_BOOKS[!(PROPOSED_BOOKS %in% ACCEPTED_BOOKS)]

# Constant: ORCID pattern used to check ORCID ID validity in the author tables
REGEX_ORCID <- "([0-9]{4})[-]([0-9]{4})[-]([0-9]{4})[-]([0-9X]{4})"

# Constant: Pattern used to check for extraneous characters in form fields -- These are the allowable characters
REGEX_TITLE <- "[^a-zA-Z0-9 ,;_-]"

# Constant: Therapeutic areas choice list
THERAPEUTIC_AREAS <- c(
  "Blood and lymphatic system disorders",
  "Cardiac disorders",
  "Congenital, familial and genetic disorders",
  "Ear and labyrinth disorders",
  "Endocrine disorders",
  "Eye disorders",
  "Gastrointestinal disorders",
  "General disorders and administration site conditions",
  "Hepatobiliary disorders",
  "Immune system disorders",
  "Infections and infestations",
  "Injury, poisoning and procedural complications",
  "Metabolism and nutrition disorders",
  "Musculoskeletal and connective tissue disorders",
  "Neoplasms benign, malignant and unspecified (incl cysts and polyps)",
  "Nervous system disorders",
  "Pregnancy, puerperium and perinatal conditions",
  "Psychiatric disorders",
  "Renal and urinary disorders",
  "Reproductive system and breast disorders",
  "Respiratory, thoracic and mediastinal disorders",
  "Skin and subcutaneous tissue disorders",
  "Social circumstances",
  "Surgical and medical procedures",
  "Vascular disorders"
)

# Constant: All cohort form fields that have an input in the cohort definition submission form
COH_FIELDS_ALL <- c(
  "coh_book_exist",
  "coh_existing_books",
  "coh_book_title",
  "coh_book_clinical_description",
  "coh_therapeutic_areas_book",
  "coh_tags_book",
  "coh_chapter_title",
  "coh_definition_description",
  "coh_development_process",
  "coh_phenotype_modality",
  "coh_therapeutic_areas_chapter",
  "coh_tags_chapter",
  "coh_provenance",
  "coh_previous_validation",
  "coh_valid_proc_desc",
  "coh_valid_data_desc",
  "coh_cdm_version",
  "coh_vocab_version",
  "coh_validation_modality",
  "coh_true_pos",
  "coh_true_neg",
  "coh_false_pos",
  "coh_false_neg",
  "coh_inconclusive",
  "coh_add_comms"
)

# Constant: All validator form fields
VAL_FIELDS_ALL <- c(
  "validation_book_selection",
  "validation_chapter_selection",
  "val_valid_proc_desc",
  "val_valid_data_desc",
  "val_cdm_version",
  "val_vocab_version",
  "val_validation_modality",
  "val_true_pos",
  "val_true_neg",
  "val_false_pos",
  "val_false_neg",
  "val_inconclusive",
  "val_add_comms"
)

# Constant: All citation form fields
CIT_FIELDS_ALL <- c(
  "citation_book_selection",
  "citation_chapter_selection",
  "citation_select",
  "citation_data"
)

# Constant: All characterization form fields
CHA_FIELDS_ALL <- c(
  "characterization_book_selection",
  "characterization_chapter_selection",
  "characterization_data_description",
  "char_cdm_version",
  "char_vocab_version"
)

# Constant: Permissible File Types
FILE_TYPES <- c(
  "image/png",
  "image/jpeg",
  "text/csv",
  "application/msword",
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "application/epub+zip",
  "text/html",
  "text/javascript",
  "application/json",
  "applicaton/ld+json",
  "application/vnd.oasis.opendocument.spreadsheet",
  "application/vnd.oasis.opendocument.text",
  "application/pdf",
  "application/vnd.ms-powerpoint",
  "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  "application/rtf",
  "application/x-sh",
  "application/x-tar",
  "text/plain",
  "application/vnd.ms-excel",
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "application/zip"
)

# Constant: Path on server to where bookmarks are being stored
BOOKMARK_PATH <<- "/gspl-bookmarks"

# Constant: Dataframe to initialize authors datatables
AUTHORS_BLANK <- data.frame(
  Name = c(""),
  Email = c(""),
  Institution = c(""),
  Position = c(""),
  Handle = c(""),
  ORCID = c(""),
  stringsAsFactors = FALSE
)

# Help text for popovers
CONTRIBUTOR_INFORMATION_HELP_TEXT <- "Please enter data for all who contributed to creating this definition. The Handle refers to the OHDSI forum username. For the ORCID iD, please refer to https://orcid.org/. Both the Handle and ORCID iD are optional."

VALIDATION_CONTRIBUTOR_INFORMATION_HELP_TEXT <- "Please enter data for all who contributed to creating this validation. The Handle refers to the OHDSI forum username. For the ORCID iD, please refer to https://orcid.org/. Both the Handle and ORCID iD are optional."

CITATION_CONTRIBUTOR_INFORMATION_HELP_TEXT <- "Please enter this information about the individual(s) filling out this citation form. The Handle refers to the OHDSI forum username. For the ORCID iD, please refer to https://orcid.org/. Both the Handle and ORCID iD are optional."

CHARACTERIZATION_CONTRIBUTOR_INFORMATION_HELP_TEXT <- "Please enter data for all who contributed to creating this cohort characterization. The Handle refers to the OHDSI forum username. For the ORCID iD, please refer to https://orcid.org/. Both the Handle and ORCID iD are optional."

BOOK_INFORMATION_HELP_TEXT <- "The library is organized in a book and chapter framework. A Book in the phenotype library represents a conceptual clinical health state, while a Chapter is an implementation intended to capture individuals who have that health state. For example, rheumatoid arthritis could be a book, with two distinct (e.g. sensitive and specific) implementations as separate chapters within that book."

CHAPTER_INFORMATION_HELP_TEXT <- BOOK_INFORMATION_HELP_TEXT

PROVENANCE_HELP_TEXT <- "Provenance establishes linkages between cohort definitions. This could be due to a new version of an existing definition, or it could be used to point to other phenotypes as a reference. When provenance is established, a user will be informed of linked phenotypes when they are viewing this phenotype."

VALIDATION_HELP_TEXT <- "If you validated your definition at your site, you have the opportunity to provide these details at this time. If you used PheValuator or APHRODITE, you can simply upload the corresponding output file. For a manual approach to validation (e.g. chart review), please fill out all of the cells you have data for, and enter '0' for cells in which you had no counts."

SUPPORTING_HELP_TEXT <- "Please submit any documentation you may have, such as the following: phenotype algorithm flowchart, written phenotype algorithm, data defintions, and/or model parameters/features."

ADDITIONAL_COMMENTS_HELP_TEXT <- "Please use this space to add any information about your cohort definition not covered above that you believe others using your definition could find useful."

LIBRARY_RESOURCE_HELP_TEXT <- "Please select the book (phenotype) and chapter (cohort definition) you would like to reference. You may select from definitions that have been accepted into the library or definitions that are currently under review."

VALIDATION_PROCEDURE_HELP_TEXT <- "Please enter the details of your validation procedure. For example, if you did a manual chart review, how many were involved in the review? What methodology did you use to discern the veracity of the labels?"

VALIDATION_DATASET_HELP_TEXT <- "Please describe generally the type of data you used (Claims vs. EMR, sample size, dates, etc.) for validation. Please also specify which version of the OMOP CDM and Standardized Vocabulary you used at the time of validation."

PERFORMANCE_METRICS_HELP_TEXT <- "If you used PheValuator or APHRODITE, you can simply upload the corresponding output file. For a manual approach to validation (e.g. chart review), please fill out all of the cells you have data for, and enter 0 for cells in which you had no counts."

ADDITIONAL_VALIDATION_COMMENTS_HELP_TEXT <- "Please use this space to add any information about this validation not covered above you believe others could find useful."

CITATION_LIBRARY_RESOURCE_HELP_TEXT <- LIBRARY_RESOURCE_HELP_TEXT

CHARACTERIZATION_LIBRARY_RESOURCE_HELP_TEXT <- LIBRARY_RESOURCE_HELP_TEXT

CHARACTERIZATION_DATA_DESCRIPTION_HELP_TEXT <- "Please describe generally the type of data you used (Claims vs. EMR, sample size, dates, etc.) for your cohort characterization. Please also specify which version of the OMOP CDM and Standardized Vocabulary you used at the time of your characterization."

PHENOTYPE_STABILITY_HELP_TEXT <- "Optionally, you may upload a plot showing incidence over time. This can help a user understand how temporally stable the cohort definition is."

FEATURE_EXTRACTION_HELP_TEXT <- "This section is the essence of the characterization, which aims to quantify what types of individuals the cohort definition is selecting (e.g. demographics by case and control). Please refer to the link below for more details."

# Create temp directory structure for holding this session's submissions
responsesDir <- tempdir()

Provenance_DF <- data.frame("Book" = character(0), "Chapter" = character(0), "Rationale" = character(0))

# Override of Shiny's save function to save ID as a global so it can be retrieved internally
saveShinySaveState <- function(state) {
  id <- paste(u_uid, createUniqueId(12), cur_savename, sep = "%")
  
  # A function for saving the state object to disk, given a directory to save to.
  saveState <- function(stateDir) {
    
    # Allow user-supplied onSave function to do things like add state$values, or
    # save data to state dir.
    if (!is.null(state$onSave)) {
      isolate(state$onSave(state))
    }
    
    # Serialize values, possibly saving some extra data to stateDir
    exclude <- c(state$exclude, "._bookmark_")
    inputValues <- serializeReactiveValues(state$input, exclude, state$dir)
    saveRDS(inputValues, file.path(BOOKMARK_PATH, paste0(id, ".rds")))
    
    # If values were added, save them also.
    if (length(state$values) != 0) {
      saveRDS(state$values, file.path(stateDir, "values.rds"))
    }
  }

  # Pass the saveState function to the save interface function, which will
  # invoke saveState after preparing the directory.

  # Look for a save.interface function. This will be defined by the hosting
  # environment if it supports bookmarking.
  saveInterface <- getShinyOption("save.interface")

  if (is.null(saveInterface)) {
    if (inShinyServer()) {
      # We're in a version of Shiny Server/Connect that doesn't have
      # bookmarking support.
      saveInterface <- function(id, callback) {
        stop("The hosting environment does not support saved-to-server bookmarking.")
      }
    } else {
      # We're running Shiny locally.
      saveInterface <- saveInterfaceLocal
    }
  }

  saveInterface(id, saveState)

  paste0("_state_id_=", encodeURIComponent(id))
}

# See: https://stackoverflow.com/questions/23279904/modifying-an-r-package-function-for-current-r-session-assigninnamespace-not-beh
tmpfun <- get("saveShinySaveState", envir = asNamespace("shiny"))
environment(saveShinySaveState) <- environment(tmpfun)
attributes(saveShinySaveState) <- attributes(tmpfun)
assignInNamespace("saveShinySaveState", saveShinySaveState, ns = "shiny")

####################################################################################################################################
# UI
####################################################################################################################################

# OHDSI Gold Standard Phenotype Library Viewer

# UI Definition
ui <-
  function(request) {
    fluidPage(

      # shinyjs must be initialized with a call to useShinyjs() in the app's ui.
      shinyjs::useShinyjs(),

      # CSS for app style settings
      includeCSS("www/styles.css"),

      # Dashboard Page
      dashboardPage(
        title = "OHDSI Gold Standard Phenotype Library",
        skin = "yellow",

        # Dashboard Header
        dashboardHeader(title = tags$img(src = "https://www.ohdsi.org/web/wiki/lib/exe/fetch.php?media=wiki:logo.png"), titleWidth = 280),

        # Dashboard Sidebar
        dashboardSidebar(
          width = 280,
          sidebarMenu(
            style = "position:fixed;width:inherit;",
            br(),
            hr(),
            # Login info block
            uiOutput("login_info"),
            hr(),
            # Save/Load block
            shinyjs::hidden(div(id = "bookmark_button", actionButton("bookmark_save_button", label = "Save Template", width = "200px", icon = icon("save")))),
            actionButton("bookmark_load_button", label = "Load Template", width = "200px", icon = icon("folder-open")),
            # Modal that comes up when a user attempts to load a template by clicking the above bookmark_load_button actionButton
            bsModal(id = "modalLoad",
                    title = "Available Bookmarks",
                    trigger = "bookmark_load_button",
                    size = "large",
                    textOutput("caution"),
                    DT::DTOutput("mybookmarktable")
            ),
            hr(),
            # Main menu items
            menuItem("Main Menu", tabName = "submit", icon = icon("bars")),
            hr(),
            shinyjs::hidden(div(
              id = "cohort_definition_menuitem",
              menuItem("Cohort Definition Submission", icon = icon("circle"), tabName = "cohort_definition_submission"),
              menuSubItem(HTML("<span><a href='#coh_contributor_information_anchor'> — Contributor Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_book_information_anchor'> — Book Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_chapter_information_anchor'> — Chapter Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_provenance_anchor'> — Provenance</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_validation_anchor'> — Validation</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_supporting_documentation_anchor'> — Supporting Documentation</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#coh_additional_comments_anchor'> — Additional Comments</a></span>"), icon = NULL)
            )),
            shinyjs::hidden(div(
              id = "validation_submission_menuitem",
              menuItem("Validation Set Submission", icon = icon("circle"), tabName = "validation_submission"),
              menuSubItem(HTML("<span><a href='#val_contributor_information_anchor'> — Contributor Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#val_select_definition_anchor'> — Library Resource </a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#val_validation_procedure_anchor'> — Validation Procedure</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#val_data_used_anchor'> — Validation Dataset</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#val_validation_anchor'> — Performance Metrics</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#val_additional_comments_anchor'> — Additional Comments</a></span>"), icon = NULL)
            )),
            shinyjs::hidden(div(
              id = "citation_submission_menuitem",
              menuItem("Citation Submission", icon = icon("circle"), tabName = "citation_submission"),
              menuSubItem(HTML("<span><a href='#cite_contributor_information_anchor'> — Contributor Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#cite_select_definition_anchor'> — Library Resource </a></span>"), icon = NULL)
            )),
            shinyjs::hidden(div(
              id = "characterization_submission_menuitem",
              menuItem("Characterization Submission", icon = icon("circle"), tabName = "characterization_submission"),
              menuSubItem(HTML("<span><a href='#char_contributor_information_anchor'> — Contributor Information</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#char_select_definition_anchor'> — Library Resource </a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#char_data_description_anchor'> — Data Description</a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#char_phenotype_stability_anchor'> — Phenotype Stability </a></span>"), icon = NULL),
              menuSubItem(HTML("<span><a href='#char_feature_extraction_anchor'> — Feature Extraction</a></span>"), icon = NULL)
            )),
            shinyjs::hidden(div(
              id = "submission_complete_menuitem",
              menuItem("Submission Complete", icon = icon("check"), tabName = "submission_complete")
            )),
            shinyjs::hidden(div(id = "conditional_hr", hr())),
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            hr(width = "240px"),
            actionButton("refresh", label = "Reset All Fields", width = "200px", icon = icon("refresh")),
            bsModal(id = "refreshmodal",
                    title = "Reset Confirm",
                    trigger = "refresh",
                    size = "large",
                    textOutput("caution2"),
                    actionButton("reset_yes", label = "Yes, I want a clean form to work with."),
                    actionButton("reset_no", label = "No, I want to return to the form I was working on.")
            ),
            id = "main_sidebar_menu"
          )
        ),

        # Dashboard Body
        dashboardBody(

          # Sidebar text size - Change to 18
          tags$head(
            tags$style(HTML(".main-sidebar { font-size: 18px; }"))
          ),

          # Text at the top of the bar
          tags$head(tags$style(HTML(
            '.myClass {
          font-size: 20px;
          line-height: 50px;
          text-align: left;
          font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
          padding: 0 15px;
          overflow: hidden;
          color: white;
          }'
          ))),
          tags$script(HTML('
                         $(document).ready(function() {
                         $("header").find("nav").append(\'<span class="myClass"> OHDSI Gold Standard Phenotype Library </span>\');
                         })')),

          tags$head(
            tags$style(
              HTML(".shiny-notification {
                 font-size: 20px;
                 position:fixed;
                 top: calc(35%);
                 left: calc(40%);
                         }")
            )
          ),

          tags$head(tags$style("#text{color: red;
                             font-size: 20px;
                             font-style: italic;
                             }")),
          
          tags$head(tags$style("#caution{color: red;
                             font-size: 20px;
                             text-align: center;
                             }")),
          
          tags$head(tags$style("#caution2{color: black;
                             font-size: 20px;
                             text-align: center;
                             }")),

          # Create Find, Submission, and About tabs
          do.call(
            tabItems, c(

              # Submit
              list(
                tabItem(
                  tabName = "submit",
                  tags$hr(style = "border-color: black;"),
                  withSpinner(uiOutput("MainMenu"), type = 8, color = "#F0AB19")
                  #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = c("refresh")),
                  #actionButton("find_refresh", "Refresh")
                )
              ),

              # Submit a New Cohort Definition
              list(
                tabItem(
                  tabName = "cohort_definition_submission",
                  withSpinner(uiOutput("Submit_Definition"), type = 8, color = "#F0AB19")
                )
              ),

              # Submit a New Validation Set
              list(
                tabItem(
                  tabName = "validation_submission",
                  withSpinner(uiOutput("Submit_Validation"), type = 8, color = "#F0AB19")
                )
              ),

              # Submit a New Cohort Characterization
              list(
                tabItem(
                  tabName = "characterization_submission",
                  withSpinner(uiOutput("Submit_Characterization"), type = 8, color = "#F0AB19")
                )
              ),

              # Submit a New Citation Usage
              list(
                tabItem(
                  tabName = "citation_submission",
                  withSpinner(uiOutput("Submit_Citation"), type = 8, color = "#F0AB19")
                )
              ),

              # Submission complete menu (initially hidden)
              list(
                tabItem(
                  tabName = "submission_complete",
                  withSpinner(uiOutput("Submission_Complete"), type = 8, color = "#F0AB19")
                )
              ),

              # About
              list(
                tabItem(
                  tabName = "about",
		  fluidRow(column(8, box(width = NULL, includeMarkdown(file.path("data", "about.md")))))
                )
              )
            ) # End c
          ) # End do.call
        ) # End dashboardBody
      ) # End dashboardPage
    ) # End fluidPage
  } # End function(request)

####################################################################################################################################
# Utility Functions
####################################################################################################################################

# The modal that appears when the "save template" button is clicked
saveDataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("savename", "Please enter a name below for your saved state:"),
    span('When you load your state, you may find it by this name. Please only use alphanumeric characters and/or underscores (no spaces).'),
    if (failed)
      div(tags$b("Please enter a valid name: Use alphanumeric characters and substitute spaces with underscores.", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_save", "OK")
    )
  )
}

# Show a message whenever the user enters invalid characters into a field
showBadCharNotification <- function() {
  showNotification("Please only use: alphanumeric characters, spaces, commas, semicolons, dashes, or underscores.", 
                   duration = 3,
                   closeButton = TRUE,
                   type = "error")
}

# Query saved states according to the logged in user's ID and return a dataframe back of the user's state load options
buildBookmarkDF <- function() {
  # Split on the "%" separator in the saved filename
  user_bookmarks <- list.files(BOOKMARK_PATH, pattern = paste0(u_uid, "%", ".*rds"), full.names = FALSE)
  bookmark_matrix <- str_split_fixed(user_bookmarks, "%", 3)
  
  # Identify the time the file was created
  bookmark_create_times <- sapply(user_bookmarks, function(x) {
    as.character(file.info(file.path(BOOKMARK_PATH,x))$ctime)
  })
  
  # Make a dataframe that can be displayed as a popup for the user to select from
  df_bookmarks <- data.frame(
    key = bookmark_matrix[,2],
    name = gsub(".rds", "", bookmark_matrix[,3], fixed = TRUE),
    time = bookmark_create_times,
    row.names = NULL
  )
  
  # Sort in reverse chronological order
  df_bookmarks <- df_bookmarks[order(df_bookmarks$time, decreasing = TRUE),]
  
  # Return
  return(df_bookmarks)
}

# Create main menu
buildMainMenu <- function() {
  return(
    fluidPage(
      titlePanel("Main Menu"),
      div(
        id = "main_menu"
      ),

      fluidRow(
        column(
          width = 12,
          box(
            title = "I would like to submit a new...",
            width = NULL,
            status = "primary",

            # New Cohort Definition Button
            fluidRow(
              tags$div(
                actionButton("new_cohort_button", "Cohort Definition", width = "300px", style = "font-size:150%")
              ),
              style = "margin:10px; "
            ),

            # New Validation Set Button
            fluidRow(
              tags$div(
                actionButton("new_validation_button", "Validation Set", width = "300px", style = "font-size:150%")
              ),
              style = "margin:10px;"
            ),

            # New Citation Usage Button
            fluidRow(
              tags$div(
                actionButton("new_citation_button", "Citation Usage", width = "300px", style = "font-size:150%")
              ),
              style = "margin:10px;"
            ),

            # New Cohort Characterization Button
            fluidRow(
              tags$div(
                actionButton("new_characterization_button", "Cohort Characterization", width = "300px", style = "font-size:150%")
              ),
              style = "margin:10px;"
            )
          )
        )
      )
    )
  )
}

# Form: Submit a New Cohort Definition
makeCohortDefinitionForm <- function() {
  return(
    fluidPage(
      titlePanel("New Cohort Definition"),
      div(
        id = "coh_form",

        # all input fields for the cohort definition form
        fluidRow(
          div(id = "coh_contributor_information_anchor", box(
            title = "Contributor Information", status = "primary",
            rHandsontableOutput("author_r_table"),
            h4("To add additional authors, right-click a cell and insert a new row.")
          )),
          actionBttn("help_contributor", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_contributor", "Contributor Information", CONTRIBUTOR_INFORMATION_HELP_TEXT)
        ),

        # Book Information
        fluidRow(div(
          id = "coh_book_information_anchor", box(
            title = "Book Information", status = "primary",
            radioButtons("coh_book_exist",
              "Does the phenotype pertaining to your cohort definition already exist in the library?",
              choices = list(
                "Yes, the phenotype already exists for my cohort definition, and I will choose from the list below." = "Yes",
                "No, I would like to create a new phenotype, and my chapter will be the first entry in this book." = "No"
              ),
              selected = "Yes"
            ),
            uiOutput("SubmissionBookMenu")
          ),
          actionBttn("help_book_information", label = "?", style = "jelly", size = "sm", color = "primary"),
          div(id = "coh_book_information_anchor", bsPopover("help_book_information", "Book Information", BOOK_INFORMATION_HELP_TEXT))
        )),

        # Chapter Information
        fluidRow(div(
          id = "coh_chapter_information_anchor", box(
            title = "Chapter Information", status = "primary",
            textInput("coh_chapter_title", "What is the title of your cohort definition?"),
            textInput("coh_definition_description", "Please provide a description of this definition."),
            textInput("coh_development_process", "Please briefly describe how the cohort definition was developed (i.e. overall thought process, 
                    type of expertise required, number of 
                    collaborators, algorithms used, etc.)"),
            radioButtons(
              "coh_phenotype_modality",
              "Phenotype Modality",
              c("Rule-Based/Heuristic", "Computable/Probabilistic")
            ),
            selectInput("coh_therapeutic_areas_chapter",
              "Please select the therapeutic area(s) that pertain to this chapter, if any.",
              multiple = TRUE,
              choices = THERAPEUTIC_AREAS
            ),
            # TODO: Leverage the index file to pre-load tags other members have made
            selectizeInput("coh_tags_chapter",
              "Tag(s), if any (Type to add new tags)",
              choices = c("Claims", "EMR", "Sensitive", "Specific"),
              selected = NULL,
              multiple = TRUE,
              options = list(create = TRUE)
            ),
            fileInput("coh_link_phenotype_def", "Please upload a file of your cohort definition (eg. ATLAS Cohort Definition JSON, etc.)",
              multiple = FALSE,
              accept = FILE_TYPES
            ),
            actionButton("coh_file_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
          ),
          actionBttn("help_chapter_information", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_chapter_information", "Chapter Information", CHAPTER_INFORMATION_HELP_TEXT)
        )),

        # Provenance
        fluidRow(div(
          id = "coh_provenance_anchor", box(
            title = "Provenance", status = "primary",
            radioButtons("coh_provenance", "Would you like to establish provenance
                       by referencing another cohort definition
                       which already resides in the library?",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = FALSE
            ),
            uiOutput("ProvenanceBookMenu"),
            uiOutput("ProvenanceChapterMenu"),
            uiOutput("ProvenanceMenu")
          ),
          actionBttn("help_provenance_information", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_provenance_information", "Provenance", PROVENANCE_HELP_TEXT)
        )),

        # Validation
        fluidRow(div(
          id = "coh_validation_anchor", box(
            title = "Validation", status = "primary",
            radioButtons("coh_previous_validation", "Do you have validation data for this cohort definition?",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = TRUE
            ),
            uiOutput("ValidationMenu")
          ),
          actionBttn("help_validation_information", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_validation_information", "Validation", VALIDATION_HELP_TEXT)
        )),

        # Supporting Documentation
        fluidRow(div(
          id = "coh_supporting_documentation_anchor", box(
            title = "Supporting Documentation (optional)",
            status = "primary",
            fileInput("coh_supp_doc",
              label = "File Upload",
              accept = FILE_TYPES,
              multiple = TRUE
            ),
            h5("Note: You may ctrl-click to upload multiple files."),
            actionButton("coh_support_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
          ),
          actionBttn("help_supporting_documentation", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_supporting_documentation", "Supporting Documentation", SUPPORTING_HELP_TEXT)
        )),

        # Additional Comments
        fluidRow(div(
          id = "coh_additional_comments_anchor", box(
            title = "Additional Comments (optional)",
            status = "primary",
            textAreaInput("coh_add_comms", "", resize = "vertical")
          ),
          actionBttn("additional_comments_documentation", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("additional_comments_documentation", "Additional Comments", ADDITIONAL_COMMENTS_HELP_TEXT)
        )),

        fluidRow(infoBox(
          title = tags$p("Form Completion", style = "font-size: 120%;"),
          width = 3,
          icon = icon("cloud-upload-alt", "fa-3x"),
          color = "orange",
          actionButton("coh_submit", "-- Submit --",
            class = "btn-primary",
            style = "font-size:110%",
            width = "100%"
          )
        )),

        shinyjs::hidden(
          div(
            id = "coh_thankyou_msg",
            h3("Thanks, your response was submitted successfully!"),
            actionLink("coh_submit_another", "Submit another response")
          )
        )
      )
    )
  )
}

# Form: Submit a New Validation Set
makeValidationForm <- function() {
  return(
    fluidPage(
      titlePanel("New Validation Set"),
      div(
        id = "val_form",

        # Contributor Information
        fluidRow(
          div(id = "val_contributor_information_anchor", box(
            title = "Contributor Information",
            status = "primary",
            rHandsontableOutput("validator_r_table"),
            h4("To add additional authors, right-click a cell and insert a row.")
          )),
          actionBttn("help_validation_contributor", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_validation_contributor", "Contributor Information", VALIDATION_CONTRIBUTOR_INFORMATION_HELP_TEXT)
        ),

        # Library Resource
        fluidRow(
          div(id = "val_select_definition_anchor", box(
            title = "Library Resource",
            status = "primary",
            selectInput("validation_book_selection",
              label = "Select a Phenotype (Book):",
              choices = list(
                "Accepted" = ACCEPTED_BOOKS,
                "Proposed" = PROPOSED_BOOKS
              )
            ),
            uiOutput("ValidationChapter"),
            fileInput("validation_check_upload", "Please Upload the Implementation File for Verification:",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "application/json",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
            actionButton("validation_check_file_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
          )),
          actionBttn("help_library_resource", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_library_resource", "Library Resource", LIBRARY_RESOURCE_HELP_TEXT)
        ),

        # Validation Procedure
        fluidRow(
          div(id = "val_validation_procedure_anchor", box(
            title = "Validation Procedure",
            status = "primary",
            textAreaInput("val_valid_proc_desc", "Please describe what you did to validate this cohort definition.",
              resize = "vertical"
            )
          )),
          actionBttn("help_validation_procedure", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_validation_procedure", "Validation Procedure", VALIDATION_PROCEDURE_HELP_TEXT)
        ),

        # Validation Dataset
        fluidRow(
          div(id = "val_data_used_anchor", box(
            title = "Validation Dataset",
            status = "primary",
            textAreaInput("val_valid_data_desc", "Validation Data Description", resize = "vertical"),
            textInput("val_cdm_version", "CDM Version"),
            textInput("val_vocab_version", "Vocabulary Version")
          )),
          actionBttn("help_validation_dataset", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_validation_dataset", "Validation Dataset", VALIDATION_DATASET_HELP_TEXT)
        ),

        # Performance Metrics
        fluidRow(
          div(id = "val_validation_anchor", box(
            title = "Performance Metrics",
            status = "primary",
            radioButtons("val_validation_modality",
              label = "Select Validation Type:",
              choiceNames = c(
                "I will upload a PheValuator validation file.",
                "I will upload an APHRODITE validation file.",
                "I will input metrics manually."
              ),
              choiceValues = c("PheValuator", "APHRODITE", "Manual")
            ),
            uiOutput("ValidationModalitySelection")
          )),
          actionBttn("help_performance_metrics", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_performance_metrics", "Performance Metrics", PERFORMANCE_METRICS_HELP_TEXT)
        ),

        # Additional Comments
        fluidRow(
          div(id = "val_additional_comments_anchor", box(
            title = "Additional Comments (optional)",
            status = "primary",
            textAreaInput("val_add_comms", "", resize = "vertical")
          )),
          actionBttn("help_additional_validation_comments", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_additional_validation_comments", "Additional Comments", ADDITIONAL_VALIDATION_COMMENTS_HELP_TEXT)
        ),

        fluidRow(infoBox(
          title = tags$p("Form Completion", style = "font-size: 120%;"),
          width = 3,
          icon = icon("cloud-upload-alt", "fa-3x"),
          color = "orange",
          actionButton("val_submit",
            "-- Submit --",
            class = "btn-primary",
            style = "font-size:110%",
            width = "100%"
          )
        ))
      )
    )
  )
}

# Form: Submit a New Citation Usage
makeCitationForm <- function() {
  return(
    fluidPage(
      titlePanel("Cite a Use Case"),
      div(
        id = "citation_form",

        # Contributor Information
        fluidRow(
          div(id = "cite_contributor_information_anchor", box(
            title = "Contributor Information", status = "primary",
            rHandsontableOutput("citation_r_table"),
            h4("To add additional authors, right-click a cell and insert a row.")
          )),
          actionBttn("help_citation_contributor_information", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_citation_contributor_information", "Contributor Information", CITATION_CONTRIBUTOR_INFORMATION_HELP_TEXT)
        ),

        # Library Resource
        fluidRow(
          div(id = "cite_select_definition_anchor", box(
            title = "Library Resource",
            status = "primary",
            selectInput("citation_book_selection",
              label = "Select a Phenotype (Book):",
              choices = list(
                "Accepted" = ACCEPTED_BOOKS,
                "Proposed" = PROPOSED_BOOKS
              )
            ),
            uiOutput("CitationChapter"),
            uiOutput("CitationMode"),
            uiOutput("CitationChoice")
          )),
          actionBttn("help_citation_library_resource", label = "?", style = "jelly", size = "sm", color = "primary"),
          bsPopover("help_citation_library_resource", "Library Resource", CITATION_LIBRARY_RESOURCE_HELP_TEXT)
        ),

        fluidRow(infoBox(
          title = tags$p("Form Completion", style = "font-size: 120%;"),
          width = 3,
          icon = icon("cloud-upload-alt", "fa-3x"),
          color = "orange",
          actionButton("citation_submit", "-- Submit --",
            class = "btn-primary",
            style = "font-size:110%",
            width = "100%"
          )
        ))
      )
    )
  )
}

# Form: Submit a New Characterization Usage
makeCharacterizationForm <- function() {
  return(
    fluidPage(
      titlePanel("New Cohort Characterization"),
      div(
        id = "characterization_form",
        fluidRow(

          # Contributor Information
          fluidRow(
            div(id = "char_contributor_information_anchor", box(
              title = "Contributor Information", status = "primary",
              rHandsontableOutput("characterization_r_table"),
              h4("To add additional authors, right-click a cell and insert a row.")
            )),
            actionBttn("help_characterization_contributor_information", label = "?", style = "jelly", size = "sm", color = "primary"),
            bsPopover("help_characterization_contributor_information", "Contributor Information", CHARACTERIZATION_CONTRIBUTOR_INFORMATION_HELP_TEXT)
          ),

          # Select Resource
          fluidRow(
            div(id = "char_select_definition_anchor", box(
              title = "Library Resource",
              status = "primary",
              selectInput("characterization_book_selection",
                label = "Select a Phenotype (Book):",
                choices = list(
                  "Accepted" = ACCEPTED_BOOKS,
                  "Proposed" = PROPOSED_BOOKS
                )
              ),
              uiOutput("CharacterizationChapter")
            )),
            actionBttn("help_characterization_library_resource", label = "?", style = "jelly", size = "sm", color = "primary"),
            bsPopover("help_characterization_library_resource", "Library Resource", CHARACTERIZATION_LIBRARY_RESOURCE_HELP_TEXT)
          ),

          # Data Description
          fluidRow(
            div(id = "char_data_description_anchor", box(
              title = "Data Description",
              status = "primary",
              textAreaInput("characterization_data_description",
                label = "Please describe the data source you used for the characterization",
                resize = "vertical"
              ),
              textInput("char_cdm_version", "CDM Version"),
              textInput("char_vocab_version", "Vocabulary Version")
            )),
            actionBttn("help_characterization_data_description", label = "?", style = "jelly", size = "sm", color = "primary"),
            bsPopover("help_characterization_data_description", "Data Description", CHARACTERIZATION_DATA_DESCRIPTION_HELP_TEXT)
          ),

          # Phenotype Stability
          fluidRow(
            div(id = "char_phenotype_stability_anchor", box(
              title = "Phenotype Stability (optional)",
              status = "primary",
              fileInput("char_stability", "Please upload an image of incidence over time.",
                multiple = FALSE,
                accept = FILE_TYPES
              ),
              actionButton("char_stability_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
            )),
            actionBttn("help_characterization_phenotype_stability", label = "?", style = "jelly", size = "sm", color = "primary"),
            bsPopover("help_characterization_phenotype_stability", "Phenotype Stability", PHENOTYPE_STABILITY_HELP_TEXT)
          ),

          # Feature Extraction
          fluidRow(
            div(id = "char_feature_extraction_anchor", box(
              title = "Feature Extraction",
              status = "primary",
              fileInput("char_features", "Please upload a 'Table 1' CSV file.",
                multiple = FALSE,
                accept = FILE_TYPES
              ),
              actionButton("char_features_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times")),
              hr(),
              p("The FeatureExtraction R package is the preferred method for generating Table 1 data:"),
              tags$a(href = "https://rdrr.io/github/OHDSI/FeatureExtraction/man/createTable1.html", "Feature Extraction Table 1 Documentation", target = "_blank")
            )),
            actionBttn("help_characterization_feature_extraction", label = "?", style = "jelly", size = "sm", color = "primary"),
            bsPopover("help_characterization_feature_extraction", "Feature Extraction", FEATURE_EXTRACTION_HELP_TEXT)
          ),

          # Submit data
          fluidRow(infoBox(
            title = tags$p("Form Completion", style = "font-size: 120%;"),
            width = 3,
            icon = icon("cloud-upload-alt", "fa-3x"),
            color = "orange",
            actionButton("char_submit", "-- Submit --",
              class = "btn-primary",
              style = "font-size:110%",
              width = "100%"
            )
          ))
        )
      )
    )
  )
}

# Form: Displayed after submitting data to the library
makeSubmissionCompleteForm <- function() {
  return(
    fluidPage(
      titlePanel("Thank you!"),
      div(
        id = "submission_complete_form",
        fluidRow(box(
          title = "Submission Successful",
          status = "primary",
          h4("Your data has been submitted to the librarians. Thank you for your contribution to the OHDSI Gold Standard Phenotype Library.")
        ))
      )
    )
  )
}

# Reactive author info
authors_csv <- function(input, session) {
  author <- input$author_r_table
  if (!is.null(author)) {
    hot_to_r(author)
  }
}

authorsv_csv <- function(input, session) {
  author <- input$validator_r_table
  if (!is.null(author)) {
    hot_to_r(author)
  }
}

authorsci_csv <- function(input, session) {
  author <- input$citation_r_table
  if (!is.null(author)) {
    hot_to_r(author)
  }
}

authorscc_csv <- function(input, session) {
  author <- input$characterization_r_table
  if (!is.null(author)) {
    hot_to_r(author)
  }
}

# Copy uploaded files into the submission directory prior to uploading to Drive
copyUploads <- function(upload_items, submission_dir) {
  print(upload_items)
  dir.create(submission_dir)
  lapply(1:nrow(upload_items), function(x) {
    file.copy(upload_items$datapath[x], file.path(submission_dir))
    file.rename(file.path(submission_dir, list.files(submission_dir)[x]), file.path(submission_dir, upload_items$name[x]))
  })
  print(list.files(submission_dir))
}

# Fuction that creates the dataframe of user inputs
coh_formData <- function(input, session) {

  # First, create data by checking relevant COH_FIELDS_ALL values
  data <- data.frame(
    "Name" = COH_FIELDS_ALL,
    "Value" = I(sapply(COH_FIELDS_ALL, function(x) {
      input[[x]]
    })),
    row.names = NULL
  )

  # If coh_book_exist is "No", then the coh_existing_books entry should be set to NULL instead of the value the form holds
  idx_coh_book_exist <- which(data$Name == "coh_book_exist")
  if (data$Value[idx_coh_book_exist] == "No") {
    idx_coh_existing_books <- which(data$Name == "coh_existing_books")
    data$Value[idx_coh_existing_books] <- NA
  }

  # Then, add in the data that are captured differently
  data2 <- data.frame(
    "Name" = c(
      "user_login",
      "id",
      "timestamp",
      "name",
      "email",
      "position",
      "institution",
      "ohdsi_forum_handle",
      "orcid",
      "provenance_book",
      "provenance_chapter",
      "provenance_rationale"
    ),
    "Value" = I(list(
      user_login_id,
      getDataHash(input$coh_link_phenotype_def$datapath),
      humanTime(),
      authors_csv(input)$Name,
      authors_csv(input)$Email,
      authors_csv(input)$Position,
      authors_csv(input)$Institution,
      authors_csv(input)$Handle,
      authors_csv(input)$ORCID,
      Provenance_DF$Book,
      Provenance_DF$Chapter,
      Provenance_DF$Rationale
    )),
    row.names = NULL
  )

  # Stack to combine, and return
  data <- rbind(data2, data)
  data
}

val_formData <- function(input, session) {
  data <- data.frame(
    "Name" = VAL_FIELDS_ALL,
    "Value" = I(sapply(VAL_FIELDS_ALL, function(x) {
      input[[x]]
    })),
    row.names = NULL
  )

  data2 <- data.frame(
    "Name" = c(
      "user_login",
      "id",
      "timestamp",
      "name",
      "email",
      "position",
      "institution",
      "ohdsi_forum_handle",
      "orcid"
    ),
    "Value" = I(list(
      user_login_id,
      getDataHash(input$validation_check_upload$datapath),
      humanTime(),
      authorsv_csv(input)$Name,
      authorsv_csv(input)$Email,
      authorsv_csv(input)$Position,
      authorsv_csv(input)$Institution,
      authorsv_csv(input)$Handle,
      authorsv_csv(input)$ORCID
    )),
    row.names = NULL
  )

  # Stack to combine
  data <- rbind(data2, data)

  # Return
  data
}

citation_formData <- function(input, session) {

  # First, create data by checking relevant CIT_FIELDS_ALL values
  data <- data.frame(
    "Name" = CIT_FIELDS_ALL,
    "Value" = I(sapply(CIT_FIELDS_ALL, function(x) {
      input[[x]]
    })),
    row.names = NULL
  )

  # Then, add in the data that are captured differently
  data2 <- data.frame(
    "Name" = c(
      "user_login",
      "id",
      "timestamp",
      "name",
      "email",
      "position",
      "institution",
      "ohdsi_forum_handle",
      "orcid"
    ),
    "Value" = I(list(
      user_login_id,
      getDataHash(humanTime()),
      humanTime(),
      authorsci_csv(input)$Name,
      authorsci_csv(input)$Email,
      authorsci_csv(input)$Position,
      authorsci_csv(input)$Institution,
      authorsci_csv(input)$Handle,
      authorsci_csv(input)$ORCID
    )),
    row.names = NULL
  )

  # Stack to combine, and return
  data <- rbind(data2, data)
  data
}

characterization_formData <- function(input, session) {

  # First, create data by checking relevant CHA_FIELDS_ALL values
  data <- data.frame(
    "Name" = CHA_FIELDS_ALL,
    "Value" = I(sapply(CHA_FIELDS_ALL, function(x) {
      input[[x]]
    })),
    row.names = NULL
  )

  # Then, add in the data that are captured differently
  data2 <- data.frame(
    "Name" = c(
      "user_login",
      "id",
      "timestamp",
      "name",
      "email",
      "position",
      "institution",
      "ohdsi_forum_handle",
      "orcid"
    ),
    "Value" = I(list(
      user_login_id,
      getDataHash(input$char_features$datapath),
      humanTime(),
      authorscc_csv(input)$Name,
      authorscc_csv(input)$Email,
      authorscc_csv(input)$Position,
      authorscc_csv(input)$Institution,
      authorscc_csv(input)$Handle,
      authorscc_csv(input)$ORCID
    )),
    row.names = NULL
  )

  # Stack to combine, and return
  data <- rbind(data2, data)
  data
}

# Function that writes the cohort definition data to Drive
submitCohortDefinitionData <- function(input, session) {
  submission_dir <- tempfile(tmpdir = file.path(responsesDir, "Cohort_Data"))
  dir.create(submission_dir)
  print("Temp dir for this session:")
  print(submission_dir)

  BuildBookMarkdown <- function(dat) {

    # Read template
    md_template <- paste0(
      readLines(file.path("data", "Book_Creation_Template.md")),
      collapse = "\n"
    )

    # Replace template anchors with real text

    # Book Title
    md_template <- gsub(paste0("<", "coh_book_title", ">"), dat[["coh_book_title"]], md_template)

    # Book Description
    md_template <- gsub(paste0("<", "coh_book_clinical_description", ">"), dat[["coh_book_clinical_description"]], md_template)

    # Therapeutic Areas, if any
    if (isTruthy(dat[["coh_therapeutic_areas_book"]])) {
      # Replace with values
      md_template <- gsub(paste0("<", "coh_therapeutic_areas_book", ">"), paste0("* ", str_split(dat[["coh_therapeutic_areas_book"]], ";")[[1]], collapse = "\n\n"), md_template)
    } else {
      # Make blank
      md_template <- gsub("## Therapeutic Areas\n\n", "", md_template)
      md_template <- gsub(paste0("<", "coh_therapeutic_areas_book", ">"), "", md_template)
    }

    # Tags, if any
    if (isTruthy(dat[["coh_tags_book"]])) {
      # Replace with values
      md_template <- gsub(paste0("<", "coh_tags_book", ">"), paste0("* ", str_split(dat[["coh_tags_book"]], ";")[[1]], collapse = "\n\n"), md_template)
    } else {
      # Make blank
      md_template <- gsub("## Tags\n\n", "", md_template)
      md_template <- gsub(paste0("<", "coh_tags_book", ">"), "", md_template)
    }

    # Construct README markdown file
    writeLines(md_template, file.path(submission_dir, "README.md"))
  }

  # Write form_data.json in a human-readable format
  form_json <- jsonlite::toJSON(coh_formData(input, session), pretty = TRUE)
  write(
    x = form_json,
    file = file.path(submission_dir, "form_data.json")
  )

  # Copy uploaded phenotype definition file into the submission folder
  if (isTruthy(input$coh_link_phenotype_def)) {
    copyUploads(input$coh_link_phenotype_def, file.path(submission_dir, "Implementation"))
  }

  # Copy uploaded supplemental file into the submission folder
  if (isTruthy(input$coh_supp_doc)) {
    copyUploads(input$coh_supp_doc, file.path(submission_dir, "Supplemental"))
  }

  if (isTruthy(input$coh_validation_upload)) {
    copyUploads(input$coh_validation_upload, file.path(submission_dir, "Validation"))
  }

  # Assume using an existing book, and switch otherwise
  bookName <- ifelse(input$coh_book_exist == "No", input$coh_book_title, input$coh_existing_books)

  if (input$coh_book_exist == "No") {

    # New Books Folder
    drive_mkdir(
      bookName,
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books"
      ))
    )

    # New Drafts Folder
    drive_mkdir(
      bookName,
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Drafts"
      ))
    )
  }

  # Chapter
  drive_mkdir(
    input$coh_chapter_title,
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName
    ))
  )
  drive_mkdir(
    "Validation",
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName,
      input$coh_chapter_title
    ))
  )
  drive_mkdir(
    "Characterizations",
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName,
      input$coh_chapter_title
    ))
  )
  drive_mkdir(
    "Citations",
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName,
      input$coh_chapter_title
    ))
  )
  drive_mkdir(
    "Supplemental",
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName,
      input$coh_chapter_title
    ))
  )
  drive_mkdir(
    "Implementation",
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      bookName,
      input$coh_chapter_title
    ))
  )

  if (input$coh_book_exist == "No") {
    dat <- data.frame(
      "coh_book_title" = input$coh_book_title,
      "coh_book_clinical_description" = input$coh_book_clinical_description,
      "coh_therapeutic_areas_book" = paste(input$coh_therapeutic_areas_book, collapse = ";"),
      "coh_tags_book" = ifelse(!is.null(input$coh_tags_book), paste(input$coh_tags_book, collapse = ";"), "None")
    )
    BuildBookMarkdown(dat)
    print(list.files(submission_dir))
    drive_upload(file.path(submission_dir, "README.md"), as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      input$coh_book_title
    )))
  }

  # Upload form data as a Google Sheet
  if (isTruthy(input$coh_book_title)) {
    drive_upload(
      file.path(submission_dir, "form_data.json"),
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$coh_book_title,
        input$coh_chapter_title
      ))
    )
  } else {
    drive_upload(
      file.path(submission_dir, "form_data.json"),
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$coh_existing_books,
        input$coh_chapter_title
      ))
    )
  }

  # All remaining files
  x <- grep(list.files(submission_dir), pattern = "README.md|form_data.json", inv = TRUE, value = TRUE)

  if (isTruthy(input$coh_book_title)) {
    base_location <-
      file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$coh_book_title,
        input$coh_chapter_title
      )
  } else {
    base_location <-
      file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$coh_existing_books,
        input$coh_chapter_title
      )
  }

  # Implementation files
  print(file.path(submission_dir, "Implementation"))

  if (dir.exists(file.path(submission_dir, "Implementation"))) {
    files <- list.files(file.path(submission_dir, "Implementation"))
    lapply(files, function(y) {
      drive_upload(
        file.path(submission_dir, "Implementation", y),
        as_dribble(file.path(base_location, "Implementation"))
      )
    })
  }

  # Supplemental files
  if (dir.exists(file.path(submission_dir, "Supplemental"))) {
    files <- list.files(file.path(submission_dir, "Supplemental"))
    lapply(files, function(y) {
      drive_upload(
        file.path(submission_dir, "Supplemental", y),
        as_dribble(file.path(base_location, "Supplemental"))
      )
    })
  }

  if (dir.exists(file.path(submission_dir, "Validation"))) {
    files <- list.files(file.path(submission_dir, "Validation"))
    lapply(files, function(y) {
      drive_upload(
        file.path(submission_dir, "Validation", y),
        as_dribble(file.path(base_location, "Validation"))
      )
    })
  }

  # Final confirmation file
  drive_upload(
    file.path("data", "Confirmation.txt"),
    file.path(base_location, "Confirmation.txt")
  )
} # End function

# Function that writes the validation data to Drive
submitValidationData <- function(input, session, values_val_upload) {
  submission_dirv <- tempfile(tmpdir = file.path(responsesDir, "Validation_Data"))
  dir.create(submission_dirv)
  print("Temp dir_v for this session:")
  print(submission_dirv)

  # Write tabular data to the the submission folder
  form_json <- jsonlite::toJSON(val_formData(input, session), pretty = TRUE)
  write(
    x = form_json,
    file = file.path(submission_dirv, "valid_form_data.json")
  )

  if (isTruthy(input$validation_check_upload)) {
    copyUploads(input$validation_check_upload, file.path(submission_dirv))
  }

  if (isTruthy(values_val_upload$upload_state == "uploaded")) {
    copyUploads(input$val_validation_upload, file.path(submission_dirv))
  }

  shinyjs::disable("val_submit")
  shinyjs::hide("val_error")

  name <- paste0(gsub(" ", "_", authorsv_csv(input)[1, 1]), "_", humanTime())
  tryCatch({
    if (!(name) %in% drive_ls(path = file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      input$validation_book_selection,
      input$validation_chapter_selection,
      "Validation"
    ))) {
      drive_mkdir(
        name,
        as_dribble(file.path(
          "OHDSI Gold Standard Phenotype Library",
          "Books",
          input$validation_book_selection,
          input$validation_chapter_selection,
          "Validation"
        ))
      )
    }

    # Upload files to Google Drive
    lapply(list.files(submission_dirv), function(y) {
      drive_upload(
        file.path(submission_dirv, y),
        as_dribble(file.path(
          "OHDSI Gold Standard Phenotype Library",
          "Books",
          input$validation_book_selection,
          input$validation_chapter_selection,
          "Validation",
          name
        ))
      )
    })

    shinyjs::reset("val_form")
    shinyjs::hide("val_form")
  })
  error <- function(err) {
    shinyjs::html("val_error_msg", err$val_message)
    shinyjs::show(id = "val_error", anim = TRUE, animType = "fade")
  }
  finally <- {
    shinyjs::enable("val_submit")
  }
}

# Function that writes the cohort definition data to Drive
submitCitationData <- function(input, session) {
  submission_dir <- tempfile(tmpdir = file.path(responsesDir, "Citation_Data"))
  dir.create(submission_dir)
  print("Temp dir for this session:")
  print(submission_dir)

  # Write form_data.json in a human-readable format
  citation_json <- jsonlite::toJSON(citation_formData(input, session), pretty = TRUE)

  if (input$citation_select == "Manual") {
    write(
      x = citation_json,
      file = file.path(submission_dir, paste0("Manual", "_", humanTime()))
    )
    drive_upload(
      file.path(submission_dir, paste0("Manual", "_", humanTime())),
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$citation_book_selection,
        input$citation_chapter_selection,
        "Citations"
      ))
    )
  } else {
    write(
      x = citation_json,
      file = file.path(submission_dir, paste0(gsub(" ", "_", input$citation_data), "_", humanTime()))
    )
    drive_upload(
      file.path(submission_dir, paste0(gsub(" ", "_", input$citation_data), "_", humanTime())),
      as_dribble(file.path(
        "OHDSI Gold Standard Phenotype Library",
        "Books",
        input$citation_book_selection,
        input$citation_chapter_selection,
        "Citations"
      ))
    )
  }
} # End function

# Function that writes the cohort definition data to Drive
submitCharacterizationData <- function(input, session) {
  submission_dir <- tempfile(tmpdir = file.path(responsesDir, "Characterization_Data"))
  dir.create(submission_dir)
  print("Temp dir for this session:")
  print(submission_dir)

  # Write form_data.json in a human-readable format
  characterization_json <- jsonlite::toJSON(characterization_formData(input, session), pretty = TRUE)
  write(
    x = characterization_json,
    file = file.path(submission_dir, "characterization_data.json")
  )

  # Copy uploaded phenotype definition file into the submission folder
  if (isTruthy(input$char_features)) {
    copyUploads(input$char_features, file.path(submission_dir, "Features"))
  }

  # Copy uploaded supplemental file into the submission folder
  if (isTruthy(input$char_stability)) {
    copyUploads(input$char_stability, file.path(submission_dir, "Stability"))
  }

  name <- paste0(gsub(" ", "_", authorscc_csv(input)[1, 1]), "_", humanTime())

  drive_mkdir(
    name,
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      input$characterization_book_selection,
      input$characterization_chapter_selection,
      "Characterizations"
    ))
  )

  drive_upload(
    file.path(submission_dir, "characterization_data.json"),
    as_dribble(file.path(
      "OHDSI Gold Standard Phenotype Library",
      "Books",
      input$characterization_book_selection,
      input$characterization_chapter_selection,
      "Characterizations",
      name
    ))
  )

  if (dir.exists(file.path(submission_dir, "Features"))) {
    files <- list.files(file.path(submission_dir, "Features"))
    lapply(files, function(y) {
      drive_upload(
        file.path(submission_dir, "Features", y),
        as_dribble(file.path(
          "OHDSI Gold Standard Phenotype Library",
          "Books",
          input$characterization_book_selection,
          input$characterization_chapter_selection,
          "Characterizations",
          name
        ))
      )
    })
  }

  # Supplemental files
  if (dir.exists(file.path(submission_dir, "Stability"))) {
    files <- list.files(file.path(submission_dir, "Stability"))
    lapply(files, function(y) {
      drive_upload(
        file.path(submission_dir, "Stability", y),
        as_dribble(file.path(
          "OHDSI Gold Standard Phenotype Library",
          "Books",
          input$characterization_book_selection,
          input$characterization_chapter_selection,
          "Characterizations",
          name
        ))
      )
    })
  }
} # End function

# time converted to human format
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Function to get the hash of a data file
getDataHash <- function(data) {
  return(digest::digest(data))
}

# Function to check the validity of the contributor table
# The message ID is used to distinguish between the cohort submission and the validation
checkContributorTable <- function(df, message_id) {

  # If the table is empty, don't give a message
  # Otherwise, check the validity of what is entered
  if (nrow(df) > 0) {

    # Check Name, Email, Institution, Position, and ORCID ID
    if (!(all(sapply(df$Name, function(x) {
      x != "" & !is.na(x)
    })))) {
      status_code <- "Name"
    } else if (!(all(sapply(df$Email, function(x) {
      x != "" & !is.na(x)
    })))) {
      status_code <- "Email"
    } else if (!(all(sapply(df$Institution, function(x) {
      x != "" & !is.na(x)
    })))) {
      status_code <- "Institution"
    } else if (!(all(sapply(df$Position, function(x) {
      x != "" & !is.na(x)
    })))) {
      status_code <- "Position"
    } else if (!(all(sapply(df$ORCID, function(x) {
      str_detect(x, REGEX_ORCID) | x == "" | is.na(x)
    })))) {
      status_code <- "ORCID"
    } else {
      status_code <- NA
    }

    # If there is a problem found with the table, show the corresponding notification
    if (!is.na(status_code)) {
      message <-
        switch(status_code,
          "Name" = "Please verify that the Name field is complete in the contributor table.",
          "Email" = "Please verify that the Email field is complete in the contributor table.",
          "Institution" = "Please verify that the Institution field is complete in the contributor table.",
          "Position" = "Please verify that the Position field is complete in the contributor table.",
          "ORCID" = "Please verify that the ORCID ID field is complete and in the correct format (XXXX-XXXX-XXXX-XXXX) in the contributor table."
        )
      showNotification(message,
        duration = NULL,
        closeButton = TRUE,
        type = "error",
        id = message_id
      )
      # Return TRUE on success - FALSE otherwise
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

# Ensure that the book information has been entered to completion
checkBookInformation <- function(input, message_id) {
  if (isTruthy(input$coh_book_exist == "Yes")) {
    status_code <- NA
  } else {
    if (!(isTruthy(input$coh_book_title))) {
      status_code <- "Book Title"
    } else if (!(isTruthy(input$coh_book_clinical_description))) {
      status_code <- "Clinical Description"
    } else {
      status_code <- NA
    }
  }

  if (!is.na(status_code)) {
    message <-
      switch(status_code,
        "Book Title" = "Please verify that the Book Title field is complete in the Book Information section.",
        "Clinical Description" = "Please verify that the Clinical Description field is complete in the Book Information section."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Ensure that the chapter information has been entered to completion
checkChapterInformation <- function(input, message_id, values_coh_phenotype) {
  if (!(isTruthy(input$coh_chapter_title))) {
    status_code <- "Chapter Title"
  } else if (!(isTruthy(input$coh_definition_description))) {
    status_code <- "Definition Description"
  } else if (!(isTruthy(input$coh_development_process))) {
    status_code <- "Development Process"
  } else if (is.null(values_coh_phenotype$upload_state)) {
    status_code <- "Phenotype File"
  } else if (values_coh_phenotype$upload_state == "reset") {
    status_code <- "Phenotype File"
  } else {
    status_code <- NA
  }

  # If there is a problem found with the table, show the corresponding notification
  if (!is.na(status_code)) {
    message <-
      switch(status_code,
             "Chapter Title" = "Please verify that the Chapter Title field is complete in the Chapter Information section.",
             "Definition Description" = "Please verify that the Definition Description field is complete in the Chapter Information secton.",
             "Development Process" = "Please verify that the Development Process field is complete in the Chapter Information section.",
             "Phenotype File" = "Please verify that the Phenotype File field is complete in the Chapter Information section."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkValidationInformation <- function(input, message_id, values_coh_validation) {
  if (isTruthy(input$coh_previous_validation == "FALSE")) {
    status_code <- NA
  } else {
    if (!(isTruthy(input$coh_valid_proc_desc))) {
      status_code <- "Validation Procedure"
    } else if (!(isTruthy(input$coh_valid_data_desc))) {
      status_code <- "Validation Data Description"
    } else if (!(isTruthy(input$coh_cdm_version))) {
      status_code <- "CDM Version"
    } else if (!(isTruthy(input$coh_vocab_version))) {
      status_code <- "Vocabulary Version"
    } else if (input$coh_validation_modality == "Manual") {
      if (!(isTruthy(input$coh_true_pos))) {
        status_code <- "True Positives"
      } else if (!(isTruthy(input$coh_true_neg))) {
        status_code <- "True Negatives"
      } else if (!(isTruthy(input$coh_false_pos))) {
        status_code <- "False Positives"
      } else if (!(isTruthy(input$coh_false_neg))) {
        status_code <- "False Negatives"
      } else if (!(isTruthy(input$coh_inconclusive))) {
        status_code <- "Inconclusive"
      } else {
        status_code <- NA
      }
    } else if (input$coh_validation_modality != "Manual") {
      if (is.null(values_coh_validation$upload_state)) {
        status_code <- "Validation Upload"
      } else if (values_coh_validation$upload_state == "reset") {
        status_code <- "Validation Upload"
      } else {
        status_code <- NA
      }
    } else {
      status_code <- NA
    }
  }

  # If there is a problem found with the table, show the corresponding notification
  if (!is.na(status_code)) {
    message <-
      switch(status_code,
        "Validation Procedure" = "Please verify that the Validation Procedure field is complete in the Validation section.",
        "Validation Data Description" = "Please verify that the Validation Data Description field is complete in the Validation section.",
        "CDM Version" = "Please verify that the CDM Version field is complete in the Validation section.",
        "Vocabulary Version" = "Please verify that the Vocabulary Version field is complete in the Validation section.",
        "True Positives" = "Please verify that the True Positives field is complete in the Phenotype Validation Metrics section.",
        "True Negatives" = "Please verify that the True Negatives field is complete in the Phenotype Validation Metrics section.",
        "False Positives" = "Please verify that the False Positives field is complete in the Phenotype Validation Metrics section.",
        "False Negatives" = "Please verify that the False Negatives field is complete in the Phenotype Validation Metrics section.",
        "Inconclusive" = "Please verify that the Inconclusive field is complete in the Phenotype Validation Metrics section.",
        "Validation Upload" = "Please verify that the Validation Upload field is complete in the Phenotype Validation Metrics section."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkValidationValInformation <- function(input, message_id, values_val_check, values_val_upload) {
  if (is.null(values_val_check$upload_state)) {
    status_code <- "Implementation File"
  } else if (values_val_check$upload_state == "reset") {
    status_code <- "Implementation File"
  } else if (!(isTruthy(input$val_valid_proc_desc))) {
    status_code <- "Validation Procedure"
  } else if (!(isTruthy(input$val_valid_data_desc))) {
    status_code <- "Validation Data Description"
  } else if (!(isTruthy(input$val_cdm_version))) {
    status_code <- "CDM Version"
  } else if (!(isTruthy(input$val_vocab_version))) {
    status_code <- "Vocabulary Version"
  } else if (input$val_validation_modality == "Manual") {
    if (!(isTruthy(input$val_true_pos))) {
      status_code <- "True Positives"
    } else if (!(isTruthy(input$val_true_neg))) {
      status_code <- "True Negatives"
    } else if (!(isTruthy(input$val_false_pos))) {
      status_code <- "False Positives"
    } else if (!(isTruthy(input$val_false_neg))) {
      status_code <- "False Negatives"
    } else if (!(isTruthy(input$val_inconclusive))) {
      status_code <- "Inconclusive"
    } else {
      status_code <- NA
    }
  } else if (input$val_validation_modality != "Manual") {
    if (is.null(values_val_upload$upload_state)) {
      status_code <- "Validation Upload"
    } else if (values_val_upload$upload_state == "reset") {
      status_code <- "Validation Upload"
    } else {
      status_code <- NA
    }
  }

  # If there is a problem found with the table, show the corresponding notification
  if (!is.na(status_code)) {
    message <-
      switch(status_code,
        "Implementation File" = "Please verify that the Implementation File Field is complete.",
        "Validation Procedure" = "Please verify that the Validation Procedure field is complete in the Validation section.",
        "Validation Data Description" = "Please verify that the Validation Data Description field is complete in the Validation section.",
        "CDM Version" = "Please verify that the CDM Version field is complete in the Validation section.",
        "Vocabulary Version" = "Please verify that the Vocabulary Version field is complete in the Validation section.",
        "True Positives" = "Please verify that the True Positives field is complete in the Phenotype Validation Metrics section.",
        "True Negatives" = "Please verify that the True Negatives field is complete in the Phenotype Validation Metrics section.",
        "False Positives" = "Please verify that the False Positives field is complete in the Phenotype Validation Metrics section.",
        "False Negatives" = "Please verify that the False Negatives field is complete in the Phenotype Validation Metrics section.",
        "Inconclusive" = "Please verify that the Inconclusive field is complete in the Phenotype Validation Metrics section.",
        "Validation Upload" = "Please verify that the Validation Upload field is complete in the Phenotype Validation Metrics section."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkCitationInformation <- function(input, message_id) {
  if (!(isTruthy(input$citation_data))) {
    status_code <- "Citation"
  } else {
    status_code <- NA
  }

  # If there is a problem found with the table, show the corresponding notification
  if (!is.na(status_code)) {
    message <-
      switch(status_code,
        "Citation" = "Please verify that you have completed the citation for your selection."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkCharacterizationInformation <- function(input, message_id, values_cha_features) {
  if (!(isTruthy(input$characterization_data_description))) {
    status_code <- "Data Description"
  } else if (!(isTruthy(input$char_cdm_version))) {
    status_code <- "CDM"
  } else if (!(isTruthy(input$char_vocab_version))) {
    status_code <- "Vocab"
  } else if (is.null(values_cha_features$upload_state)) {
    status_code <- "Features File"
  } else if (values_cha_features$upload_state == "reset") {
    status_code <- "Features File"
  } else {
    status_code <- NA
  }

  # If there is a problem found with the table, show the corresponding notification
  if (!is.na(status_code)) {
    message <-
      switch(status_code,
        "Data Description" = "Please verify that the Data Description field is complete.",
        "CDM" = "Please verify that the CDM Version field is complete.",
        "Vocab" = "Please verify that the Vocab Version field is complete.",
        "Features File" = "Please verify that you have uploaded a file into the Feature Extraction field."
      )
    showNotification(message,
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = message_id
    )
    # Return TRUE on success - FALSE otherwise
    return(FALSE)
  } else {
    return(TRUE)
  }
}

####################################################################################################################################
# Server Definition
####################################################################################################################################
server <- function(input, output, session) {

  # User ID from Auth0
  u_uid <<- strsplit(session$userData$auth0_info$sub, "\\|")[[1]][2]

  # Reactive Values
  # TODO: Consolidate
  values_coh_phenotype <- reactiveValues(upload_state = NULL)
  values_coh_validation <- reactiveValues(upload_state = NULL)
  values_val_check <- reactiveValues(upload_state = NULL)
  values_val_upload <- reactiveValues(upload_state = NULL)
  values_cha_features <- reactiveValues(upload_state = NULL)
  provenance_table <- reactiveVal()

  # Capture the user ID so it can be referenced later at the time of submission
  # If Auth0 isn't being used, then "No ID" will be assigned in the ID's place
  if (USING_AUTH0) {
    # Replacing pipe in the ID so that the pipe-separated form data csv can be written properly
    user_login_id <<- sub("|", "_", session$userData$auth0_info$sub, fixed = TRUE)
  } else {
    user_login_id <<- "No ID"
  }

  # Write directories if they don't already exist
  coh_write_path <- file.path(responsesDir, "Cohort_Data")
  if (!dir.exists(coh_write_path)) {
    dir.create(coh_write_path)
  }

  val_write_path <- file.path(responsesDir, "Validation_Data")
  if (!dir.exists(val_write_path)) {
    dir.create(val_write_path)
  }

  citation_write_path <- file.path(responsesDir, "Citation_Data")
  if (!dir.exists(citation_write_path)) {
    dir.create(citation_write_path)
  }

  characterization_write_path <- file.path(responsesDir, "Characterization_Data")
  if (!dir.exists(characterization_write_path)) {
    dir.create(characterization_write_path)
  }

  ##################################################################################################################################
  # Event Listeners
  ##################################################################################################################################

  # Listener to check for the cohort definition contributor table correctness
  onevent(
    "mouseenter",
    "coh_submit",
    {
      # Assume submission is not possible until checked otherwise
      disable("coh_submit")

      # First check if the author table is properly filled in
      if (checkContributorTable(hot_to_r(input$author_r_table), "author_table_warning")) {

        # Then, check if the other mandatory fields are filled in
        if (checkBookInformation(input, "author_table_warning")) {
          if (checkChapterInformation(input, "author_table_warning", values_coh_phenotype)) {
            if (checkValidationInformation(input, "author_table_warning", values_coh_validation)) {

              # If the tests have passed, then enable the submit button and display a notification
              enable("coh_submit")
              showNotification("Click to submit your definition to the librarians.",
                duration = NULL,
                closeButton = TRUE,
                type = "message",
                id = "author_table_warning"
              )
            }
          }
        }
      }
    }
  )
  onevent(
    "mouseleave",
    "coh_submit",
    removeNotification("author_table_warning")
  )
  onevent(
    "click",
    "coh_submit",
    removeNotification("author_table_warning")
  )

  # Listener to check for the validation set contributor table correctness
  onevent(
    "mouseenter",
    "val_submit",
    {
      # Assume submission is not possible until checked otherwise
      disable("val_submit")

      # First check if the author table is properly filled in
      if (checkContributorTable(hot_to_r(input$validator_r_table), "validator_table_warning")) {

        # Then, check if the other mandatory fields are filled in
        if (checkValidationValInformation(input, "validator_table_warning", values_val_check, values_val_upload)) {

          # If the tests have passed, then enable the submit button and display a notification
          enable("val_submit")
          showNotification("Click to submit your validation set to the librarians.",
            duration = NULL,
            closeButton = TRUE,
            type = "message",
            id = "validator_table_warning"
          )
        }
      }
    }
  )
  onevent(
    "mouseleave",
    "val_submit",
    removeNotification("validator_table_warning")
  )
  onevent(
    "click",
    "val_submit",
    removeNotification("validator_table_warning")
  )

  onevent(
    "mouseenter",
    "citation_submit",
    {
      # Assume submission is not possible until checked otherwise
      disable("citation_submit")

      # First check if the author table is properly filled in
      if (checkContributorTable(hot_to_r(input$citation_r_table), "author_table_warning")) {

        # Then, check if the other mandatory fields are filled in
        if (checkCitationInformation(input, "author_table_warning")) {

          # If the tests have passed, then enable the submit button and display a notification
          enable("citation_submit")
          showNotification("Click to submit your definition to the librarians.",
            duration = NULL,
            closeButton = TRUE,
            type = "message",
            id = "author_table_warning"
          )
        }
      }
    }
  )
  onevent(
    "mouseleave",
    "citation_submit",
    removeNotification("author_table_warning")
  )
  onevent(
    "click",
    "citation_submit",
    removeNotification("author_table_warning")
  )

  onevent(
    "mouseenter",
    "char_submit",
    {
      # Assume submission is not possible until checked otherwise
      disable("char_submit")

      # First check if the author table is properly filled in
      if (checkContributorTable(hot_to_r(input$characterization_r_table), "author_table_warning")) {

        # Then, check if the other mandatory fields are filled in
        if (checkCharacterizationInformation(input, "author_table_warning", values_cha_features)) {

          # If the tests have passed, then enable the submit button and display a notification
          enable("char_submit")
          showNotification("Click to submit your definition to the librarians.",
            duration = NULL,
            closeButton = TRUE,
            type = "message",
            id = "author_table_warning"
          )
        }
      }
    }
  )
  onevent(
    "mouseleave",
    "char_submit",
    removeNotification("author_table_warning")
  )
  onevent(
    "click",
    "char_submit",
    removeNotification("author_table_warning")
  )

  # Hover text for help -- Mouse enter event for "Submit a New Cohort Definition"
  onevent(
    "mouseenter",
    "new_cohort_button",
    showNotification("Submit a new book (phenotype) and/or chapter (cohort definition) to the library.",
      duration = NULL,
      closeButton = TRUE,
      type = "message",
      id = "new_cohort_button_notification"
    )
  )
  onevent(
    "mouseleave",
    "new_cohort_button",
    removeNotification("new_cohort_button_notification")
  )

  onevent(
    "click",
    "new_cohort_button",
    removeNotification("new_cohort_button_notification")
  )

  # Hover text for help -- Mouse enter event for "Submit a New Validation Set"
  onevent(
    "mouseenter",
    "new_validation_button",
    showNotification("Submit validation data for an existing chapter (cohort definition).",
      duration = NULL,
      closeButton = TRUE,
      type = "message",
      id = "new_validation_button_notification"
    )
  )
  onevent(
    "mouseleave",
    "new_validation_button",
    removeNotification("new_validation_button_notification")
  )
  onevent(
    "click",
    "new_validation_button",
    removeNotification("new_validation_button_notification")
  )

  # Hover text for help -- Mouse enter event for "Submit a New Citation Usage"
  onevent(
    "mouseenter",
    "new_citation_button",
    showNotification("Submit a published use of an existing chapter (cohort definition).",
      duration = NULL,
      closeButton = TRUE,
      type = "message",
      id = "new_citation_button_notification"
    )
  )
  onevent(
    "mouseleave",
    "new_citation_button",
    removeNotification("new_citation_button_notification")
  )
  onevent(
    "click",
    "new_citation_button",
    removeNotification("new_citation_button_notification")
  )

  # Hover text for help -- Mouse enter event for "Submit a New Cohort Characterization"
  onevent(
    "mouseenter",
    "new_characterization_button",
    showNotification("Submit characterization of a cohort obtained after having applied a library cohort definition.",
      duration = NULL,
      closeButton = TRUE,
      type = "message",
      id = "new_characterization_button_notification"
    )
  )
  onevent(
    "mouseleave",
    "new_characterization_button",
    removeNotification("new_characterization_button_notification")
  )
  onevent(
    "click",
    "new_characterization_button",
    removeNotification("new_characterization_button_notification")
  )

  # Only show the bookmark button if a form has been started
  onevent(
    "click",
    "new_cohort_button",
    shinyjs::show("bookmark_button")
  )

  onevent(
    "click",
    "new_validation_button",
    shinyjs::show("bookmark_button")
  )

  onevent(
    "click",
    "new_citation_button",
    shinyjs::show("bookmark_button")
  )

  onevent(
    "click",
    "new_characterization_button",
    shinyjs::show("bookmark_button")
  )

  ##################################################################################################################################
  # Observers
  ##################################################################################################################################

  # Guide the user to enter a valid save name by substituting out disallowed characters when typed
  observeEvent(input$savename, {
    updateTextInput(session, "savename", value = gsub("[^a-zA-Z0-9_]", "", input$savename))
  })
  
  # When OK button is pressed from the save modal, check the text
  # If ok, then proceed with saving and notify the user
  # Else, loop back in on the modal with a set failed flag
  observeEvent(input$ok_save, {
    if (!is.null(input$savename) && nzchar(input$savename) && !grepl("[^a-zA-Z0-9_]",input$savename)) {
      cur_savename <<- input$savename
      session$doBookmark()
      showNotification(paste0("Saved state as: ", cur_savename), closeButton = TRUE, duration = 5, type = "message")
      removeModal()
    } else {
      showModal(saveDataModal(failed = TRUE))
    }
  })
  
  # On save click, transfer the save logic to the saveDataModal() function
  observeEvent(input$bookmark_save_button, {
    showModal(saveDataModal())
  })
  
  # Check book for extraneous characters
  observeEvent(input$coh_book_title, {
    if (grepl(REGEX_TITLE, input$coh_book_title)) {
      updateTextInput(session, "coh_book_title", value = gsub(REGEX_TITLE, "", input$coh_book_title))
      showBadCharNotification()
    }
  })
  
  # Check chapter for extraneous characters
  observeEvent(input$coh_chapter_title, {
    if (grepl(REGEX_TITLE, input$coh_chapter_title)) {
      updateTextInput(session, "coh_chapter_title", value = gsub(REGEX_TITLE, "", input$coh_chapter_title))
      showBadCharNotification()
    }
  })
  
  # Modal refresh - No
  observeEvent(input$reset_no, {
    toggleModal(session, "refreshmodal", "close")
  })
  
  # Modal refresh - Yes
  observeEvent(input$reset_yes, {
    toggleModal(session, "refreshmodal", "close")
    if (input$main_sidebar_menu == "cohort_definition_submission") {
      reset("coh_form")
      output$author_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
      Provenance_DF <<- data.frame("Book" = character(0), "Chapter" = character(0), "Rationale" = character(0))
      provenance_table(Provenance_DF)
      prov_check$used <<- FALSE
      #updateSelectInput(session, "coh_provenance_book", choices = subset(prov_check, used != TRUE)$Book, selected = sample(subset(prov_check, used != TRUE & Book != input$coh_provenance_book)$Book, 1))
      #updateSelectInput(session, "coh_provenance_chapter", choices = subset(prov_check, used != TRUE)$Chapter)
      output$provenance_table <- renderDT({
        datatable(provenance_table(), selection = "single", options = list(dom = "t"))
      })
    } else if (input$main_sidebar_menu == "validation_submission") {
      reset("val_form")
      output$validator_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else if (input$main_sidebar_menu == "citation_submission") {
      reset("citation_form")
      output$citation_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else {
      reset("characterization_form")
      output$characterization_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    }
  })
  
  # Load button click - Routes depending on whether any bookmarks exist for the user
  observeEvent(input$bookmark_load_button, {
    if (length(list.files(BOOKMARK_PATH, pattern = paste0(u_uid, "%", ".*rds"), full.names = TRUE)) > 0) {
      shinyjs::hide("caution")
      shinyjs::show("mybookmarktable")
    } else {
      shinyjs::show("caution")
      shinyjs::hide("mybookmarktable")
    }
  })
  
  # Prevent the book title from colliding with an existing title
  observeEvent(input$coh_book_title, {
    if (input$coh_book_title %in% PROPOSED_BOOKS) {
      showNotification("Please enter a book title that has not already been taken, or choose this title from the existing list.",
        id = "note_book_title_taken",
        type = "error",
        duration = NULL
      )
    } else {
      removeNotification("note_book_title_taken")
    }
  })

  observeEvent(input$coh_book_exist, {
    if (input$coh_book_exist == "Yes") {
      removeNotification("note_book_title_taken")
    }
  })

  observeEvent(input$coh_link_phenotype_def, {
    values_coh_phenotype$upload_state <- "uploaded"
  })

  observeEvent(input$coh_file_delete, {
    values_coh_phenotype$upload_state <- "reset"
  })

  observeEvent(input$coh_validation_upload, {
    values_coh_validation$upload_state <- "uploaded"
  })

  observeEvent(input$coh_validation_file_delete, {
    values_coh_validation$upload_state <- "reset"
  })

  observeEvent(input$validation_check_upload, {
    values_val_check$upload_state <- "uploaded"
  })

  observeEvent(input$validation_check_file_delete, {
    values_val_check$upload_state <- "reset"
  })

  observeEvent(input$val_validation_upload, {
    values_val_upload$upload_state <- "uploaded"
  })

  observeEvent(input$validation_file_delete, {
    values_val_upload$upload_state <- "reset"
  })

  observeEvent(input$coh_validation_modality, {
    reset("coh_validation_upload")
    values_coh_validation$upload_state <- "reset"
  })

  observeEvent(input$val_validation_modality, {
    reset("val_validation_upload")
    values_val_upload$upload_state <- "reset"
  })

  observeEvent(input$char_features, {
    values_cha_features$upload_state <- "uploaded"
  })

  observeEvent(input$char_features_delete, {
    values_cha_features$upload_state <- "reset"
  })

  observeEvent(input$coh_provenance, {
    if (input$coh_provenance == "FALSE") {
      hide("ProvenanceBookMenu")
      hide("ProvenanceChapterMenu")
      hide("ProvenanceMenu")
    } else {
      show("ProvenanceBookMenu")
      show("ProvenanceChapterMenu")
      show("ProvenanceMenu")
    }
  })

  observeEvent(input$coh_provenance_rationale, {
    if (isTruthy(input$coh_provenance_rationale) && any(prov_check$used != TRUE)) {
      enable("coh_provenance_update_table")
    }
  })

  observeEvent(input$coh_provenance_update_table, {
    prov_vec <- data.frame("Book" = input$coh_provenance_book, "Chapter" = input$coh_provenance_chapter, "Rationale" = input$coh_provenance_rationale)
    Provenance_DF <<- rbind(Provenance_DF, prov_vec)
    indices <- which(prov_check == input$coh_provenance_chapter, arr.ind = TRUE)
    prov_check[indices[1], (indices[2] + 1)] <<- TRUE
    provenance_table(Provenance_DF)
    if (any(prov_check$used != TRUE)) {
      if (dim(subset(prov_check, used != TRUE & Book != input$coh_provenance_book))[1] != 0) {
        updateSelectInput(session, "coh_provenance_book", choices = subset(prov_check, used != TRUE)$Book, selected = sample(subset(prov_check, used != TRUE & Book != input$coh_provenance_book)$Book, 1))
        reset("coh_provenance_rationale")
        disable("coh_provenance_update_table")
      } else {
        updateSelectInput(session, "coh_provenance_book", choices = subset(prov_check, used != TRUE)$Book)
        updateSelectInput(session, "coh_provenance_chapter", choices = subset(prov_check, used != TRUE)$Chapter)
        reset("coh_provenance_rationale")
        disable("coh_provenance_update_table")
      }
    } else {
      disable("coh_provenance_update_table")
      updateSelectInput(session, "coh_provenance_book", choices = character(0))
      updateSelectInput(session, "coh_provenance_chapter", choices = character(0))
      updateTextAreaInput(session, "coh_provenance_rationale", value = "", resize = "vertical")
      showNotification("Provenance has been established with all potential cohort defintions. No more can be added to the table.", type = "warning")
    }
    output$provenance_table <- renderDT({
      datatable(provenance_table(), selection = "single", options = list(dom = "t"))
    })
  })

  observeEvent(input$provenance_table_rows_selected, {
    enable("coh_provenance_delete_table")
  })

  observeEvent(input$coh_provenance_delete_table, {
    t <- provenance_table()
    if (!is.null(input$provenance_table_rows_selected)) {
      if (dim(t)[1] > 1) {
        x <- t[as.numeric(input$provenance_table_rows_selected), ]$Chapter
        indices <- which(prov_check == levels(x)[x], arr.ind = TRUE)
        prov_check[indices[1], (indices[2] + 1)] <<- FALSE
        t <- t[-as.numeric(input$provenance_table_rows_selected), ]
        Provenance_DF <<- Provenance_DF[-as.numeric(input$provenance_table_rows_selected), ]
        updateSelectInput(session, "coh_provenance_book", choices = subset(prov_check, used != TRUE)$Book, selected = sample(subset(prov_check, used != TRUE & Book != input$coh_provenance_book)$Book, 1))
      } else {
        t <- t[-as.numeric(input$provenance_table_rows_selected), ]
        Provenance_DF <<- Provenance_DF[-as.numeric(input$provenance_table_rows_selected), ]
        prov_check$used <<- FALSE
        updateSelectInput(session, "coh_provenance_book", choices = subset(prov_check, used != TRUE)$Book, selected = sample(subset(prov_check, used != TRUE & Book != input$coh_provenance_book)$Book, 1))
      }
    }
    provenance_table(t)
    disable("coh_provenance_delete_table")
  })

  # Main menu -- Click Event for "Submit a New Cohort Definition"
  observeEvent(input$new_cohort_button, {
    updateTabItems(session, "main_sidebar_menu", "cohort_definition_submission")
    shinyjs::show("cohort_definition_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    shinyjs::hide("citation_submission_menuitem")
    shinyjs::hide("characterization_submission_menuitem")
    shinyjs::hide("submission_complete_menuitem")
    shinyjs::show("conditional_hr")
  })

  # Main menu -- Click Event for "Submit a New Validation Set"
  observeEvent(input$new_validation_button, {
    updateTabItems(session, "main_sidebar_menu", "validation_submission")
    shinyjs::show("validation_submission_menuitem")
    shinyjs::hide("cohort_definition_menuitem")
    shinyjs::hide("citation_submission_menuitem")
    shinyjs::hide("characterization_submission_menuitem")
    shinyjs::hide("submission_complete_menuitem")
    shinyjs::show("conditional_hr")
  })

  # Main menu -- Click Event for "Submit a Citation Usage"
  observeEvent(input$new_citation_button, {
    updateTabItems(session, "main_sidebar_menu", "citation_submission")
    shinyjs::show("citation_submission_menuitem")
    shinyjs::hide("cohort_definition_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    shinyjs::hide("characterization_submission_menuitem")
    shinyjs::hide("submission_complete_menuitem")
    shinyjs::show("conditional_hr")
  })

  # Main menu -- Click Event for "Submit a Cohort Characterization"
  observeEvent(input$new_characterization_button, {
    updateTabItems(session, "main_sidebar_menu", "characterization_submission")
    shinyjs::show("characterization_submission_menuitem")
    shinyjs::hide("cohort_definition_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    shinyjs::hide("citation_submission_menuitem")
    shinyjs::hide("submission_complete_menuitem")
    shinyjs::show("conditional_hr")
  })

  observeEvent(input$coh_file_delete, {
    reset("coh_link_phenotype_def")
  })

  observeEvent(input$coh_support_delete, {
    reset("coh_supp_doc")
  })

  observeEvent(input$coh_validation_file_delete, {
    reset("coh_validation_upload")
  })

  observeEvent(input$validation_file_delete, {
    reset("val_validation_upload")
  })

  observeEvent(input$validation_check_file_delete, {
    reset("validation_check_upload")
  })

  observeEvent(input$char_stability_delete, {
    reset("char_stability")
  })

  observeEvent(input$char_features_delete, {
    reset("char_features")
  })

  # validation metric fields conditional on a prior validation
  observeEvent(input$coh_previous_validation, {
    if (input$coh_previous_validation == "Yes") {
      show("coh_valid_proc_desc")
      show("coh_valid_data_desc")
      show("coh_test_sample_size")
      show("coh_true_pos")
      show("coh_true_neg")
      show("coh_false_pos")
      show("coh_false_neg")
      show("coh_inconclusive")
    } else {
      hide("coh_valid_proc_desc")
      hide("coh_valid_data_desc")
      hide("coh_test_sample_size")
      hide("coh_true_pos")
      hide("coh_true_neg")
      hide("coh_false_pos")
      hide("coh_false_neg")
      hide("coh_inconclusive")
    }
  })

  # Cohort definition submit button click
  observeEvent(input$coh_submit, {

    # First, disable the button to prevent a double click
    disable("coh_submit")

    # Notify that submission is in progress
    showNotification("Your form is in the saving process. Please wait...",
      duration = NULL,
      closeButton = FALSE,
      type = "warning",
      id = "submission_in_progress"
    )

    # Proceed with Submission Code
    submitCohortDefinitionData(input, session)

    # Then, move the user to the Submission Complete menu
    removeNotification("submission_in_progress")
    shinyjs::show("submission_complete_menuitem")
    shinyjs::hide("cohort_definition_menuitem")
    updateTabItems(session, "main_sidebar_menu", "submission_complete")
  })

  observeEvent(input$coh_submit_another, {
    shinyjs::show("coh_form")
    shinyjs::hide("coh_thankyou_msg")
  })

  # refreshing of the cohort form inputs
  observeEvent(input$coh_refresh, {
    shinyjs::reset("coh_form")
  })

  # refreshing the library page of the applicataion
  observeEvent(input$find_refresh, {
    shinyjs::js$refresh()
  })

  # Validation submit button click
  observeEvent(input$val_submit, {

    # First, disable the button to prevent a double click
    disable("val_submit")

    # Notify that submission is in progress
    showNotification("Your form is in the saving process. Please wait...",
      duration = NULL,
      closeButton = FALSE,
      type = "warning",
      id = "submission_in_progress"
    )

    # Proceed with Submission Code
    submitValidationData(input, session, values_val_upload)

    # On success, move the user to the Submission Complete menu
    removeNotification("submission_in_progress")
    shinyjs::show("submission_complete_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    updateTabItems(session, "main_sidebar_menu", "submission_complete")
  })

  # refresh of validation form
  observeEvent(input$val_refresh, {
    shinyjs::reset("val_form")
  })

  observeEvent(input$citation_submit, {

    # First, disable the button to prevent a double click
    disable("citation_submit")

    # Notify that submission is in progress
    showNotification("Your form is in the saving process. Please wait...",
      duration = NULL,
      closeButton = FALSE,
      type = "warning",
      id = "submission_in_progress"
    )

    # Proceed with Submission Code
    submitCitationData(input, session)

    # On success, move the user to the Submission Complete menu
    removeNotification("submission_in_progress")
    shinyjs::show("submission_complete_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    updateTabItems(session, "main_sidebar_menu", "submission_complete")
  })

  observeEvent(input$char_submit, {

    # First, disable the button to prevent a double click
    disable("char_submit")

    # Notify that submission is in progress
    showNotification("Your form is in the saving process. Please wait...",
      duration = NULL,
      closeButton = FALSE,
      type = "warning",
      id = "submission_in_progress"
    )

    # Proceed with Submission Code
    submitCharacterizationData(input, session)

    # On success, move the user to the Submission Complete menu
    removeNotification("submission_in_progress")
    shinyjs::show("submission_complete_menuitem")
    shinyjs::hide("validation_submission_menuitem")
    updateTabItems(session, "main_sidebar_menu", "submission_complete")
  })

  # Observe logout button click
  observeEvent(input$logout_auth0, {
    shinyjs::runjs(
      '
      var ask_logout = confirm("Are you sure you want to log out? If so, you will lose all unsaved changes.");
      if (ask_logout == true) {
      location.replace("https://www.ohdsi.org")
      }
      '
    )
  })

  # When a row is selected, restore the app in the state of that selected row
  observeEvent(input$mybookmarktable_rows_selected, {
    
    # First, close the modal
    toggleModal(session, "modalLoad", "close")
    
    # Locate the key of the corresponding selected row
    load_key <- as.character(buildBookmarkDF()[input$mybookmarktable_rows_selected, "key"])
    
    # Locate the save name of the corresponding selected row
    load_name <- as.character(buildBookmarkDF()[input$mybookmarktable_rows_selected, "name"])
    
    # # Load RDS file corresponding to the row the user selected in the table
    fn <- paste0(u_uid, "%", load_key, "%", load_name, ".rds")
    loadRDS <- readRDS(file.path(BOOKMARK_PATH, fn))

    # Then go on to restore the state corresponding to this RDS file

    if (loadRDS$main_sidebar_menu == "cohort_defintion_submission") {
      updateTabItems(session, "main_sidebar_menu", "cohort_definition_submission")
      shinyjs::show("cohort_definition_menuitem")
      shinyjs::hide("validation_submission_menuitem")
      shinyjs::hide("citation_submission_menuitem")
      shinyjs::hide("characterization_submission_menuitem")
      shinyjs::hide("submission_complete_menuitem")
      shinyjs::show("conditional_hr")
    } else if (loadRDS$main_sidebar_menu == "validation_submission") {
      updateTabItems(session, "main_sidebar_menu", "validation_submission")
      shinyjs::show("validation_submission_menuitem")
      shinyjs::hide("cohort_definition_menuitem")
      shinyjs::hide("citation_submission_menuitem")
      shinyjs::hide("characterization_submission_menuitem")
      shinyjs::hide("submission_complete_menuitem")
      shinyjs::show("conditional_hr")
    } else if (loadRDS$main_sidebar_menu == "citation_submission") {
      updateTabItems(session, "main_sidebar_menu", "citation_submission")
      shinyjs::show("citation_submission_menuitem")
      shinyjs::hide("cohort_definition_menuitem")
      shinyjs::hide("validation_submission_menuitem")
      shinyjs::hide("characterization_submission_menuitem")
      shinyjs::hide("submission_complete_menuitem")
      shinyjs::show("conditional_hr")
    } else if (loadRDS$main_sidebar_menu == "characterization_submission") {
      updateTabItems(session, "main_sidebar_menu", "characterization_submission")
      shinyjs::show("characterization_submission_menuitem")
      shinyjs::hide("cohort_definition_menuitem")
      shinyjs::hide("validation_submission_menuitem")
      shinyjs::hide("citation_submission_menuitem")
      shinyjs::hide("submission_complete_menuitem")
      shinyjs::show("conditional_hr")
    }
    
    # New Cohort Definition Variables
    if (("author_r_table" %in% names(loadRDS)) && loadRDS$author_r_table$data[[1]][[1]] != ""){
      output$author_r_table <- renderRHandsontable(
        rhandsontable(data.frame(
          Name = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[1]]), "", loadRDS$author_r_table$data[[x]][[1]])),
          Email = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[2]]), "", loadRDS$author_r_table$data[[x]][[2]])),
          Institution = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[3]]), "", loadRDS$author_r_table$data[[x]][[3]])),
          Position = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[4]]), "", loadRDS$author_r_table$data[[x]][[4]])),
          Handle = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[5]]), "", loadRDS$author_r_table$data[[x]][[5]])),
          ORCID = sapply(seq(to = length(loadRDS$author_r_table$data)), function (x) ifelse(is.null(loadRDS$author_r_table$data[[x]][[6]]), "", loadRDS$author_r_table$data[[x]][[6]])),
          stringsAsFactors = FALSE
        ),
        selectCallback = TRUE,
        readOnly = FALSE,
        rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else {
      output$author_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    }
    
    # New Cohort Definition Variables

    # TODO: Restore contributor information

    updateRadioButtons(session,
      inputId = "coh_book_exist",
      selected = loadRDS$coh_book_exist
    )

    updateTextInput(session,
      inputId = "coh_book_title",
      value = loadRDS$coh_book_title
    )

    updateTextAreaInput(session,
      inputId = "coh_book_clinical_description",
      value = loadRDS$coh_book_clinical_description
    )

    updateSelectInput(session,
      inputId = "coh_therapeutic_areas_book",
      selected = loadRDS$coh_therapeutic_areas_book
    )

    updateSelectInput(session,
      inputId = "coh_tags_book",
      selected = loadRDS$coh_tags_book
    )

    updateSelectInput(session,
      inputId = "coh_existing_books",
      selected = loadRDS$coh_existing_books
    )

    # Chapter Information

    updateTextInput(session,
      inputId = "coh_chapter_title",
      value = loadRDS$coh_chapter_title
    )

    updateTextInput(session,
      inputId = "coh_definition_description",
      value = loadRDS$coh_definition_description
    )

    updateTextInput(session,
      inputId = "coh_development_process",
      value = loadRDS$coh_development_process
    )

    updateRadioButtons(session,
      inputId = "coh_phenotype_modality",
      selected = loadRDS$coh_phenotype_modality
    )

    updateSelectInput(session,
      inputId = "coh_therapeutic_areas_chapter",
      selected = loadRDS$coh_therapeutic_areas_chapter
    )

    updateSelectInput(session,
      inputId = "coh_tags_chapter",
      selected = loadRDS$coh_tags_chapter
    )

    # TODO: Definition Upload
    # https://github.com/rstudio/shiny/issues/1729

    # Provenance

    updateRadioButtons(session,
      inputId = "coh_provenance",
      selected = loadRDS$coh_provenance
    )

    updateSelectInput(session,
      inputId = "coh_provenance_book",
      selected = loadRDS$coh_provenance_book
    )

    updateSelectInput(session,
      inputId = "coh_provenance_chapter",
      selected = loadRDS$coh_provenance_chapter
    )

    updateTextAreaInput(session,
      inputId = "coh_provenance_rationale",
      value = loadRDS$coh_provenance_rationale
    )

    # TODO: Restore Provenance Table

    # Validation

    updateRadioButtons(session,
      inputId = "coh_previous_validation",
      selected = loadRDS$coh_previous_validation
    )

    updateTextAreaInput(session,
      inputId = "coh_valid_proc_desc",
      value = loadRDS$coh_valid_proc_desc
    )

    updateTextAreaInput(session,
      inputId = "coh_valid_data_desc",
      value = loadRDS$coh_valid_data_desc
    )

    updateTextInput(session,
      inputId = "coh_cdm_version",
      value = loadRDS$coh_cdm_version
    )

    updateTextInput(session,
      inputId = "coh_vocab_version",
      value = loadRDS$coh_vocab_version
    )

    updateRadioButtons(session,
      inputId = "coh_validation_modality",
      selected = loadRDS$coh_validation_modality
    )

    updateTextInput(session,
      inputId = "coh_true_pos",
      value = loadRDS$coh_true_pos
    )

    updateTextInput(session,
      inputId = "coh_true_neg",
      value = loadRDS$coh_true_neg
    )

    updateTextInput(session,
      inputId = "coh_false_pos",
      value = loadRDS$coh_false_pos
    )

    updateTextInput(session,
      inputId = "coh_false_neg",
      value = loadRDS$coh_false_neg
    )

    updateTextInput(session,
      inputId = "coh_inconclusive",
      value = loadRDS$coh_inconclusive
    )

    # TODO: Supporting Documentation Upload

    # Additional Comments
    updateTextAreaInput(session,
      inputId = "coh_add_comms",
      value = loadRDS$coh_add_comms
    )

    # New Validation Set Variables
    if(("validator_r_table" %in% names(loadRDS)) && loadRDS$validator_r_table$data[[1]][[1]] != ""){
      output$validator_r_table <- renderRHandsontable(
        rhandsontable(data.frame(
          Name = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[1]]), "", loadRDS$validator_r_table$data[[x]][[1]])),
          Email = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[2]]), "", loadRDS$validator_r_table$data[[x]][[2]])),
          Institution = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[3]]), "", loadRDS$validator_r_table$data[[x]][[3]])),
          Position = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[4]]), "", loadRDS$validator_r_table$data[[x]][[4]])),
          Handle = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[5]]), "", loadRDS$validator_r_table$data[[x]][[5]])),
          ORCID = sapply(seq(to = length(loadRDS$validator_r_table$data)), function (x) ifelse(is.null(loadRDS$validator_r_table$data[[x]][[6]]), "", loadRDS$validator_r_table$data[[x]][[6]])),
          stringsAsFactors = FALSE
        ),
        selectCallback = TRUE,
        readOnly = FALSE,
        rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else {
      output$validator_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    }

    # Library Resource
    updateSelectInput(session,
      inputId = "validation_book_selection",
      selected = loadRDS$validation_book_selection
    )

    updateSelectInput(session,
      inputId = "validation_chapter_selection",
      selected = loadRDS$validation_chapter_selection
    )

    # TODO: Restore validation file upload

    # Validation Procedure
    updateTextAreaInput(session,
      inputId = "val_valid_proc_desc",
      value = loadRDS$val_valid_proc_desc
    )

    # Validation Dataset
    updateTextAreaInput(session,
      inputId = "val_valid_data_desc",
      value = loadRDS$val_valid_data_desc
    )

    updateTextInput(session,
      inputId = "val_cdm_version",
      value = loadRDS$val_cdm_version
    )

    updateTextInput(session,
      inputId = "val_vocab_version",
      value = loadRDS$val_vocab_version
    )

    # Performance Metrics

    updateRadioButtons(session,
      inputId = "val_validation_modality",
      selected = loadRDS$val_validation_modality
    )

    updateTextInput(session,
      inputId = "val_true_pos",
      value = loadRDS$val_true_pos
    )

    updateTextInput(session,
      inputId = "val_true_neg",
      value = loadRDS$val_true_neg
    )

    updateTextInput(session,
      inputId = "val_false_pos",
      value = loadRDS$val_false_pos
    )

    updateTextInput(session,
      inputId = "val_false_neg",
      value = loadRDS$val_false_neg
    )

    updateTextInput(session,
      inputId = "val_inconclusive",
      value = loadRDS$val_inconclusive
    )

    # Additional Comments
    updateTextAreaInput(session,
      inputId = "val_add_comms",
      value = loadRDS$val_add_comms
    )

    # Citation Usage
    if(("citation_r_table" %in% names(loadRDS)) && loadRDS$citation_r_table$data[[1]][[1]] != ""){
      output$citation_r_table <- renderRHandsontable(
        rhandsontable(data.frame(
          Name = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[1]]), "", loadRDS$citation_r_table$data[[x]][[1]])),
          Email = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[2]]), "", loadRDS$citation_r_table$data[[x]][[2]])),
          Institution = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[3]]), "", loadRDS$citation_r_table$data[[x]][[3]])),
          Position = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[4]]), "", loadRDS$citation_r_table$data[[x]][[4]])),
          Handle = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[5]]), "", loadRDS$citation_r_table$data[[x]][[5]])),
          ORCID = sapply(seq(to = length(loadRDS$citation_r_table$data)), function (x) ifelse(is.null(loadRDS$citation_r_table$data[[x]][[6]]), "", loadRDS$citation_r_table$data[[x]][[6]])),
          stringsAsFactors = FALSE
        ),
        selectCallback = TRUE,
        readOnly = FALSE,
        rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else {
      output$citation_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    }

    # Library Resource
    updateSelectInput(session,
      inputId = "citation_book_selection",
      selected = loadRDS$citation_book_selection
    )

    updateSelectInput(session,
      inputId = "citation_chapter_selection",
      selected = loadRDS$citation_chapter_selection
    )

    updateRadioButtons(session,
      inputId = "citation_select",
      selected = loadRDS$citation_select
    )

    updateTextInput(session,
      inputId = "citation_data",
      value = loadRDS$citation_data
    )

    # Cohort Characterization

    if(("characterization_r_table" %in% names(loadRDS)) && loadRDS$characterization_r_table$data[[1]][[1]] != ""){
      output$characterization_r_table <- renderRHandsontable(
        rhandsontable(data.frame(
          Name = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[1]]), "", loadRDS$characterization_r_table$data[[x]][[1]])),
          Email = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[2]]), "", loadRDS$characterization_r_table$data[[x]][[2]])),
          Institution = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[3]]), "", loadRDS$characterization_r_table$data[[x]][[3]])),
          Position = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[4]]), "", loadRDS$characterization_r_table$data[[x]][[4]])),
          Handle = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[5]]), "", loadRDS$characterization_r_table$data[[x]][[5]])),
          ORCID = sapply(seq(to = length(loadRDS$characterization_r_table$data)), function (x) ifelse(is.null(loadRDS$characterization_r_table$data[[x]][[6]]), "", loadRDS$characterization_r_table$data[[x]][[6]])),
          stringsAsFactors = FALSE
        ),
        selectCallback = TRUE,
        readOnly = FALSE,
        rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    } else {
      output$characterization_r_table <- renderRHandsontable(
        rhandsontable(AUTHORS_BLANK,
                      selectCallback = TRUE,
                      readOnly = FALSE,
                      rowHeaders = NULL
        ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      )
    }

    # Library Resource
    updateSelectInput(session,
      inputId = "characterization_book_selection",
      selected = loadRDS$characterization_book_selection
    )

    updateSelectInput(session,
      inputId = "characterization_chapter_selection",
      selected = loadRDS$characterization_chapter_selection
    )

    # Data Description
    updateTextAreaInput(session,
      inputId = "characterization_data_description",
      value = loadRDS$characterization_data_description
    )

    updateTextInput(session,
      inputId = "char_cdm_version",
      value = loadRDS$char_cdm_version
    )

    updateTextInput(session,
      inputId = "char_vocab_version",
      value = loadRDS$char_vocab_version
    )

    # TODO: Restore phenotype stability upload

    # TODO: Restore feature extraction upload

    showNotification("State Loaded.", duration = 1, type = "message", closeButton = FALSE)
  })

  ##################################################################################################################################
  # output$X Expressions
  ##################################################################################################################################

  # Bookmark table for loading a bookmark
  output$mybookmarktable <- DT::renderDataTable(datatable(buildBookmarkDF(),
                                                          selection = "single",
                                                          rownames = FALSE
  ))
  
  # Bookmarking caution items
  output$caution <- renderText("No bookmarks currently exist for this user.")
  output$caution2 <- renderText("Are you sure you want to reset all fields for this form?")
  
  # Login Credentials and Logout Button
  output$login_info <- renderUI({
    if (USING_AUTH0) {
      div(
        id = "login_credentials_menuitem",
        menuItem("Logged in as:", icon = icon("key"), tabName = "login_tab"),
        menuSubItem(HTML(paste0("<center><img src=", session$userData$auth0_info$picture, " width = 50></center>")), icon = NULL),
        menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$name, "</span>")), icon = NULL),
        menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$email, "</span>")), icon = NULL),
        actionButton("logout_auth0", "Logout", icon = icon("sign-out-alt"), width = "200px")

        # For reference, these are all of the Auth0 credential fields we can currently use:
        # menuItem("Logged in as:", icon = icon("key"), tabName = "login_tab"),
        # menuSubItem(HTML(paste0("<span>—", names(session$userData$auth0_info), "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$sub, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$given_name, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$family_name, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$nickname, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$name, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$email, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$locale, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<span>—", session$userData$auth0_info$update_at, "</span>")), icon = NULL),
        # menuSubItem(HTML(paste0("<img src=", session$userData$auth0_info$picture, " width = 50>")), icon = NULL)
      )
    }
  })

  # Contributors for Cohort Definition
  output$author_r_table <- renderRHandsontable(
    rhandsontable(AUTHORS_BLANK,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = NULL
    ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  )

  # Contributors for Validation
  output$validator_r_table <- renderRHandsontable(
    rhandsontable(AUTHORS_BLANK,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = NULL
    ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  )

  # Contributors for Characterization
  output$characterization_r_table <- renderRHandsontable(
    rhandsontable(AUTHORS_BLANK,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = NULL
    ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  )

  # Contributors for Citations
  output$citation_r_table <- renderRHandsontable(
    rhandsontable(AUTHORS_BLANK,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = NULL
    ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  )

  # Show the validation items if the author wishes to add validation data at the time of submission
  output$ValidationMenu <- renderUI({
    if (input$coh_previous_validation) {
      div(
        fluidRow(box(
          width = 12,
          title = "Validation Procedure",
          status = "primary",
          textAreaInput("coh_valid_proc_desc",
            "Please describe what you did to validate this cohort definition.",
            resize = "vertical"
          )
        )),

        fluidRow(box(
          width = 12,
          title = "Data Used for Validation",
          status = "primary",
          textAreaInput("coh_valid_data_desc", "Validation Data Description",
            resize = "vertical"
          ),
          textInput("coh_cdm_version", "CDM Version"),
          textInput("coh_vocab_version", "Vocabulary Version")
        )),

        fluidRow(box(
          width = 12,
          title = "Phenotype Validation Metrics",
          status = "primary",
          radioButtons("coh_validation_modality",
            label = "Select Validation Type:",
            choiceNames = c(
              "I will upload a PheValuator validation file.",
              "I will upload an APHRODITE validation file.",
              "I will input metrics manually."
            ),
            choiceValues = c("PheValuator", "APHRODITE", "Manual")
          ),
          uiOutput("CohortValidationModalitySelection")
        ))
      )
    }
  })

  output$ProvenanceBookMenu <- renderUI({
    if (input$coh_provenance) {
      fluidRow(
        box(
          width = 12,
          status = "warning",
          selectInput("coh_provenance_book", "Establish Provenance with an available book:",
            choices = ACCEPTED_BOOKS
          )
        )
      )
    }
  })

  output$ProvenanceChapterMenu <- renderUI({
    if (isTruthy(input$coh_provenance_book)) {
      choices <- subset(prov_check, Book == input$coh_provenance_book & used == FALSE)$Chapter
      if (any(prov_check$Chapter != TRUE)) {
        fluidRow(
          box(
            width = 12,
            status = "warning",
            selectInput("coh_provenance_chapter", "Establish Provenance with an available chapter:",
              choices = choices
            ),
            textAreaInput("coh_provenance_rationale", "What is your rationale for establishing this specific provenance?", resize = "vertical"),
            disabled(actionButton("coh_provenance_update_table", "Update Table")),
            disabled(actionButton("coh_provenance_delete_table", "Remove Row"))
          )
        )
      } else {
        fluidRow(
          box(
            width = 12,
            status = "warning",
            selectInput("coh_provenance_chapter", "Establish Provenance with an available chapter:",
              choices = character(0)
            ),
            textAreaInput("coh_provenance_rationale", "What is your rationale for establishing this specific provenance?", resize = "vertical"),
            disabled(actionButton("coh_provenance_update_table", "Update Table")),
            disabled(actionButton("coh_provenance_delete_table", "Remove Row"))
          )
        )
      }
    }
  })

  # Show the provenance table if the author wishes to establish provenance
  output$ProvenanceMenu <- renderUI({
    if (input$coh_provenance) {
      DTOutput("provenance_table")
    }
  })

  # Show the chapters corresponding to the currently selected book in the citation submission page
  output$CitationChapter <- renderUI({
    chapterChoices <- sort(phe[phe$Book == input$citation_book_selection, "Chapter"])
    selectInput("citation_chapter_selection",
      label = "Select a Cohort Definition (Chapter) from the Above Book:",
      choices = chapterChoices
    )
  })

  # Show the modes of citations
  output$CitationMode <- renderUI({
    radioButtons("citation_select",
      "To cite, I will use...",
      choiceNames = c(
        "PMID (PubMed ID)",
        "PMCID (PubMed ID with PMC prefix)",
        "Manuscript ID",
        "DOI",
        "Manual Entry"
      ),
      choiceValues = c(
        "PMID",
        "PMCID",
        "Manuscript ID",
        "DOI",
        "Manual"
      ),
      selected = "PMID"
    )
  })

  # Show the selected citation mode
  output$CitationChoice <- renderUI({
    if (!is.null(input$citation_select)) {
      if (input$citation_select == "Manual") {
        textAreaInput("citation_data",
          label = "Add a Citation:",
          rows = 3,
          placeholder = 'e.g. Banda JM, Evans L, Vanguri RS, Tatonetti NP, Ryan PB, Shah NH (2016).\n"A curated and standardized adverse drug event resource to accelerate drug safety research."\nScientific data, 3, 160026.',
          resize = "vertical"
        )
      } else {
        textInput(
          "citation_data",
          paste("Add", input$citation_select, ":")
        )
      }
    }
  })

  # Show the chapters corresponding to the currently selected book in the cohort characterization submission page
  output$CharacterizationChapter <- renderUI({
    chapterChoices <- sort(phe[phe$Book == input$characterization_book_selection, "Chapter"])
    selectInput("characterization_chapter_selection",
      label = "Select a Cohort Definition (Chapter) from the Above Book:",
      choices = chapterChoices
    )
  })

  # Show the list of books or allow for creation of a new book
  output$SubmissionBookMenu <- renderUI({
    if (input$coh_book_exist == "Yes") {
      fluidRow(
        box(
          status = "warning",
          selectInput("coh_existing_books", "Add my Chapter (Cohort Definition) to:",
            choices = list(
              "Accepted" = ACCEPTED_BOOKS,
              "Proposed" = PROPOSED_BOOKS
            )
          )
        )
      )
    } else {
      fluidRow(
        box(
          status = "warning", title = "New Book",
          textInput("coh_book_title", "Book (Phenotype) Title"),
          textAreaInput("coh_book_clinical_description", "Clinical Description", resize = "vertical"),
          selectInput("coh_therapeutic_areas_book", "Therapeutic Area(s), if any",
            multiple = TRUE,
            choices = THERAPEUTIC_AREAS
          ),
          # TODO: Leverage the index file to pre-load tags other members have made
          selectizeInput("coh_tags_book",
            "Tag(s), if any (Type to add new tags)",
            choices = c("Claims", "EMR", "Sensitive", "Specific"),
            selected = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          )
        )
      )
    }
  })

  # Show the chapters for the currently selected book in the validation menu
  output$ValidationChapter <- renderUI({
    chapterChoices <- sort(phe[phe$Book == input$validation_book_selection, "Chapter"])
    selectInput("validation_chapter_selection",
      label = "Select a Cohort Definition (Chapter) from the Above Book:",
      choices = chapterChoices
    )
  })

  # Show a fileInput or boxes, depending on the user's choice of validation
  output$ValidationModalitySelection <- renderUI({
    if (input$val_validation_modality == "Manual") {
      fluidRow(box(
        status = "warning",
        textInput("val_true_pos", "True Positives"),
        textInput("val_true_neg", "True Negatives"),
        textInput("val_false_pos", "False Positives"),
        textInput("val_false_neg", "False Negatives"),
        textInput("val_inconclusive", "Inconclusive")
      ))
    } else {
      fluidRow(box(
        status = "warning",
        fileInput("val_validation_upload", label = paste("Upload", input$val_validation_modality, "File:"), multiple = FALSE),
        actionButton("validation_file_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
      ))
    }
  })

  # Same as above, but for the author's validation information
  output$CohortValidationModalitySelection <- renderUI({
    if (input$coh_validation_modality == "Manual") {
      fluidRow(box(
        status = "warning",
        textInput("coh_true_pos", "True Positives"),
        textInput("coh_true_neg", "True Negatives"),
        textInput("coh_false_pos", "False Positives"),
        textInput("coh_false_neg", "False Negatives"),
        textInput("coh_inconclusive", "Inconclusive")
      ))
    } else {
      fluidRow(box(
        status = "warning",
        fileInput("coh_validation_upload", label = paste("Upload", input$coh_validation_modality, "File:"), multiple = FALSE),
        actionButton("coh_validation_file_delete", "Remove Uploaded Files", class = "btn-default", icon = icon("times"))
      ))
    }
  })

  # validator author table
  output$validator_r_table <- renderRHandsontable(
    rhandsontable(AUTHORS_BLANK,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = NULL
    ) %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
  )

  # For rendering the main menu
  output$MainMenu <- renderUI({
    buildMainMenu()
  })

  # For rendering the cohort definition tab
  output$Submit_Definition <- renderUI({
    makeCohortDefinitionForm()
  })

  # For rendering the cohort definition tab
  output$Submit_Validation <- renderUI({
    makeValidationForm()
  })

  # For rendering the citation submission tab
  output$Submit_Citation <- renderUI({
    makeCitationForm()
  })

  # For rendering the cohort characterization submission tab
  output$Submit_Characterization <- renderUI({
    makeCharacterizationForm()
  })

  # For after having completed a submission tab
  output$Submission_Complete <- renderUI({
    makeSubmissionCompleteForm()
  })
} # End server

# Run app
if (USING_AUTH0) {
  # Auth0-based hosting for this app:
  shinyAppAuth0(ui, server)
} else {
  # Bypassing Auth0 for developer mode
  shinyApp(ui, server)
}
