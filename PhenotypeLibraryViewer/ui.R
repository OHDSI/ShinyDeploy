# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(ggplot2)
library(knitr)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# TODO: Consider making CSS file for styles (i.e. get rid of most "tags$" expressions)

# TODO: Read the index file on load, e.g.
# phenotypes <- getURL("https://raw.github.com/phenotype/location/index.json")

# UI Definition
shinyUI(
  fluidPage(

    # Dashboard Page
    dashboardPage(
      skin = "yellow",

      # Dashboard Header
      dashboardHeader(title = "PhenoPipe", titleWidth = 250),

      # Dashboard Sidebar
      dashboardSidebar(
        width = 250,
        sidebarMenu(
          menuItem("Find", tabName = "find", icon = icon("search")),
          uiOutput("conditionalHR"),
          menuItemOutput("getInspectMenu"),
          menuItemOutput("getFirstPhenotype"),
          menuItemOutput("getSecondPhenotype"),
          menuItemOutput("getThirdPhenotype"),
          menuItemOutput("getFourthPhenotype"),
          menuItemOutput("getFifthPhenotype"),
          hr(),
          menuItem("About", tabName = "about", icon = icon("info-circle"))
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
        })'
        )),

        tags$h2("Under Development -- Do Not Use", style = "color:red", align = "center"),
        
        # Create Find, Phenotype(s) 1-5, and About tabs
        tabItems(

          # Find
          tabItem(
            tabName = "find",

            h3("Search"),
            tags$hr(style = "border-color: black;"),

            fluidRow(
              column(
                width = 12,
                box(
                  width = 12, status = "primary",
                  title = "Search for a phenotype",
                  selectizeInput("phenotype_search",
                    label = "Select up to 5 phenotypes for comparison:",
                    # TODO: Retrieve choices from GSPL Index File with filters applied
                    # e.g. choices = getFilteredChoices()
                    choices = sort(c(
                      "",
                      # state.name,
                      "Addison’s disease",
                      "Asthma",
                      "Autism",
                      "Bipolar Mood Disorder",
                      "Bronchiectasis",
                      "Cardiac failure",
                      "Cardiomyopathy",
                      "Chronic obstructive pulmonary disorder",
                      "Chronic renal disease",
                      "Coronary artery disease",
                      "Crohn’s disease",
                      "Diabetes insipidus",
                      "Diabetes Type 1",
                      "Diabetes Type 2",
                      "Dysrhythmias",
                      "Epilepsy",
                      "Familial Hyperlipidemia",
                      "Glaucoma",
                      "Hemophilia",
                      "Hyperlipidemia",
                      "Hypertension",
                      "Hypothyroidism",
                      "Metastatic Cancer",
                      "Multiple sclerosis",
                      "Parkinson’s disease",
                      "Schizophrenia",
                      "Systemic lupus erythematosus",
                      "Ulcerative colitis",
                      "Rheumatoid arthritis - V1.0",
                      "Rheumatoid arthritis - V2.0",
                      "Rheumatoid arthritis - V3.0",
                      "Rheumatoid arthritis - V4.0",
                      "Rheumatoid arthritis - V5.0"
                    )),
                    selected = "",
                    multiple = TRUE,
                    options = list(maxItems = 5)
                  )
                )
              )
            ),

            h3("Filter"),
            tags$hr(style = "border-color: black;"),

            # Dependency Filter
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL, status = "warning",
                  pickerInput(
                    inputId = "picker_cdm",
                    label = h4("Allowable Dependencies:"),
                    choices = c("Conditions", "Drug Exposures", "Labs", "Notes NLP", "Observations", "Procedures", "Visits"),
                    selected = c("Conditions", "Drug Exposures", "Labs", "Notes NLP", "Observations", "Procedures", "Visits"),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      size = 7,
                      `selected-text-format` = "count > 3"
                    )
                  )
                )
              ),

              # Modality Filter
              column(
                width = 6,
                box(
                  width = NULL, status = "warning",
                  pickerInput(
                    inputId = "picker_modality",
                    label = h4("Allowable Modalities:"),
                    choices = c("Rule-based (Heuristic)", "Computable (Algorithmic)"),
                    selected = c("Rule-based (Heuristic)", "Computable (Algorithmic)"),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      size = 2,
                      `selected-text-format` = "count > 3"
                    )
                  )
                )
              )
            ),

            # Validation Filter Metrics
            fluidRow(column(
              width = 12,
              box(
                width = NULL, status = "warning",
                h4("Minimum Allowable Metric Percentage:"),
                column(width = 12, awesomeRadio("validation_filter_metric",
                  "Based on Validation:",
                  choices = c("Average", "Median"),
                  inline = TRUE,
                  status = "warning"
                )),

                fluidRow(
                  width = 12,
                  column(width = 3, h4("Sensitivity")),
                  column(width = 3, h4("Specificity")),
                  column(width = 3, h4("Positive Predictive Value")),
                  column(width = 3, h4("Negative Predictive Value"))
                ),

                fluidRow(
                  width = 12,
                  column(width = 3, knobInput("knob_sensitivity", label = NULL, value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "100%")),
                  column(width = 3, knobInput("knob_specificity", label = NULL, value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "100%")),
                  column(width = 3, knobInput("knob_ppv", label = NULL, value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "100%")),
                  column(width = 3, knobInput("knob_npv", label = NULL, value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "100%"))
                )
              )
            ))
          ), # End Find

          # Inspect
          tabItem(
            tabName = "inspect",
            uiOutput("inspect_tab")
          ), # End Inspect

          # TODO: Wrap similar Tab code in function calls -- Part 1 of 3

          # Tab 1
          tabItem(
            tabName = "tab1",
            uiOutput("tab1")
          ),

          # Tab 2
          tabItem(
            tabName = "tab2",
            uiOutput("tab2")
          ),

          # Tab 3
          tabItem(
            tabName = "tab3",
            uiOutput("tab3")
          ),

          # Tab 4
          tabItem(
            tabName = "tab4",
            uiOutput("tab4")
          ),

          # Tab 5
          tabItem(
            tabName = "tab5",
            uiOutput("tab5")
          ),

          # About
          tabItem(
            tabName = "about",
            fluidRow(column(6, offset = 3, box(width = NULL, div(img(src = "OHDSI_Logo.png", align = "center"), style = "text-align: center;")))),
            fluidRow(column(12, box(width = NULL, includeMarkdown("about.md"))))
          )
        ) # End tabItems
      ) # End dashboardBody
    ) # End dashboardPage
  ) # End fluidPage
) # End UI
