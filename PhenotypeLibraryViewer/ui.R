# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(ggplot2)
library(knitr)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# TODO: Consider making CSS file for styles (i.e. get rid of most "tags$" expressions)

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
                         })')),

        tags$h2("Under Development -- Do Not Use", style = "color:red", align = "center"),

        # Create Find, Phenotype(s) 1-5, and About tabs
        tabItems(

          # Find
          tabItem(
            tabName = "find",

            h3("Search"),
            actionButton("refreshButton", "Refresh", icon=icon("refresh")),
            tags$hr(style = "border-color: black;"),

            fluidRow(
              column(
                width = 12,
                box(
                  width = 12, status = "primary",
                  title = "Search for a phenotype",
                  uiOutput("filtered_phenotypes")
                )
              )
            ),

            uiOutput("number_choices"),
            tags$hr(style = "border-color: black;"),

            # Validation Filter Metrics
            fluidRow(

              # Knobs for sensitivity, specificity, PPV, and NPV
              column(
                width = 6,
                box(
                  width = NULL, status = "warning",
                  h4("Minimum Allowable Metrics:"),
                  h6("This is based on validation set averages, weighted by sample size."),

                  fluidRow(
                    width = 12,
                    column(width = 6, knobInput("knob_sensitivity", label = h4("Sensitivity"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%")),
                    column(width = 6, knobInput("knob_specificity", label = h4("Specificity"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"))
                  ),

                  fluidRow(
                    width = 12,
                    column(width = 6, knobInput("knob_ppv", label = h4("Positive Predictive Value"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%")),
                    column(width = 6, knobInput("knob_npv", label = h4("Negative Predictive Value"), value = 0, min = 0, max = 100, lineCap = "round", fgColor = "orange", width = "60%"))
                  )
                )
              ),

              # Dependency and Modality Filters
              column(
                width = 6,

                # Modality
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
                ),

                # CDM Dependencies
                box(
                  width = NULL, status = "warning",
                  pickerInput(
                    inputId = "picker_cdm",
                    label = h4("Allowable CDM Dependencies:"),
                    choices = c("Conditions", "Drug Exposures", "Labs", "Measurements", "Notes NLP", "Observations", "Procedures", "Visits"),
                    selected = c("Conditions", "Drug Exposures", "Labs", "Measurements", "Notes NLP", "Observations", "Procedures", "Visits"),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      size = 7,
                      `selected-text-format` = "count > 3"
                    )
                  )
                ),

                # Demographic Dependencies
                box(
                  width = NULL, status = "warning",
                  pickerInput(
                    inputId = "picker_demographics",
                    label = h4("Allowable Demographic Dependencies:"),
                    choices = c("Age", "Sex"),
                    selected = c("Age", "Sex"),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      size = 7,
                      `selected-text-format` = "count > 3"
                    )
                  )
                )
              )
            )
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
