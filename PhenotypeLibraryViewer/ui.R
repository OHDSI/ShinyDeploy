# OHDSI Gold Standard Phenotype Library Viewer

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

        # TODO: Consider making CSS file for styles (i.e. get rid of most "tags$" expressions)

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
            actionButton("refreshButton", "Refresh", icon = icon("refresh")),
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

            # X out of Y selected
            uiOutput("number_choices"),
            tags$hr(style = "border-color: black;"),

            # Filter options
            fluidRow(
              column(width = 6, box(
                width = NULL, status = "warning",

                pickerInput(
                  inputId = "knobFilters",
                  label = h4("Add Metric Filter:"),
                  choices = c(
                    "Sensitivity",
                    "Specificity",
                    "Positive Predictive Value",
                    "Negative Predictive Value",
                    "Accuracy",
                    "F1 Score"
                  ),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    size = 6,
                    `selected-text-format` = "count > 2"
                  )
                ),

                uiOutput("filter_bank")
              )),

              column(width = 6, box(
                width = NULL, status = "warning",
                # Dependency and Modality Filters

                # Modality
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
                ),

                # CDM Dependencies
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
                ),

                # Demographic Dependencies
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
              ))
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
