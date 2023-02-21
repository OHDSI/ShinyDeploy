#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(

  # Application title
  titlePanel(
    title = "Comparator Selection Explorer",
    windowTitle = "Comparator Selection Explorer"),
  p("Janssen Research & Development"),
  # sidebar with option for target cohort selection

  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "App",
      sidebarLayout(
        sidebarPanel(
          h4(strong("Settings")),
          shiny::selectizeInput(
            inputId = "selectedDatabase",
            choices = NULL,
            label = "Select target data source:"),
          shiny::selectizeInput(
            inputId = "selectedExposure",
            choices = NULL,
            label = "Select target exposure:"),
          shiny::selectInput(inputId = "selectedComparatorTypes",
                             label = "Select comparator type(s):",
                             choices = c("RxNorm Ingredients", "ATC Classes"),
                             selected = "RxNorm Ingredients",
                             multiple = TRUE),
          h4(strong("Visualizations")),
          h6(em("Similarity scores by domain-specific ranking")),
          shinycssloaders::withSpinner(
            plotOutput(
              outputId = "stepPlot"),
          ),
          shiny::conditionalPanel(
            condition = "output.selectedComparator == true",
            h6(em("Covariate prevalence")),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                outputId = "scatterPlot"
              )
            ),
            h6(em("Standardized mean differences")),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                outputId = "smdPlot"
              )
            )
          )
        ),

        # display table
        mainPanel(
          h3("Comparator listing"),
          shiny::textOutput("selectedCohortInfo"),
          p("Select comparator to view covariate distributions"),
          shinycssloaders::withSpinner(reactable::reactableOutput("cosineSimilarityTbl")),

          shiny::conditionalPanel(
            condition = "output.selectedComparator == true",
            h3(strong("Distribution of covariates")),
            h4(strong("Demographics")),
            shiny::textOutput("covTableDemoBalance"),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableDemo")),
            h4(strong("Presentation")),
            h5(em("One covariate per condition observed in 30 days prior to index")),
            shiny::textOutput("covTablePresBalance"),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTablePres")),
            h4(strong("Medical history")),
            h5(em("One covariate per condition observed more than 30 days prior to index")),
            shiny::textOutput("covTableMhistBalance"),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableMhist")),
            h4(strong("Prior medications")),
            h5(em("One covariate per RxNorm ingredient observed more than 30 days prior to index")),
            shiny::textOutput("covTablePmedsBalance"),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTablePmeds")),
            h4(strong("Visit context")),
            h5(em("Inpatient and emergency department visits observed in 30 days prior to index")),
            shiny::textOutput("covTableVisitBalance"),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableVisit"))
          ),
        )
      )
    ),
    tabPanel(
      title = "About",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          h3("Description"),
          shiny::htmlTemplate("about.html"),
          h3("Currently Available Data Sources"),
          shinycssloaders::withSpinner(
            reactable::reactableOutput("dataSources")
          ),
          h3("License"),
          shiny::htmlTemplate("license.html"),
        )
      )
    )
  )
))
