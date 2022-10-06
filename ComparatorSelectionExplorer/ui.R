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

    # sidebar with option for target cohort selection
    sidebarLayout(
        sidebarPanel(
          h4(strong("Settings")),
            shiny::selectizeInput(
              inputId = "selectedExposure",
              choices=NULL, 
              label = "Select target exposure:"),
            shiny::selectizeInput(
              inputId = "selectedComparator",
              choices=NULL, 
              label = "Comparator exposure for detail:"),
            shiny::selectInput(inputId = "selectedComparatorTypes",
                               label = "Select comparator type(s):", 
                               choices = c("RxNorm Ingredients", "ATC Classes"), 
                               selected = "RxNorm Ingredients", 
                               multiple = TRUE),
            h4(strong("Visualizations")),
            h6(em("Similarity scores by domain-specific ranking")),
            shiny::plotOutput(
              outputId = "stepPlot"),
            h6(em("Covariate prevalence")),
            shiny::plotOutput(
              outputId = "scatterPlot"
            ),
            h6(em("Standardized mean differences")),
            shiny::plotOutput(
              outputId = "smdPlot"
            ),
        ),

        # display table
        mainPanel(
            h3(strong("Comparator listing")),
            shinycssloaders::withSpinner(reactable::reactableOutput("cosineSimilarityTbl")),
            h3(strong("Distribution of covariates")),
            h4(strong("Demographics")),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableDemo")),
            h4(strong("Presentation")),
            h5(em("One covariate per condition observed in 30 days prior to index")),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTablePres")),
            h4(strong("Medical history")),
            h5(em("One covariate per condition observed more than 30 days prior to index")),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableMhist")),
            h4(strong("Prior medications")),
            h5(em("One covariate per RxNorm ingredient observed more than 30 days prior to index")),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTablePmeds")),
            h4(strong("Visit context")),
            h5(em("Inpatient and emergency department visits observed in 30 days prior to index")),
            shinycssloaders::withSpinner(reactable::reactableOutput("covTableVisit"))
        )
    )
))
