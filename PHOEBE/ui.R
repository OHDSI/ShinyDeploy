library(shiny)
library(DT)
library(shinydashboard)
source("DataPulls.R")


ohdsiBlueHex <- "#20425A"
ohdsiLightYellowHex <- "FBC209"

dashboardPage(
  dashboardHeader(
    title = "PHOEBE",
    tags$li(div(img(src = 'logo.png',
                    title = "Initial concept and concept set reommender", 
                    height = "40px", 
                    width = "40px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
  ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                menuItem("About", tabName = "about"),
                menuItem("Initial Concept", tabName = "InitialConcept"),
                menuItem("Concept Set Recommender", tabName="ConceptSet")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("md/about.md")
      ),
      tabItem(tabName = "InitialConcept",
              htmlOutput("borderInitialConcept"),
              HTML("This page provides you a recommendation for initial concept to be used with descendants for your initial concept set"),
              fluidRow(tags$br()),
              textAreaInput(inputId = "sourceDomain",
                            label = "Insert your domain of interest:",
                            value = ""),
              textAreaInput(inputId = "sourceString",
                            label = "Search string:",
                            value = ""),
              actionButton(inputId = "inputActionConcept", label = "Show recommendations"),
              fluidRow(tags$br()),
              dataTableOutput("InitialConceptTable")
      ),
      tabItem(tabName = "ConceptSet",
              htmlOutput("borderConceptSet"),
              textAreaInput(inputId = "conceptList",
                            label = "Insert your comma-separated concept list:",
                            value = ""),
              actionButton(inputId = "inputActionList", label = "Show recommendations"),
              fluidRow(tags$br()),
              tabsetPanel(
                tabPanel("Standard Concepts",
                         fluidRow(tags$br()),
                         HTML("This page provides you standard concept recommendations to modify your concept set.
                              <br> Proceed to next tab to see recommendations for non-standard concepts."),
                         fluidRow(tags$br()),
                         fluidRow(tags$br()),
                         dataTableOutput("ConceptSetStandardTable"),
                         downloadButton("dlConceptSetStandard", "Download Data")
                         ),
                tabPanel("Source Concepts",
                         fluidRow(tags$br()),
                         HTML("This page shows you non-standard candidates for your concept set with corresponsing standard concepts."),
                         fluidRow(tags$br()),
                         dataTableOutput("ConceptSetSourceTable"),
                         fluidRow(tags$br()),
                         downloadButton("dlConceptSetSource", "Download Data"))
              )
       )
      
    )
  )
)
