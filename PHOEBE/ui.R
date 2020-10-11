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
              textAreaInput(inputId = "sourceDomain",
                            label = "Domain:",
                            value = ""),
              textAreaInput(inputId = "sourceString",
                            label = "Search string:",
                            value = ""),
              actionButton(inputId = "inputActionConcept", label = "Show recommendations"),
              dataTableOutput("InitialConceptTable")
      ),
      tabItem(tabName = "ConceptSet",
              htmlOutput("borderConceptSet"),
              tabsetPanel(
                tabPanel("Standard Concepts",
                         textAreaInput(inputId = "conceptList",
                                       label = "Comma-separated concept list:",
                                       value = ""),
                         actionButton(inputId = "inputActionList", label = "Show recommendations"),
                         dataTableOutput("ConceptSetStandardTable"),
                         downloadButton("dlConceptSetStandard", "Download Data")
                         ),
                
                
                tabPanel("Source Concepts",dataTableOutput("ConceptSetSourceTable"))
              )
       )
      
    )
  )
)
