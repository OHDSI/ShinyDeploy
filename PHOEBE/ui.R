library(shiny)
library(DT)
source("DataPulls.R")

shinyUI(
  pageWithSidebar(
    headerPanel("PHOEBE Initial Concept Selection"),
    sidebarPanel(
      HTML("Select your domain of interest and pass the string to get the recommendation for initial concept selection"),
      textAreaInput(inputId = "sourceDomain",
                    label = "Domain:",
                    value = ""),
      
      textAreaInput(inputId = "sourceString",
                    label = "Search string:",
                    value = ""),
      
      actionButton(inputId = "inputAction", label = "Show Inputs")
    ),
    mainPanel(
      dataTableOutput("table"))
  ))