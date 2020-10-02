library(shiny)
library(DT)
source("DataPulls.R")

shinyUI(
  pageWithSidebar(
  headerPanel("PHOEBE Initial conccept selection"),
  sidebarPanel(
          HTML("Select your domain of interest and pass the string to get the recommendation for initial concept selection"),
           textAreaInput(inputId="source_domain",
                         label = "Domain:",
                         value=""),
         
           textAreaInput(inputId="source_string",
                         label = "Search string:",
                         value=""),
         
            actionButton(inputId = "input_action", label = "Show Inputs")
  ),
  mainPanel(
    dataTableOutput("table"))
))