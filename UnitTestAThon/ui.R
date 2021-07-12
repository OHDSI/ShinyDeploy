library(shiny)
library(DT)

shinyUI(
  fluidPage(
    titlePanel("HADES Unit-Test-a-Thon"),
    fluidRow(
      column(12,
             dataTableOutput("mainTable")
      )
    )
  )
)