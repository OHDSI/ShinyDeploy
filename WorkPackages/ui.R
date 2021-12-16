library(shinydashboard)
#library(bs4Dash)
#library(shinymaterial)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "Work Packages"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    h1("All available work packages"),
    fluidRow(
      column(4,
             selectInput("packageId",
                         "Package ID:",
                         c("All",
                           unique(as.character(packageDescriptions$packageId))))
      ),
      column(4,
             selectInput("textField",
                         "Some text:",
                         c("All",
                           unique(as.character(packageDescriptions$textField))))
      )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table"),
    h1("Notes"),
    verbatimTextOutput("note")
  )
)
