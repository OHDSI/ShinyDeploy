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
             selectInput("man",
                         "Manufacturer:",
                         c("All",
                           unique(as.character(mpg$manufacturer))))
      ),
      column(4,
             selectInput("trans",
                         "Transmission:",
                         c("All",
                           unique(as.character(mpg$trans))))
      ),
      column(4,
             selectInput("cyl",
                         "Cylinders:",
                         c("All",
                           unique(as.character(mpg$cyl))))
      )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table"),
    h1("Notes"),
    verbatimTextOutput("note")
  )
)
