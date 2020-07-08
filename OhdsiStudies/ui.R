library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel(
    title = div(img(src = "logo.png", height = 50, width = 50), "OHDSI Studies"),
    windowTitle = "OHDSI Studies"
  ),
  style = "width:1500px;",
  p("OHDSI is a global, open-science community that is committed to generating real-world evidence to both support clinical decision-making and advance the methodology within this field. We have collaborated on many network studies across our community, many of which (both past and ongoing) are listed in this table. Please click on any listing that interests you to learn more about the study, and how you can potentially collaborate to generate reliable, reproducible evidence."),
  dataTableOutput("mainTable"),
  uiOutput("detailsUi"),
  uiOutput("lastUpdated")
  
  
)
