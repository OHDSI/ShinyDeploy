library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel(
    title = div(img(src = "logo.png", height = 50, width = 50), "OHDSI Studies"),
    windowTitle = "OHDSI Studies"
  ),
  style = "width:1500px;",
  dataTableOutput("mainTable"),
  uiOutput("detailsUi"),
  uiOutput("lastUpdated")
  
  
)
