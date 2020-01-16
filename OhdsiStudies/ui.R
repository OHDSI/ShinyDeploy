library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel("OHDSI Studies"),
  style = "width:1500px;",
  dataTableOutput("mainTable"),
  uiOutput("detailsUi"),
  uiOutput("lastUpdated")
  

)
