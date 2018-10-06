library(shiny)
library(DT)
source("widgets.R")

shinyUI(fluidPage(style = "width:1000px;",
                  titlePanel(title=div(img(src="logo.png", height = 50, width = 50), 
                            "LegendMed Central"),
                            windowTitle = "PubLegend Central"),
                  verticalLayout(div(style = "background-color: #AAAAAA;", tags$table(border = 0, width = "100%", tags$tr(tags$td(HTML("&nbsp;")), tags$td(textInput("query", label = "", placeholder = "Enter your search here", width = "100%")), tags$td(HTML("&nbsp;")), tags$td(align = "left", searchButton("searchButton", "Search", "query"))))),
                                 conditionalPanel("output.isSearchResultPage == true", dataTableOutput("searchResults")),
                                 conditionalPanel("output.isAbstractPage == true", 
                                                  uiOutput("abstract"), 
                                                  downloadLink("pdf",
                                                               label = paste("Generate and download report PDF")
                                                               )
                                                  )
                                 )
                  )
        )
