library(shiny)
library(DT)
source("widgets.R")

shinyUI(fluidPage(style = "width:1000px;",
                  titlePanel(title = div(img(src = "logo.png", height = 50, width = 50), 
                                         "LegendMed Central"),
                             windowTitle = "PubLegend Central"),
                  verticalLayout(
                    div(style = "background-color: #CCCCCC; padding: 8px;", 
                        radioButtons("queryType", label = NULL, choices = c("Free-text", "Structured"), inline = TRUE),
                        conditionalPanel("input.queryType == 'Free-text'",
                                         textInput("query", label = "", placeholder = "Enter your search here", width = "100%"),
                                         searchButton("textSearchButton", "Search",  structured = FALSE)
                        ),
                        conditionalPanel("input.queryType == 'Structured'",
                                         fluidRow(
                                           # column(4, selectInput("indication", "Indication", c("All", indications$indicationId))),
                                           column(4, selectInput("exposureGroup", "Exposure group", c("All", unique(exposureGroups$exposureGroup))))
                                         ),
                                         fluidRow(
                                           column(4, selectInput("target", "Target", c("All", unique(exposures$exposureName)))),
                                           column(4, selectInput("comparator", "Comparator", c("All", unique(exposures$exposureName))))
                                         ),
                                         selectInput("outcome", "Outcome", c("All", unique(outcomes$outcomeName))),
                                         selectInput("database", "Database", c("All", databases$databaseId)),
                                         searchButton("structuredSearchButton", "Search", structured = TRUE)
                        )
                    ),
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
