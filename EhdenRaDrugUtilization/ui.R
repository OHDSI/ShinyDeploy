library(shiny)
library(shinydashboard)
library(DT)

ohdsiBlueHex <- "#20425A"
ohdsiLightYellowHex <- "FBC209"

addInfo <- function(item, infoId, class = NULL, style = NULL) {
  if (is.null(class)) {
    class = "badge pull-right action-button"
  }
  if (is.null(style)) {
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;"
  }
  infoTag <- tags$small(class = class,
                        style = style,
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

dashboardPage(
  dashboardHeader(
    title = "EHDEN RA DUS",
    tags$li(div(img(src = 'logo.png',
                    title = "EHDEN Rheumatoid Arthritis Drug Utilization Study", 
                    height = "40px", 
                    width = "40px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
    ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                menuItem("About", tabName = "about"),
                menuItem("csDMARD Totals", tabName = "totals"),
                menuItem("Secular Trends", tabName = "trends"),
                menuItem("Database information", tabName = "databaseInformation")#,
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="ohdsi.css")
    ),
    ### changing theme
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("md/about.md")
      ),
      tabItem(tabName = "totals",
        radioButtons("csDmardTotalTableType", "", c("Plot", "Pivoted Table", "Raw Table"), selected = "Plot", inline = TRUE),
        conditionalPanel(condition = "input.csDmardTotalTableType=='Raw Table'",
           dataTableOutput("csDmardTotalTable")
        ),
        conditionalPanel(condition = "input.csDmardTotalTableType=='Pivoted Table'",
           dataTableOutput("csDmardPivotTable")
        ),
        conditionalPanel(condition = "input.csDmardTotalTableType=='Plot'",
           box(
             title = "First line csDMARD treatment during 1yr from first observed RA Diagnosis", width = NULL, status = "primary",
             plotOutput("csDmardTotalPlot", height = 700, hover = hoverOpts("plotHoverTotalPlot", delay = 100, delayType = "debounce"))
           )
        )
      ),
      tabItem(tabName = "trends",
        radioButtons("csDmardTrendTableType", "", c("Plot", "Pivoted Table", "Raw Table"), selected = "Plot", inline = TRUE),
        conditionalPanel(condition = "input.csDmardTrendTableType=='Raw Table'",
           dataTableOutput("csDmardTrendTable")
        ),
        conditionalPanel(condition = "input.csDmardTrendTableType=='Pivoted Table'",
           dataTableOutput("csDmardTrendPivotTable")
        ),
        conditionalPanel(condition = "input.csDmardTrendTableType=='Plot'",
         box(
           title = "Secular trends of RA first line treatment by database", width = NULL, status = "primary",
           plotOutput("csDmardTrendPlotUS", hover = hoverOpts("plotHoverTotalPlotUS", delay = 100, delayType = "debounce")),
           plotOutput("csDmardTrendPlotEU", hover = hoverOpts("plotHoverTotalPlotEU", delay = 100, delayType = "debounce")),
           plotOutput("csDmardTrendPlotAP", hover = hoverOpts("plotHoverTotalPlotAP", delay = 100, delayType = "debounce"))
         )
        )
      ),
     tabItem(tabName = "databaseInformation",
              downloadButton("dlDatabaseInformation", "Download Data"),
              htmlOutput("borderDatabaseInformation"),
              dataTableOutput("databaseInformationTable")
      )
    )
  )
)