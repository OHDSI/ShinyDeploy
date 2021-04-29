ohdsiBlueHex <- "#20425A"
ohdsiLightYellowHex <- "FBC209"
dashboardPage(
  dashboardHeader(
    title = "Incidence AESI",
    tags$li(div(img(src = 'logo.png',
                    title = "Characterizing Incidence AESI", 
                    height = "40px", 
                    width = "40px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
    ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                menuItem("Results", tabName = "results"),
                menuItem("About", tabName = "about"),
                menuItem("Data Source", tabName = "dataSource")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("md/about.md")
      ),
     tabItem(tabName = "results",
             column(
               4,
               shinyWidgets::pickerInput(
                 inputId = "sexFilter",
                 label = "Filter by Sex",
                 choices = c(),
                 selected = c(),
                 multiple = TRUE
               )
             ),
             column(4,
             shinyWidgets::pickerInput(inputId = "ageFilter",label = "Filter by Age Group",choices = c(),selected = c(),multiple = TRUE)
             ),
             column(4,
             shinyWidgets::pickerInput(inputId = "databaseFilter",label = "Filter by Database",choices = c(),selected = c(),multiple = TRUE)
             ),
             tabsetPanel(type = "tabs",
                         tabPanel("Plot", plotOutput(outputId = "outputPlot",height = "1000px")),
                         tabPanel("Table", DT::DTOutput("resultTable"))
             )
            
      ),
     tabItem(tabName = "dataSource",
             shinydashboard::box(DT::DTOutput("dataSourceTable"),width = NULL))
    )
  )
)