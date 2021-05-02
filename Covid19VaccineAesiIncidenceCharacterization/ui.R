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
                menuItem("About", tabName = "about"),
                menuItem("Data Sources", tabName = "dataSource"),
                menuItem("Cohort definitions", tabName = "cohortDefinitions"),
                menuItem("Cohort Results", tabName = "results")
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML('.content-wrapper {height: auto !important; position:relative; overflow:auto}')
    )),
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("md/about.md")
      ),
      tabItem(tabName = "cohortDefinitions",
              shinydashboard::box(DT::DTOutput("cohortTable"),width = NULL),
              column(
                12,
                conditionalPanel(
                  "output.cohortDefinitionRowIsSelected == true",
                  shiny::tabsetPanel(
                    type = "tab",
                    shiny::tabPanel(title = "Details",
                                    shiny::htmlOutput("cohortDetailsText")),
                    shiny::tabPanel(title = "Cohort definition",
                                    copyToClipboardButton(toCopyId = "cohortDefinitionText",
                                                          style = "margin-top: 5px; margin-bottom: 5px;"),
                                    shiny::htmlOutput("cohortDefinitionText")),
                    shiny::tabPanel(
                      title = "Concept Sets",
                      shiny::downloadButton(
                        "saveConceptSetButton",
                        label = "Save to CSV file",
                        icon = shiny::icon("download"),
                        style = "margin-top: 5px; margin-bottom: 5px;"
                      ),
                      DT::dataTableOutput(outputId = "conceptsetExpressionTable"),
                      shiny::conditionalPanel(condition = "output.conceptSetExpressionRowSelected == true",
                                              tags$table(tags$tr(
                                                tags$td(
                                                  shiny::radioButtons(
                                                    inputId = "conceptSetsType",
                                                    label = "",
                                                    choices = c("Concept Set Expression",
                                                                "Json"),
                                                    selected = "Concept Set Expression",
                                                    inline = TRUE
                                                  )
                                                ),
                                              ))),
                      shiny::conditionalPanel(
                        condition = "output.conceptSetExpressionRowSelected == true &
                input.conceptSetsType != 'Json'",
                        DT::dataTableOutput(outputId = "cohortDefinitionConceptSetsTable")
                      ),
                      shiny::conditionalPanel(
                        condition = "input.conceptSetsType == 'Json'",
                        copyToClipboardButton(toCopyId = "cohortConceptsetExpressionJson",
                                              style = "margin-top: 5px; margin-bottom: 5px;"),
                        shiny::verbatimTextOutput(outputId = "cohortConceptsetExpressionJson"),
                        tags$head(
                          tags$style("#cohortConceptsetExpressionJson { max-height:400px};")
                        )
                      )
                    ),
                    shiny::tabPanel(
                      title = "JSON",
                      copyToClipboardButton("cohortDefinitionJson", style = "margin-top: 5px; margin-bottom: 5px;"),
                      shiny::verbatimTextOutput("cohortDefinitionJson"),
                      tags$head(tags$style(
                        "#cohortDefinitionJson,#cohortDefinitionSql { max-height:400px;overflow:auto;};"
                      ))
                    ),
                    shiny::tabPanel(
                      title = "SQL",
                      copyToClipboardButton("cohortDefinitionSql", style = "margin-top: 5px; margin-bottom: 5px;"),
                      shiny::verbatimTextOutput("cohortDefinitionSql")
                    )
                  )
                )
              )),
     tabItem(tabName = "results",
             column(
               4,
               shinyWidgets::pickerInput(
                 inputId = "sexFilter",
                 label = "Filter by Sex",
                 choices = c(),
                 selected = c(),
                 multiple = TRUE,
                 choicesOpt = list(style = rep_len("color: black;", 999)),
                 options = shinyWidgets::pickerOptions(
                   actionsBox = TRUE,
                   liveSearch = TRUE,
                   size = 10,
                   liveSearchStyle = "contains",
                   liveSearchPlaceholder = "Type here to search",
                   virtualScroll = 50
                 )
               )
             ),
             column(4,
             shinyWidgets::pickerInput(inputId = "ageFilter",
                                       label = "Filter by Age Group",
                                       choices = c(),
                                       selected = c(),
                                       multiple = TRUE,
                                       choicesOpt = list(style = rep_len("color: black;", 999)),
                                       options = shinyWidgets::pickerOptions(
                                         actionsBox = TRUE,
                                         liveSearch = TRUE,
                                         size = 10,
                                         liveSearchStyle = "contains",
                                         liveSearchPlaceholder = "Type here to search",
                                         virtualScroll = 50
                                       ))
             ),
             column(4,
             shinyWidgets::pickerInput(inputId = "databaseFilter",
                                       label = "Filter by Database",
                                       choices = c(),
                                       selected = c(),
                                       multiple = TRUE,
                                       choicesOpt = list(style = rep_len("color: black;", 999)),
                                       options = shinyWidgets::pickerOptions(
                                         actionsBox = TRUE,
                                         liveSearch = TRUE,
                                         size = 10,
                                         liveSearchStyle = "contains",
                                         liveSearchPlaceholder = "Type here to search",
                                         virtualScroll = 50
                                       ))
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