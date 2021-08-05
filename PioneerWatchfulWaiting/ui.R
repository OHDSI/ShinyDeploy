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
    title = "PIONEER / EHDEN / OHDSI Watchful Waiting study",
    tags$li(div(img(src = 'logo.png',
                    title = "Characterizing long term outcomes of prostate cancer patients undergoing non-interventional management", 
                    height = "60px", 
                    width = "200px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
    ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                # Used for testing cookie set/remove
                #actionButton("cookieGetVal", "Cookie value"),
                #actionButton("cookieRmVal", "Cookie Remove"),
                menuItem("About", tabName = "about"),
                menuItem("Cohorts", tabName = "cohorts"),
                if (exists("cohortCount")) addInfo(menuItem("Cohort Counts", tabName = "cohortCounts"), "cohortCountsInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Cohort Characterization", tabName = "cohortCharacterization"), "cohortCharacterizationInfo"),
                if (exists("cohortTimeToEvent")) addInfo(menuItem("Time To Event", tabName = "timeToEvent"), "cohortCountsInfo"),
                if (exists("metricsDistribution")) addInfo(menuItem("Metrics Distribution", tabName = "metricsDistribution"), "cohortsCountsInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Compare Cohort Char.", tabName = "compareCohortCharacterization"), "compareCohortCharacterizationInfo"),
                menuItem("Database information", tabName = "databaseInformation"), 
                menuItem("Change Log", tabName="changeLog"),
                conditionalPanel(condition = "input.tabs=='compareCohortCharacterization'",
                   selectInput("database", "Database", database$databaseId, selectize = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='cohortCharacterization' | input.tabs=='cohortCounts'",
                   #checkboxGroupInput("databases", "Database", database$databaseId, selected = database$databaseId[1])
                   shinyWidgets::pickerInput("databases", "Database",
                                             choices = database$databaseId,
                                             selected = database$databaseId,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = TRUE)
                ),
                conditionalPanel(condition = "input.tabs=='cohortCounts'",
                   shinyWidgets::pickerInput("targetCohortList", "Cohort",
                                             choices = targetCohort$targetName,
                                             selected = targetCohort$targetName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = TRUE),
                   shinyWidgets::pickerInput("strataCohortList", "Strata",
                                             choices = strataCohort$strataName,
                                             selected = strataCohort$strataName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = TRUE)
                ),
                
                conditionalPanel(condition = "input.tabs=='timeToEvent'",
                                 shinyWidgets::pickerInput("databasesTimeToEvent", "Database",
                                                           choices = database$databaseId,
                                                           selected = database$databaseId,
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE),
                                 shinyWidgets::pickerInput("targetTimeToEvent", "Cohort",
                                                           choices = targetCohort$targetName,
                                                           selected = 'All',
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE),
                                 shinyWidgets::pickerInput("strataTimeToEvent", "Strata",
                                                           choices = strataCohort$strataName,
                                                           selected = 'All',
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE), 
                                 shinyWidgets::pickerInput("KMPlot", "Plot Choice",
                                                           choices = KMIds$name,
                                                           selected = KMIds$name,
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = TRUE )
                ),
                
                conditionalPanel(condition = "input.tabs=='metricsDistribution'",
                                 shinyWidgets::pickerInput("databasesMetricsDistribution", "Database",
                                                           choices = database$databaseId,
                                                           selected = database$databaseId,
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE),
                                 shinyWidgets::pickerInput("targetMetricsDistribution", "Cohort",
                                                           choices = targetCohort$targetName,
                                                           selected = 'All',
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE),
                                 shinyWidgets::pickerInput("strataMetricsDistribution", "Strata",
                                                           choices = strataCohort$strataName,
                                                           selected = 'All',
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE)
                ),

                conditionalPanel(condition = "input.tabs!='about' & input.tabs!='cohorts' & input.tabs!='cohortCounts' & input.tabs!='timeToEvent' & input.tabs!='metricsDistribution' & input.tabs!='databaseInformation' & input.tabs!='changeLog'" ,
                   shinyWidgets::pickerInput("targetCohort", "Cohort (Target)",
                                             choices = characterizationTargetCohort$targetName,
                                             selected = targetName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE),
                   shinyWidgets::pickerInput("strataCohort", "Strata (Target)",
                                             choices = characterizationStrataCohort$strataName,
                                             selected = strataName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='compareCohortCharacterization'",
                   shinyWidgets::pickerInput("comparatorCohort", "Cohort (Comparator)",
                                             choices = characterizationTargetCohort$targetName,
                                             selected = comparatorName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE),
                   shinyWidgets::pickerInput("comparatorStrataCohort", "Strata (Comparator)",
                                             choices = characterizationStrataCohort$strataName,
                                             selected = comparatorStrataName,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='cohortCharacterization' | input.tabs=='compareCohortCharacterization'",
                                 shinyWidgets::pickerInput("domainFilter", "Domain",
                                                           choices = unique(domain$name),
                                                           selected = domainName,
                                                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                                                           multiple = FALSE)
                ),
                conditionalPanel(condition = "(input.tabs=='cohortCharacterization' | input.tabs=='compareCohortCharacterization') & input.domainFilter!='Demographics'",
                   shinyWidgets::pickerInput("timeWindowFilter", "Time Window",
                                             choices = timeWindow$name,
                                             selected = timeWindow$name,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                                             multiple = TRUE)
                )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="ohdsi.css"),
      tags$script(src = "js/lib/js.cookie.js"),
      tags$script(src = "js/charybdis.js")
    ),
    ### changing theme
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("md/about.md")
      ),
      tabItem(tabName = "cohorts",
              downloadButton("dlCohortInfo", "Download Data"),
              htmlOutput("borderCohortInfo"),
              dataTableOutput("cohortInfoTable")
      ),
      tabItem(tabName = "cohortCounts",
              htmltools::withTags(
                addInfo(
                  div(class="download-container",
                    shinyWidgets::dropdownButton(
                      inputId = "cohortCountsDownload",
                      label = "Download",
                      icon = icon("download"),
                      circle = F,
                      margin="20px",
                      downloadButton("dlCohortCountsByDb", "Download Table View"),
                      downloadButton("dlCohortCountsFlat", "Download Flat Data")
                    ),
                  ),
                "dlCohortCountsInfo"
              )),
              htmlOutput("borderCohortCounts"),
              dataTableOutput("cohortCountsTable")
      ),
      
      tabItem(tabName = "cohortCharacterization",
              htmlOutput("cohortName"),
              htmltools::withTags(
                addInfo(
                  div(class="download-container",
                      shinyWidgets::dropdownButton(
                        inputId = "characterizationDownload",
                        label = "Download",
                        icon = icon("download"),
                        circle = F,
                        margin="20px",
                        downloadButton("dlCharacterizationByDb", "Download Table View"),
                        downloadButton("dlCharacterizationFlat", "Download Flat Data")
                      ),
                  ),
                  "dlCharacterizationInfo"
                )),
              htmlOutput("borderCharacterization"),
              dataTableOutput("characterizationTable")
      ),
      
      tabItem(tabName = "timeToEvent",
              div(class="download-container",
                  downloadButton("dlTimeToEvent", "Download Data")),
              tags$h4(textOutput('survivalHeader')),
              tags$div(
                plotOutput("TimeToEventDeath", height = 650, width = 1050)
              )
              
      ),
      
      tabItem (tabName = "metricsDistribution",
               tags$h4(textOutput('metricsHeader')),
               # htmlOutput("borderCharacterization"),
               dataTableOutput("metricsTable")
               ),
      
      tabItem(tabName = "compareCohortCharacterization",
              htmlOutput("comparisonName"),
              downloadButton("dlCharCompare", "Download Data"),
              htmlOutput("borderCharCompare"),
              radioButtons("charCompareType", "", c("Table", "Plot"), selected = "Table", inline = TRUE),
              conditionalPanel(condition = "input.charCompareType=='Table'",
                               dataTableOutput("charCompareTable")
              ),
              conditionalPanel(condition = "input.charCompareType=='Plot'",
                               box(
                                 title = "Compare Cohort Characterization", width = NULL, status = "primary",
                                 htmlOutput("hoverInfoCharComparePlot"),
                                 plotOutput("charComparePlot", height = 700, hover = hoverOpts("plotHoverCharCompare", delay = 100, delayType = "debounce"))
                               )
              )
      ),
      tabItem(tabName = "databaseInformation",
              downloadButton("dlDatabaseInformation", "Download Data"),
              htmlOutput("borderDatabaseInformation"),
              dataTableOutput("databaseInformationTable")
      ),
      tabItem(tabName = "changeLog",
              includeHTML("changeLog.html"))
    )
  )
)