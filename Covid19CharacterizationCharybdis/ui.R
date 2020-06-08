library(shiny)
library(shinydashboard)
library(DT)

ohdsiBlueHex <- "#20425A"
ohdsiLightYellowHex <- "FBC209"

addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

dashboardPage(
  dashboardHeader(
    title = "CHARYBDIS",
    tags$li(div(img(src = 'logo.png',
                    title = "Characterizing Health Associated Risks, and Your Baseline Disease In SARS-COV-2", 
                    height = "40px", 
                    width = "40px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
    ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                if (exists("cohortCount")) addInfo(menuItem("Cohort Counts", tabName = "cohortCounts"), "cohortCountsInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Cohort Characterization", tabName = "cohortCharacterization"), "cohortCharacterizationInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Compare Cohort Char.", tabName = "compareCohortCharacterization"), "compareCohortCharacterizationInfo"),
                menuItem("Database information", tabName = "databaseInformation"), 
                conditionalPanel(condition = "input.tabs=='cohortCharacterization' | input.tabs=='compareCohortCharacterization'",
                   selectInput("database", "Database", database$databaseId, selectize = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='cohortCounts'",
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
                conditionalPanel(condition = "input.tabs!='cohortCounts' & input.tabs!='databaseInformation'",
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
      tags$link(rel="stylesheet", type="text/css", href="ohdsi.css")
    ),
    ### changing theme
    tabItems(
      tabItem(tabName = "cohortCounts",
              dataTableOutput("cohortCountsTable")
      ),
      tabItem(tabName = "cohortCharacterization",
              htmlOutput("cohortName"),
              dataTableOutput("characterizationTable")
      ),
      tabItem(tabName = "compareCohortCharacterization",
              htmlOutput("comparisonName"),
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
              # uiOutput("databaseInformationPanel")
              dataTableOutput("databaseInformationTable")
      )
    )
  )
)