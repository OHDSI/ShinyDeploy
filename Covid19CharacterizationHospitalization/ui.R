library(shiny)
library(shinydashboard)
library(DT)

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
      title = "OHDSI COVID and Influenza Hospitalization Characterization",
      titleWidth = 450,
      tags$li(div(img(src = 'logo.png',
                      title = "OHDSI COVID Characterization", height = "40px", width = "40px"),
                  style = "padding-top:0px; padding-bottom:0px;"),
              class = "dropdown")    
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                addInfo(menuItem("About", tabName = "about"), "aboutInfo"),
                addInfo(menuItem("Databases", tabName = "databases"), "databaseInfo"),
                if (exists("cohortCount")) addInfo(menuItem("Cohort Counts", tabName = "cohortCounts"), "cohortCountsInfo"),
                #if (exists("incidenceRate")) addInfo(menuItem("Incidence Rate", tabName = "incidenceRate"), "incidenceRateInfo"),
                #if (exists("timeDistribution")) addInfo(menuItem("Time Distributions", tabName = "timeDistribution"), "timeDistributionInfo"),
                #if (exists("includedSourceConcept")) addInfo(menuItem("Included (Source) Concepts", tabName = "includedConcepts"), "includedConceptsInfo"),
                #if (exists("orphanConcept")) addInfo(menuItem("Orphan (Source) Concepts", tabName = "orphanConcepts"), "orphanConceptsInfo"),
                #if (exists("inclusionRuleStats")) addInfo(menuItem("Inclusion Rule Statistics", tabName = "inclusionRuleStats"), "inclusionRuleStatsInfo"),
                #if (exists("indexEventBreakdown")) addInfo(menuItem("Index Event Breakdown", tabName = "indexEventBreakdown"), "indexEventBreakdownInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Cohort Characterization", tabName = "cohortCharacterization"), "cohortCharacterizationInfo"),
                #if (exists("cohortOverlap")) addInfo(menuItem("Cohort Overlap", tabName = "cohortOverlap"), "cohortOverlapInfo"),
                if (exists("covariateValue")) addInfo(menuItem("Compare Cohort Char.", tabName = "compareCohortCharacterization"), "compareCohortCharacterizationInfo"),
                conditionalPanel(condition = "input.tabs!='incidenceRate' & input.tabs!='timeDistribution' & input.tabs!='cohortCharacterization' & input.tabs!='cohortCounts' & input.tabs!='indexEventBreakdown'",
                                 selectInput("database", "Database", database$databaseId, selectize = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='incidenceRate' | input.tabs=='timeDistribution' | input.tabs=='cohortCharacterization' | input.tabs=='cohortCounts' | input.tabs=='indexEventBreakdown'",
                                 checkboxGroupInput("databases", "Database", database$databaseId, selected = database$databaseId)
                ),
                conditionalPanel(condition = "input.tabs!='cohortCounts'",
                                 selectInput("cohort", "Cohort (Target)", choices = cohort$cohortFullName, selectize = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='includedConcepts' | input.tabs=='orphanConcepts'",
                                 selectInput("conceptSet", "Concept Set", c(""), selectize = FALSE)
                ),
                conditionalPanel(condition = "input.tabs=='cohortOverlap' | input.tabs=='compareCohortCharacterization'",
                                 selectInput("comparator", "Comparator", cohort$cohortFullName, selectize = FALSE, selected = cohort$cohortFullName[min(2, nrow(cohort))])
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about",
        br(),
        includeHTML("./html/about.html")
      ),
      tabItem(
        tabName = "databases",
        includeHTML("./html/databasesInfo.html")
      ),
      tabItem(tabName = "cohortCounts",
              dataTableOutput("cohortCountsTable")
      ),
      tabItem(tabName = "incidenceRate",
              box(
                title = "Incidence Rate", width = NULL, status = "primary",
                tags$table(style = "width: 100%",
                           tags$tr(
                  tags$td(valign = "bottom",
                          checkboxGroupInput(inputId = "irStratification", 
                                             label = "Stratify by",
                                             choices = c("Age", "Gender", "Calendar Year"),
                                             selected = c("Age", "Gender", "Calendar Year"),
                                             inline = TRUE)
                  ),
                  tags$td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
                  tags$td(valign = "bottom", style = "text-align: right",
                          checkboxInput("irYscaleFixed", "Use same y-scale across databases")
                  )
                )),
                htmlOutput("hoverInfoIr"),
                plotOutput("incidenceRatePlot", height = 700, hover = hoverOpts("plotHoverIr", delay = 100, delayType = "debounce"))
              )
      ),
      tabItem(tabName = "timeDistribution",
              box(
                title = "Time Distributions", width = NULL, status = "primary",
                plotOutput("timeDisPlot")
              ),
              box(
                title = "Time Distributions Table", width = NULL, status = "primary",
                dataTableOutput("timeDistTable")
              )
      ),
      tabItem(tabName = "includedConcepts",
              radioButtons("includedType", "", c("Source Concepts", "Standard Concepts"), selected = "Source Concepts", inline = TRUE),
              dataTableOutput("includedConceptsTable")
      ),
      tabItem(tabName = "orphanConcepts",
              dataTableOutput("orphanConceptsTable")
      ),
      tabItem(tabName = "inclusionRuleStats",
              dataTableOutput("inclusionRuleTable")
      ),
      tabItem(tabName = "indexEventBreakdown",
              dataTableOutput("breakdownTable")
      ),
      tabItem(tabName = "cohortCharacterization",
              tags$table(style = "width: 100%",
                         tags$tr(
                           tags$td(valign = "bottom",
                              radioButtons("charType", "", c("Pretty", "Raw"), selected = "Pretty", inline = TRUE),
                           ),
                           tags$td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
                              tags$td(valign = "bottom", style = "text-align: right",
                                  conditionalPanel(condition = "input.charType=='Raw'", 
                                      radioButtons("rawCharSubType", "", c("Binary", "Continuous"), selected = "Binary", inline = TRUE)
                                  ),
                           )
                         )),
              dataTableOutput("characterizationTable")
      ),
      tabItem(tabName = "cohortOverlap",
              box(
                title = "Cohort Overlap (Subjects)", width = NULL, status = "primary",
                plotOutput("overlapPlot")
              ),
              box(
                title = "Cohort Overlap Statistics", width = NULL, status = "primary",
                dataTableOutput("overlapTable")
              )
      ),
      tabItem(tabName = "compareCohortCharacterization",
              tags$table(style = "width: 100%",
                         tags$tr(
                           tags$td(valign = "bottom",
                                   radioButtons("charCompareType", "", c("Pretty table", "Raw table", "Plot"), selected = "Pretty table", inline = TRUE),
                           ),
                           tags$td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
                           tags$td(valign = "bottom", style = "text-align: right",
                                   conditionalPanel(condition = "input.charCompareType=='Raw table'", 
                                                    radioButtons("rawCharCompareSubType", "", c("Binary", "Continuous"), selected = "Binary", inline = TRUE)
                                   ),
                           )
                         )),
              conditionalPanel(condition = "input.charCompareType=='Pretty table' | input.charCompareType=='Raw table'",
                               dataTableOutput("charCompareTable")
              ),
              conditionalPanel(condition = "input.charCompareType=='Plot'",
                               box(
                                 title = "Compare Cohort Characterization", width = NULL, status = "primary",
                                 htmlOutput("hoverInfoCharComparePlot"),
                                 plotOutput("charComparePlot", height = 700, hover = hoverOpts("plotHoverCharCompare", delay = 100, delayType = "debounce"))
                               )
              )
      )
    )
  )
)