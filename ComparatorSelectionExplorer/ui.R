library(shiny)

menu <- shinydashboard::sidebarMenu(
  shinydashboard::menuItem(text = "Recommend Comparators",
                           tabName = "comparators",
                           icon = shiny::icon("table")
  ),
  shinydashboard::menuItem(text = "About",
                           tabName = "about",
                           icon = shiny::icon("table")
  )
)

bodyTabs <- shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "about",
    shiny::fluidPage(
      shinydashboard::box(
        width = 12,
        h3("Description"),
        htmlTemplate("about.html"),
        h3("Currently Available Data Sources"),
        shinycssloaders::withSpinner(
          reactable::reactableOutput("dataSources")
        ),
        h3("License"),
        htmlTemplate("license.html"),
      )
    )
  ),
  shinydashboard::tabItem(
    tabName = "exposureInfo",
    shiny::div()
  ),
  shinydashboard::tabItem(
    tabName = "comparators",
    shiny::fluidPage(
      shinydashboard::box(
        title = "Target Selection Settings",
        width = 12,
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              inputId = "selectedExposure",
              choices = NULL,
              width = "100%",
              label = "Select target exposure:"),
            selectInput(
              inputId = "selectedComparatorTypes",
              label = "Select comparator types:",
              width = "100%",
              choices = c("RxNorm Ingredients", "ATC Classes"),
              selected = "RxNorm Ingredients",
              multiple = TRUE
            ),
            selectInput(
              inputId = "selectedDatabases",
              label = "Select data sources:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            )
          ),
          column(
            width = 6,
            sliderInput(
              inputId = "minNumDatabases",
              label = "Minimum data sources with comparator presence:",
              min = 1,
              max = 10,
              value = 2,
              step = 1,
              ticks = FALSE
            ),
            radioButtons(
              inputId = "avgOn",
              label = "Rank comparators on:",
              choices = c("Average similarity score", "Average source-specific rank"),
              selected = "Average similarity score"
            ),
          )
        ),

        fluidRow(
          column(
            width = 3,
            shiny::actionButton(inputId = "getResults", "Suggest Comparators")
          ),
          column(
            width = 9,
            conditionalPanel(
              "input.selectedExposure != ''",
              shiny::actionButton(inputId = "showRankings", "Show rank plot"),
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.getResults > 0",
        shinydashboard::box(
          width = 12,
          title = "Comparator listing",
          shinycssloaders::withSpinner(reactable::reactableOutput("multiDatabaseSimTable"))
        )
      )
    )
  )
)

shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Comparator Selection Explorer"),
  shinydashboard::dashboardSidebar(menu, collapsed = TRUE),
  shinydashboard::dashboardBody(
    bodyTabs
  ),
  title = "Comparator Selection Explorer",
  skin = "black"
)

