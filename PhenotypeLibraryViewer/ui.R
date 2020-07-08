# OHDSI Gold Standard Phenotype Library Viewer

# UI Definition
shinyUI(
  fluidPage(
    
    # CSS for app style settings
    includeCSS("www/styles.css"),

    # Dashboard Page
    dashboardPage(
      skin = "yellow",

      # Dashboard Header
      dashboardHeader(title = "", titleWidth = 250),

      # Dashboard Sidebar
      dashboardSidebar(
        width = 250,
        sidebarMenu(
          id = "tabs",
          menuItem("Find", tabName = "find", icon = icon("search")),
          uiOutput("conditionalHR1"),
          lapply(1:10, function(x) {
            menuItemOutput(paste0("ChapterMenu_Tab", x))
          }),
          hr(),
          menuItem("About", tabName = "about", icon = icon("info-circle"))
        )
      ),

      # Dashboard Body
      dashboardBody(

        # Sidebar text size - Change to 18
        tags$head(
          tags$style(HTML(".main-sidebar { font-size: 18px; }"))
        ),

        # TODO: Consider making CSS file for styles (i.e. get rid of most "tags$" expressions)

        # Text at the top of the bar
        tags$head(tags$style(HTML(
          '.myClass {
          font-size: 20px;
          line-height: 50px;
          text-align: left;
          font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
          padding: 0 15px;
          overflow: hidden;
          color: white;
          }'
        ))),
        tags$script(HTML('
                         $(document).ready(function() {
                         $("header").find("nav").append(\'<span class="myClass"> OHDSI Gold Standard Phenotype Library </span>\');
                         })')),

        tags$h2("Under Development -- Do Not Use", style = "color:red", align = "center"),

        # Create Find, Phenotype(s) 1-5, and About tabs
        do.call(
          tabItems, c(

            # Find
            list(
              tabItem(
                tabName = "find",

                # TODO: Add refresh button that initializes everything back to the starting state
                # actionButton("refreshButton", "Refresh", icon = icon("refresh")),
                tags$hr(style = "border-color: black;"),

                # Book search bar
                fluidRow(column(
                  width = 12,
                  box(
                    title = "Select a Resource from the Library",
                    width = NULL, #TODO: Try 12 to see if the table can be expanded
                    status = "primary",
                    # uiOutput("filtered_phenotypes")
                    h4("Selected rows will appear in the sidebar, where they can be further explored."),
                    uiOutput("chapterFilters"),
                    #uiOutput("filtered_phenotypes"),
                    uiOutput("chapterSelect") # Make datatable of chapter selections

                    #h5('A library "Book" represents a desired health state.'),
                    # selectizeInput("book_search",
                    #   label = "",
                    #   choices = sort(unique(books)),
                    #   selected = NULL,
                    #   multiple = FALSE,
                    #   options = list(
                    #     onInitialize = I('function() { this.setValue(""); }')
                    #   )
                      # ,
                      # options = list(maxItems = 5)
                    )
                  )
                )
                ),

                # When a book is selected, display the book description (nested between lines)
                uiOutput("conditionalHR2"),
                uiOutput("bookDescription"),
                uiOutput("conditionalHR3")

                # Chapter selection
                # TODO: Bring this in as a gear for the table
                #uiOutput("chapterFilters") #, # Make filters to allow for custom data table build
                #uiOutput("chapterSelect") # Make datatable of chapter selections
              #)
            ), # End Find

            # Phenotype(s) 1-10
            lapply(1:10, function(x) {
              tabItem(
                tabName = paste0("tab", x),
                uiOutput(paste0("tab", x))
              )
            }),

            # About
            list(
              tabItem(
                tabName = "about",
                fluidRow(column(6, offset = 3, box(width = NULL, div(img(src = "OHDSI_Logo.png", align = "center"), style = "text-align: center;")))),
                fluidRow(column(12, box(width = NULL, includeMarkdown("about.md"))))
              )
            )
          ) # End c
        ) # End do.call
      ) # End dashboardBody
    ) # End dashboardPage
  ) # End fluidPage
) # End UI
