
ui <- dashboardPage(title = "QueryLibrary",
  
  dashboardHeader(title = div(img(src="logo.png", height = 50, width = 50), "QueryLibrary")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "select", icon = icon("home")),
      menuItem("Configuration", tabName = "configuration", icon = icon("cog")),
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Feedback", icon = icon("comment"), href = "https://github.com/OHDSI/QueryLibrary/issues")
    ),
    tags$footer(
      align = "right",
      style = "
      position:absolute;
      bottom:0;
      width:100%;
      height:225px;
      color: black;
      padding: 10px;
      text-align:center;
      background-color: #eee;
      z-index: 1000;",
      HTML(
        "<a href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache 2.0</a>
        <div style=\"margin-bottom:10px;\">open source software</div>
        <div>provided by</div>
        <div><a href=\"http://www.ohdsi.org\"><img src=\"ohdsi_color.png\" height=42 width = 100></a></div>
        <div>and</div>
        <div><a href=\"http://www.ehden.eu\"><img src=\"ehden_logo.png\" height=42 width = 100></a></div>
        <div><a href=\"http://www.ohdsi.org\">join the journey</a></div>"
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             color:red;
             position:fixed;
             top: calc(12%);;
             left: calc(50%);;
             width: calc(45%);;
             }
             "
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "select",
        tabsetPanel(
          id = "SelectExecuteTabs",
          tabPanel("Select",
            h2("Select a query"),
            fluidRow(
              column(
                label = 'selectedQuery',
                width = 6,
                offset = 0,
                #actionButton("clearFiltersButton","Clear All Filters"),
                DTOutput("queriesTable")
              ),
              column(
                width = 6,
                box(
                  title = "Query Description",
                  width = NULL,
                  status = "primary",
                  uiOutput(outputId = "html")
                )
              )
            )
          ),
          
          tabPanel(
            "Execute",
            box(
              title = "Execute",
              width = NULL,
              height = '80%',
              actionButton("importButton", "Import selected query", icon = icon("home")),
              textAreaInput("target", NULL, ""),
              actionButton("executeButton", "Run", icon = icon("play")),
              buttonCopyTextAreaToClipboard("copyClipboardButton","target","Copy query to clipboard"),
              buttonDownloadTextArea("save","target","Save query to file")
            ),
            
            ### show timer
            conditionalPanel(
              "updateBusy() || $('html').hasClass('shiny-busy')",
              id='progressIndicator',
              "Running",
              div(id='progress',includeHTML("timer.js"))
            ),
            
            tags$head(
              tags$style(
                type="text/css",
                '#progressIndicator {',
                # '  position: fixed; top: 120px; right: 80px; width: 170px; height: 60px;',
                '  padding: 8px; border: 1px solid CCC; border-radius: 8px; color:green',
                '}'
              )
            ),
            
            box(
              title = "Results",
              width = NULL,
              height = '80%',
              tableOutput("resultsTable")
              #downloadButton('downloadData', 'Download Results')
            )
          )
        )
    
      ),
      
      tabItem(
        tabName = "configuration",
        h2("Configuration"),
        conditionalPanel(
          condition = "output.allowexecute",
          shinyFilesButton("loadConfig", "Load", "Select Configuration file", multiple = FALSE),
          shinySaveButton("saveConfig", "Save", "Save file as...", filename = configFilename, filetype = list(settings = "Rds"))
        ),
        # fluidRow(
        #   offset = 10,
        #   column(
        #     width = 6,
        #     box(
        #       background = "light-blue",
        #     
        #       width = NULL,
        #       h4("user queries folder"),
        #       textInput("userFolder", NULL),
        #       shinyDirButton("selectUserFolder", "Select", "Select folder containing user-defined query files")
        #     )
        #   )
        # ),
        
        fluidRow(
          offset = 5,
          column(
            width = 6,
            box(
              background = "light-blue",
                
              width = NULL,
              h4("target dialect"),
              selectInput(
                "dialect",
                NULL,
                choices = c(
                  "BigQuery",
                  "Impala",
                  "Netezza",
                  "Oracle",
                  "PDW",
                  "PostgreSQL",
                  "RedShift",
                  "SQL Server"
                ),
                selected = "SQL Server"
              ),
              
              conditionalPanel(
                condition = "output.allowexecute",
                h4("server"),
                textInput("server", NULL)
                ,
                h4("username"),
                textInput("user", NULL),
                
                h4("password"),
                passwordInput("password", NULL)
                ,
                h4("port"),
                textInput("port", NULL, value = 1521)
              ),
                
              h4("cdm schema"),
              textInput("cdm", NULL, value = "cdm"),
                
              h4("vocabulary schema"),
              textInput("vocab", NULL, value = "cdm"),
              
              conditionalPanel(
                condition = "output.allowexecute",
                h4("Oracle temp schema"),
                textInput("oracleTempSchema", NULL),
                
                h4("extra setting"),
                textInput("extraSettings", NULL),
                
                actionButton("testButton","Test Connection")
              )
                
            ),
            
            conditionalPanel(
              condition = "output.allowexecute",
              textOutput("connected"),
              textOutput("warnings")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              status = "primary",
              uiOutput(outputId = "about")
            )
          )
        )
      )
    )
  )
)
