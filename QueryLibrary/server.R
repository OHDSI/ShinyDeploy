
server <- shinyServer(
  function(input, output, session) {
    options(shiny.sanitize.errors = TRUE)
  
    query <- reactiveValues()
  
    query$saved <- FALSE
  
    query$name <- reactive({toString(queriesDf$Query[input$queriesTable_rows_selected])})
  
    query$inputFileName <- reactive({mdFiles[input$queriesTable_rows_selected]})
  
    query$sqlSource <- reactive({getSqlFromMarkdown(query$inputFileName())})
  
    query$parameters <- reactive({getParameters(query$sqlSource())})
  
    query$sqlTarget <- reactive(
      {
        parameterValues <- list()
        for (param in query$parameters()) {
          value <- input[[param]]
          if (!is.null(value)) {
            parameterValues[[param]] <- value
          }
        }
        sql <- do.call("renderSql", append(query$sqlSource(), parameterValues))$sql
        warningString <- c()
        handleWarning <- function(e) {
          output$warnings <- e$message
        }
        if (allow_execute) {
          oracleTempSchema <- input$oracleTempSchema
          if (oracleTempSchema == "")
            oracleTempSchema <- NULL
          sql <- withCallingHandlers(
            suppressWarnings(
              translateSql(
                sql,
                targetDialect = tolower(input$dialect),
                oracleTempSchema = oracleTempSchema
              )$sql
            ),
            warning = handleWarning
          )
        }
        if (!is.null(warningString))
          output$warnings <- warningString
        return(sql)
      }
    )
  
    renderedFilename <- reactive(
      {
        if (length(query$inputFileName())>0) {
          createRenderedHtml(query$inputFileName(), query$sqlTarget())
        } else
          return(NULL)
      }
    )
  
    output$html <- renderText(
      {
        if (!is.null(renderedFilename())){
          includeHTML(renderedFilename())      
        } else
          return("select a query")
      }
    )
  
    output$parameterInputs <- renderUI(
      {
        params <- query$parameters()
        sql <- query$sqlSource()
    
        createRow <- function(param, sql) {
          # Get current values if already exists:
          value <- isolate(input[[param]])
      
          if (is.null(value)) {
            # Get default values:
            value <- regmatches(sql, regexpr(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=[^}]+}"), sql))
            if (length(value) == 1) {
              value = sub(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=\\s*"), "", sub("\\}$", "", value)) 
            } else {
              value = ""
            }
          }
          textInput(param, param, value = value)
        }
        lapply(params, createRow, sql = sql)
      }
    )
  
    ### TABLES
  
    output$queriesTable <- renderDT(
      {
        table =  queriesDf
        return(table)
      },
      server = FALSE,

      filter = list(position = 'top'),
      extensions = 'Buttons',
      rowname = FALSE,
      selection = 'single',
      options = list(
        clear = FALSE,
        columnDefs = list(
          list(
            visible=FALSE, 
            targets=c(2,3)
          ), 
          list(
            width = '200px',
            targets=c(0)
          ),
          list(
            width = '80px',
            targets=c(2)
          )
        ),
        autoWidth = FALSE,
        lengthMenu = c(10, 50, 75, 100),
        searchHighlight = TRUE,
        dom = 'Blfrtip',
        buttons = I('colvis'),
        processing=FALSE
      )
    )
  
    queriesProxy = dataTableProxy('queriesTable', session = session)
  
    # obsolete (does not work to have two DTtables in a the app?, to be fixed)
    output$resultsTable <- renderDataTable(
      query$data,
      server = FALSE,
      caption = "Table 2: Query results",
      filter = list(position = 'top'),
      extensions = 'Buttons',
      rowname = FALSE,
      selection = 'single',
      options = list(
        autoWidth = FALSE,
        lengthMenu = c(25, 50, 75, 100),
        searchHighlight = TRUE,
        dom = 'Blfrtip',
        buttons = I('colvis'),
        processing=FALSE
      )
    )
  
    output$resultsTable <- renderTable({query$data})
  
    # Load the app configuration settings
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileChoose(input, "loadConfig", roots = volumes, session = session)
  
    observe(
      {
        if (length(parseFilePaths(roots=volumes,input$loadConfig)$datapath)>0) {
          configFilename = parseFilePaths(roots=volumes, input$loadConfig)$datapath
          if(!file.exists(configFilename)) {return(NULL)}
      
          savedInputs <- readRDS(configFilename)
      
          inputIDs      <- names(savedInputs) 
          inputvalues   <- unlist(savedInputs) 
          for (i in 1:length(savedInputs)) { 
            session$sendInputMessage(inputIDs[i],  list(value=inputvalues[[i]]) )
          }
      
        } 
      }
    )

    # save the app configuration settings

    shinyFileSave(input, "saveConfig", roots = volumes, session = session, restrictions = system.file(package = "base"))
  
    observe(
      {
        if (length(parseSavePath(roots=volumes,input$saveConfig)$datapath)>0) {
          configFilename = parseSavePath(roots=volumes,input$saveConfig)$datapath
          saveRDS(
            isolate(reactiveValuesToList(input))[c("dialect","server","user","password","port","cdm","vocab","oracleTempSchema","extraSettings")], 
            file = configFilename
          )
        } 
      }
    )
  
  
    # select the custom query folder
    shinyDirChoose(input, "selectUserFolder", roots = volumes, session = session)

    #observe({
    #  updateTextAreaInput(session,"userFolder", value =  parseDirPath(volumes, input$selectUserFolder))
    #})
  
    # observe(
    #   {
    #     queriesDf <<- loadQueriesTable(queryFolder,input$userFolder)
    #     mdFilesPackage <-  list.files(queryFolder, recursive = TRUE, pattern='*.md')
    #     mdFilesPackage <- paste(queryFolder,mdFilesPackage, sep="/")
    #     mdFilesUser <- list.files(input$userFolder, recursive = TRUE, pattern='*.md')
    #     mdFilesUser <- paste(input$userFolder,mdFilesUser, sep="/")
    #     mdFiles <<- c(mdFilesPackage,mdFilesUser)
    #   }
    # )

    
    ### BUTTONS
  
    observeEvent(
      input$importButton,
      {
        if (!is.null(isolate(input$queriesTable_rows_selected))) {
          updateTextAreaInput(session, "target", value = query$sqlTarget())     
        } else
          showNotification("First select a query to import",type = "message", duration = 2)
      }
    )
  
    observeEvent(
      input$executeButton, 
      {
        connectionDetails <- createConnectionDetails(
          dbms = tolower(input$dialect),
          user = input$user,
          password = input$password,
          server = input$server,
          port = input$port,
          extraSettings = input$extraSettings
        )
        con <-DatabaseConnector::connect(connectionDetails)
    
        sql <- input$target
        results<-NULL
        tryCatch(
          {
            results <- DatabaseConnector::querySql(con,sql)
          },
          error=function(cond) {
            showNotification(as.character(cond$message),type = "error")
          }
        )
        disconnect(con)
        #return(as.data.frame(results))
        # Fill in the reactiveValues to tricker table update
        query$sql <- sql
        query$data <- results
      },
      ignoreNULL = TRUE
    )
  
    
    observe(
      {
        toggleState("executeButton", !is.null(input$target) && input$target != "")
      }
    )
 
    observe(
      {
        toggleState("copyClipboardButton", !is.null(input$target) && input$target != "")
      }
    ) 
  
    observe(
      {
        toggleState("downloadData", !is.null(input$target) && input$target != "")
      }
    )

    observe(
      {
        toggleState("save", !is.null(input$target) && input$target != "")
      }
    )
  
    ## OTHER
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('query-', Sys.Date(), '.sql', sep='')
      },
      content = function(file) {
        write.csv(
          query$sqlTarget(),
          file
        )
        #SqlRender::writeSql(sql =  "Select * from table", targetFile = con)
      }
    )
  
  

    output$connected <- eventReactive(
      input$testButton,
      {
        connectionDetails <- createConnectionDetails(
          dbms = tolower(input$dialect),
          user = input$user,
          password = input$password,
          server = input$server,
          port = input$port,
          extraSettings = input$extraSettings
        )
        con <-DatabaseConnector::connect(connectionDetails)
        if (length(con)>0) {
          disconnect(con)
          return ("Connection Successful")
        } else
          return ("Not Connected")
    
      },
      ignoreNULL = TRUE
    )
    
    # Managing visibility of interface parts
    observe(
      {
        if (allow_execute == FALSE) {
          hide(selector = "#SelectExecuteTabs li a[data-value=Execute]")
        }
      }
    )
    
    output$allowexecute <- reactive(
      {
        allow_execute
      }
    )
    
    outputOptions(output, "allowexecute", suspendWhenHidden = FALSE)
  }
)
