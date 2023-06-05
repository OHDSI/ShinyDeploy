#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 750 * 1024^2)

library(shiny)
library(OhdsiSharing)

defaultWidth <- "800px"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny OHDSI Sharing"),
    column(width = 8,
           textInput("userName", "Upload user name (provided by study coordinator)", value = "study-data-site-",
                     width = defaultWidth),
           textInput("folderName", "Upload folder name (provided by study coordinator)", value = ".",
                     width = defaultWidth),
           fileInput("uploadZip", "OHDSI Network Results *.zip", accept = ".zip",
                     width = defaultWidth),
           fileInput("uploadPpk", "Private key file", accept = ".ppk",
                     width = defaultWidth)),
    column(width = 2,
           br(),
           actionButton("process", "Process uploaded data")),
    tableOutput("files")



)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$files <- renderTable(rbind(input$uploadZip,input$uploadPpk))

    observeEvent(input$process, {

        req(input$uploadZip)
        req(input$uploadPpk)

        cat("start\n")

        # if (ext != "zip") {
        #     validate(need("Invalid file; Please upload a .zip file")
        # }

        getFileName <- reactive({
            ext <- tools::file_ext(input$uploadZip$name)
            validate(need(ext == "zip", "Invalid file; Please upload a .zip file"))
            input$uploadZip$datapath
        })
        fileName <- getFileName()

        remoteFolder <- input$folderName
        privateKeyFileName <- input$uploadPpk$datapath
        userName <- input$userName

        notify <- function(msg, id = NULL) {
            showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
        }

        notify("Connecting to SFTP server")
        connection <- OhdsiSharing::sftpConnect(privateKeyFileName, userName)
        on.exit(sftpDisconnect(connection))
        notify("Connected to SFTP server")

        remoteFileName <- input$uploadZip$name
        remoteFileName <- paste(paste(sample(c(letters, 0:9), 8),
                                      collapse = ""), remoteFileName, sep = "_")
        if (remoteFolder != ".") {
            remoteFileName <- paste(remoteFolder, remoteFileName, sep = "/")
        }

        # ParallelLogger::logInfo("Uploading ", fileName, " to ", remoteFileName, " on OHDSI SFTP server")
        notify("Uploading file")
        sftpPutFile(connection, fileName, remoteFileName)
        notify("Done")

        # clear out entries
    })
}

# Run the application
shinyApp(ui = ui, server = server)
