#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 150 * 1024^2)

library(shiny)
library(OhdsiSharing)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny OHDSI Sharing"),

    column(width = 4,
           fileInput("uploadZip", "OHDSI Network Results *.zip", accept = ".zip"),
           fileInput("uploadPpk", "Private key file", accept = ".ppk")),
    column(width = 4,
           br(),
           actionButton("process", "Process uploaded data")),
    tableOutput("files")



)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$files <- renderTable(rbind(input$uploadZip,input$uploadPpk))

    observeEvent(input$process, {

        req(input$uploadZip)
        ext <- tools::file_ext(input$uploadZip$name)
        if (ext != "zip") {
            validate("Invalid file; Please upload a .zip file")
        }

        req(input$uploadPpk)

        file <- input$uploadZip
        cat("hello")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
