library(shiny)
ui <- fluidPage(
  textInput("fileName", label="Enter file name:", value="Covid19CharacterizationCharybdis/hlimdxf1_PreMerged.RData"),
  textInput("bucket", label="Bucket:", value="ohdsi-shiny-data"),
  actionButton("exists", "File exists?"),
  htmlOutput("fileExists")
)
server <- function(input, output, session) {
  fileName <- eventReactive(input$exists, {
    return(input$fileName)
  })
  
  bucket <- eventReactive(input$exists, {
    return(input$bucket)
  })
  
  output$fileExists <- renderText({
    msg <- paste0("<b>fileName:</b> ", fileName(), "<br/><b>bucket</b>:", bucket())
    headObject <- aws.s3::head_object(fileName(), bucket = bucket())
    msg <- paste(msg, "<br/><b>Head Object</b>:", headObject)
    return(HTML(msg))
  })
}
shinyApp(ui, server)