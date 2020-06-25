library(shiny)
library(aws.ec2metadata)


ui <- fluidPage(
  textInput("fileName", label="Enter file name:", value="Covid19CharacterizationCharybdis/fgz8jale_PreMerged.RData"),
  textInput("bucket", label="Bucket:", value=Sys.getenv("OHDSI_SHINY_DATA_BUCKET")),
  actionButton("exists", "File exists?"),
  htmlOutput("fileExists")
)
server <- function(input, output, session) {
  # Borrowed from devtools:
  # https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
  is_installed <- function(pkg, version = 0) {
    installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
    !is.na(installed_version) && installed_version >= version
  }
  
  fileName <- eventReactive(input$exists, {
    return(input$fileName)
  })
  
  bucket <- eventReactive(input$exists, {
    return(input$bucket)
  })
  
  output$fileExists <- renderText({
    msg <- paste0("<b>fileName:</b> ", fileName(), "<br/><b>bucket</b>:", bucket())

    # S3 access using file.exist will fail
    s3BucketUrl <- paste0("s3://", bucket(), "/", fileName())
    msg <- paste(msg, "<br/><b>S3 URL</b>:", s3BucketUrl)
    foundFile <- file.exists(s3BucketUrl)
    msg <- paste(msg, "<br/><b>File exists</b>:", foundFile)
    
    # # Curl attempt
    # curlUrl <- paste0("https://s3.amazonaws.com/", bucket(), "/", fileName())
    # msg <- paste(msg, "<br/><b>HTTPS URL</b>:", curlUrl)
    # curlFileFound <- tryCatch({
    #   con <- curl::curl(curlUrl)
    #   open(con)
    #   return(TRUE)
    # }, error = function(e) {
    #   return(FALSE)
    # })
    # msg <- paste(msg, "<br/><b>Curl file exists</b>:", curlFileFound)
    
    # Use aws.s3 library if installed
    if (is_installed("aws.s3")) {
      headObject <- aws.s3::head_object(fileName(), bucket = bucket())
      msg <- paste(msg, "<br/><b>Head Object</b>:", headObject)
      if (headObject) {
        aws.s3::s3load(fileName(), bucket = bucket())
        msg <- paste(msg, "<br/><b>File loaded!</b>")
      }
    } else {
      msg <- paste(msg, "<br/><b>aws.s3 not installed</b>")
    }
    return(HTML(msg))
  })
}
shinyApp(ui, server)