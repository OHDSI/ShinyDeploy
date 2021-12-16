library(shinydashboard)
library(dplyr)

server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- packageDescriptions
    if (input$packageId != "All") {
      data <- data[data$packageId == input$packageId,]
    }
    # if (input$cyl != "All") {
    #   data <- data[data$textField == input$textField,]
    # }
    data %>% select(-anotherField)
  },
  selection = "single"))

  output$note <- renderPrint({
    s <- input$table_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, packageDescriptions$anotherField[s], sep = ', ')
    }
  })
}

shinyServer(server)
