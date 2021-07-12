library(shiny)
library(DT)
library(dplyr)

percentDef <- function(columns) {
  list(
    targets = columns,
    render = DT::JS(
      "function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    return (data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
  }"
    )
  )
}

shinyServer(function(input, output, session) {

  getCoverage <- function(name) {
    urlTemplate <- "https://codecov.io/api/gh/OHDSI/%s/branch/%s"
    if (name == "PatientLevelPrediction") {
      branch <- "development"
    } else {
      branch <- "develop"
    }
    url <- sprintf(urlTemplate, name, branch)
    data <- readLines(url, warn = FALSE)
    data <- RJSONIO::fromJSON(data)
    return(as.numeric(data$commit$totals$c))
  }
  
  getCoverageAllPackages <- reactive({
    result <- tibble(name = hadesPackages$name,
                     coverage = sapply(hadesPackages$name, getCoverage)) %>% 
      mutate(branch = if_else(.data$name == "PatientLevelPrediction", "development", "develop"))
    # saveRDS(result, "data/baseLine.rds")
    return(result)
  })
  
  output$mainTable <- renderDataTable({
    result <- getCoverageAllPackages()
    linkTemplate <- "<a href = https://codecov.io/github/OHDSI/%s?branch=%s>Details</a>"
    result <- baseline %>%
      select(.data$name, startCoverage = .data$coverage) %>%
      inner_join(result, by = "name") %>%
      mutate(improvement = .data$coverage - .data$startCoverage,
             link = sprintf(linkTemplate, .data$name, .data$branch)) %>%
      select(-.data$branch)
    options = list(pageLength = 10000, 
                   searching = FALSE, 
                   lengthChange = FALSE,
                   ordering = FALSE,
                   paging = FALSE,
                   columnDefs = list(percentDef(1:3)))
    table <- datatable(result, 
                       options = options,
                       escape = FALSE,
                       rownames = FALSE,
                       colnames = c("Package", "Start coverage", "Current coverage", "Improvement", "Link"),
                       class = "stripe nowrap compact") %>%
      formatStyle( c("startCoverage", "coverage"),
                   backgroundColor = styleInterval(80, c(NA, "lightgreen"))) %>%
      formatStyle("improvement",
                  background = styleColorBar(c(0, max(result$improvement)), "lightgreen"),
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
    return(table)
  })
})