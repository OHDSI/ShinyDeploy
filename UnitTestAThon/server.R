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
    return(tibble(name = name,
                  branch = branch,
                  coverage = as.numeric(data$commit$totals$c),
                  lines = data$commit$totals$h))
  }
  
  getCoverageAllPackages <- reactive({
    result <- lapply(hadesPackages$name, getCoverage) %>%
      bind_rows()
    # saveRDS(result, "data/baseLine.rds")
    return(result)
  })
  
  output$mainTable <- renderDataTable({
    result <- getCoverageAllPackages()
    repoLinkTemplate <- "<a href = https://github.com/OHDSI/%s>%s</a>"
    detailsLinkTemplate <- "<a href = https://codecov.io/github/OHDSI/%s/branch/%s>Details</a>"
    result <- baseline %>%
      select(.data$name, startCoverage = .data$coverage, startLines = .data$lines) %>%
      inner_join(result, by = "name") %>%
      mutate(linkedName = sprintf(repoLinkTemplate, .data$name, .data$name),
             improvement = .data$coverage - .data$startCoverage,
             improvementLines = .data$lines - .data$startLines,
             link = sprintf(detailsLinkTemplate, .data$name, .data$branch)) %>%
      arrange(.data$name) %>%
      select(.data$linkedName, 
             .data$startCoverage, 
             .data$coverage, 
             .data$improvement, 
             .data$startLines, 
             .data$lines, 
             .data$improvementLines, 
             .data$link)
    
    options = list(pageLength = 10000, 
                   searching = FALSE, 
                   lengthChange = FALSE,
                   ordering = TRUE,
                   paging = FALSE,
                   columnDefs = list(percentDef(1:3)))
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, "Package"),
          th(colspan = 3, "Coverage (percent)"),
          th(colspan = 3, "Coverage (lines)"),
          th(rowspan = 2, "Report")
        ),
        tr(
          lapply(rep(c("Start", "Current", "Improvement"), 2), th)
        )
      )
    ))
    table <- datatable(result, 
                       options = options,
                       escape = FALSE,
                       rownames = FALSE,
                       container = sketch,
                       selection = "none",
                       class = "stripe nowrap compact") %>%
      formatStyle( c("startCoverage", "coverage"),
                   backgroundColor = styleInterval(80, c(NA, "lightgreen"))) %>%
      formatStyle(c("improvement", "improvementLines"),
                  background = styleColorBar(c(0, max(result$improvement)), "lightgreen"),
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
    return(table)
  })
})