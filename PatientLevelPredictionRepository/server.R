library(shiny)
library(DT)

server <- function(input, output) {
  
  if (difftime(Sys.time(), repoTable$timeStamp[1], "hours") > 24) {
    repoTable <<- scrapeGithub()
  }
  
  selectedRow <- reactive({
    idx <- input$mainTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      row <- repoTable[idx, ]
      return(row)
    }
  })
  
  output$mainTable <- renderDT({
    table <- repoTable[, c( "targetName", "targetSize", "outcomeName", "outcomeSize", 'tar','database', 'auroc', 'auprc', 'ppv10', 'ppv50', 'calibrationGradient', 'calibrationIntercept')]
    
    truncScript <- "function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > %s ?\n
        '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;\n
     }"
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   columnDefs = list(list(
                     targets = c(0),
                     render = JS(sprintf(truncScript, 50, 50))
                   ),
                   list(
                     targets = 1:5,
                     render = JS(sprintf(truncScript, 30, 30))
                   )),
                   paging = TRUE)
    selection = list(mode = "single", target = "row")
    table <- datatable(table,
                       filter = "top",
                       options = options,
                       selection = selection,
                       rownames = FALSE,
                       colnames = c("Target Name","Target Size","Outcome Name",'Outcome Count', "Time-at-risk","Database", "AUROC",'AUPRC','PPV @10% Sens','PPV @50% Sens',"Calibration Gradient", "Calibration Intercept"),
                       escape = FALSE,
                       class = "stripe nowrap compact")
    return(table)
  })
  
  output$detailsUi <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(HTML("<p>Select a PatientLevelPrediction model to see further details and useful links</p>"))
    } else {
  
      formatDate <- function(date) {
        if (is.na(date)) {
          return("")
        } else {
          return(format(date, "%B %d, %Y"))
        }
      }
      
      convertHyperlinks <- function(markdown) {
        mdLinks <- regmatches(markdown, gregexpr("\\[[^\\[]*\\]\\(http[^\\(]*\\)", markdown))[[1]]
        html <- markdown
        if (length(mdLinks) > 0) {
          for (i in 1:length(mdLinks)) {
            url <- gsub(".*\\((.*)\\).*", "\\1", mdLinks[i])
            label <- gsub(".*\\[(.*)\\].*", "\\1", mdLinks[i])
            href <- sprintf("<a href=\"%s\">%s</a>", url, label)
            html <- sub(mdLinks[i], href, html, fixed = TRUE)
          }
        }    
        return(html)
      }
      markdown <- row$description
      
      convertFormatting <- function(markdown) {
        bold <- regmatches(markdown, gregexpr("\\*\\*[^\\*]*\\*\\*", markdown))[[1]]
        html <- markdown
        if (length(bold) > 0) {
          for (i in 1:length(bold)) {
            text <- gsub("\\*\\*([^\\*]*)\\*\\*", "\\1", bold[i])
            converted <- sprintf("<strong>%s</strong>", text)
            html <- sub(bold[i], converted, html, fixed = TRUE)
          }
        }    
        return(html)
      }
      lines <- list(sprintf("<h2>%s</h2>", row$title),
                    sprintf("<p>%s</p>", convertFormatting(convertHyperlinks(row$description))),
                    "<table>",
                    sprintf("<tr><td>Study Date</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$studyStartDate),
                    sprintf("<tr><td>Model Type</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$modelType),
                    sprintf("<tr><td>Target Criteria</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", convertHyperlinks(row$targetCriteria)),
                    
                    sprintf("<tr><td>Outcome Criteria</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", convertHyperlinks(row$outcomeCriteria)),
                    
                    sprintf("<tr><td>Covariates</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$covariates),
                    
                    sprintf("<tr><td>Vocabulary Used</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$vocabulary),
                    sprintf("<tr><td>Model</td><td>&nbsp;&nbsp;</td><td><a href=\"https://github.com/ohdsi-studies/%s\"><strong>ohdsi-studies/%s</strong></a></td></tr>", row$name, row$name),
                     sprintf("<tr><td>Protocol</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", convertHyperlinks(row$protocol)),
                    sprintf("<tr><td>Publications</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", convertHyperlinks(row$publications)),
                    sprintf("<tr><td>Resuls explorer</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", convertHyperlinks(row$resultsExplorer)),
                    "</table>")
      return(HTML(paste(lines, collapse = "\n")))
      
    }
  })
  
  output$lastUpdated <- renderUI({
    return(HTML(paste("<p>&nbsp;</p><p>Last updated:", 
                      as.character(repoTable$timeStamp[1]), 
                      " (Updated every 24 hours)</p>")))
  })
}
