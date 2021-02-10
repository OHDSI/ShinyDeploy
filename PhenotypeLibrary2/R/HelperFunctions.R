cohortReference <- function(outputId) {
  shinydashboard::box(# title = "Reference",
    status = "warning",
    width = "100%",
    shiny::uiOutput(outputId = outputId))
}

standardDataTable <- function(data, 
                              selectionMode = "single",
                              selected = c(1),
                              searching = TRUE) {
  
  dataTableFilter =
    list(position = 'top',
         clear = TRUE,
         plain = FALSE)
  
  dataTableOption =
    list(
      pageLength = 10,
      lengthMenu = list(c(5, 10, 20, -1), c("5", "10", "20", "All")),
      lengthChange = TRUE,
      searching = searching,
      ordering = TRUE,
      scrollX = TRUE,
      ordering = TRUE,
      paging = TRUE,
      info = TRUE,
      searchHighlight = TRUE,
      # search = list(regex = TRUE, caseInsensitive = FALSE),
      stateSave = TRUE,
      dom = 'lBfrtip',
      # B for buttons
      buttons = list(
        'copy',
        list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        ),
        'print',
        'colvis'
      ),
      colReorder = TRUE,
      realtime = FALSE,
      # for col reorder
      # fixedColumns = list(leftColumns = 1),
      # fixedHeader = TRUE,
      # processing = TRUE,
      autoWidth = TRUE
    )
  listOfVariablesThatAreAlwaysFactors <- c('domainId',
                                           'conceptClassId',
                                           'vocabularyId',
                                           'standardConcept',
                                           'conceptSetName',
                                           'conceptName',
                                           'cohortId',
                                           'cohortName',
                                           'phenotypeId',
                                           'phenotypeName',
                                           'analysisName',
                                           'startDay',
                                           'endDay',
                                           'analysisId',
                                           'temporalChoices',
                                           'covariateName',
                                           'conceptId'
                                           )
  
  convertVariableToFactor <- function(data, variables) {
    for (i in (1:length(variables))) {
      variable <- variables[i]
      if (variable %in% colnames(data)) {    
        data[[variable]] <- as.factor(data[[variable]]) %>%
          dplyr::tibble()
      }
    }
    return(data)
  }
  data <- convertVariableToFactor(data = data, 
                                  variables = listOfVariablesThatAreAlwaysFactors)
  
  dataTable <- DT::datatable(
    data = data,
    class = "stripe compact order-column hover",
    rownames = FALSE,
    options = dataTableOption,
    colnames = colnames(data) %>% camelCaseToTitleCase(),
    filter = dataTableFilter,
    # style = 'bootstrap4',
    escape = FALSE,
    selection = list(mode = selectionMode, target = "row", selected = selected),
    editable = FALSE,
    # container = sketch,
    extensions = c('Buttons', 'ColReorder', 'FixedColumns', 'FixedHeader'),
    plugins = c('natural') #'ellipsis'
    # escape = FALSE
  )
  
  colNames <- colnames(data)
  listRounds <-
    colNames[stringr::str_detect(string = tolower(colNames),
                                 pattern = 'entries|subjects|count|min|max|p10|p25|median|p75|p90|max|before|onvisitstart|after|duringvisit')]
  listDecimal <-
    colNames[stringr::str_detect(string = tolower(colNames),
                                 pattern = 'average|standarddeviation|mean|sd|personyears|incidencerate')]
  listPercent <-
    colNames[stringr::str_detect(string = tolower(colNames),
                                 pattern = 'percent')]
  if (length(listRounds) > 0) {
    dataTable <- DT::formatRound(table = dataTable,
                                 columns = listRounds,
                                 digits = 0)
  }
  if (length(listDecimal) > 0) {
    dataTable <- DT::formatRound(table = dataTable,
                                 columns = listDecimal,
                                 digits = 2)
  }
  if (length(listPercent) > 0) {
    dataTable <- DT::formatPercentage(table = dataTable,
                                      columns = listPercent,
                                      digits = 1)
  }
  return(dataTable)
}

# Infoboxes ------------------------------------------------------------------------
addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button",
    id = infoId,
    "i"
  )
  item$children[[1]]$children <-
    append(item$children[[1]]$children, list(infoTag))
  return(item)
}

showInfoBox <- function(title, htmlFileName) {
  shiny::showModal(shiny::modalDialog(
    title = title,
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    HTML(readChar(
      htmlFileName, file.info(htmlFileName)$size
    ))
  ))
}

getConceptSetDataFrameFromConceptSetExpression <-
  function(conceptSetExpression) {
    if ("items" %in% names(conceptSetExpression)) {
      items <- conceptSetExpression$items
    } else {
      items <- conceptSetExpression
    }
    conceptSetExpressionDetails <- items %>%
      purrr::map_df(.f = purrr::flatten)
    if ('CONCEPT_ID' %in% colnames(conceptSetExpressionDetails)) {
      if ('isExcluded' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::rename(IS_EXCLUDED = .data$isExcluded)
      }
      if ('includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::rename(INCLUDE_DESCENDANTS = .data$includeDescendants)
      }
      if ('includeMapped' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::rename(INCLUDE_MAPPED = .data$includeMapped)
      }
      colnames(conceptSetExpressionDetails) <- 
        snakeCaseToCamelCase(colnames(conceptSetExpressionDetails))
    }
    return(conceptSetExpressionDetails)
  }

getConceptSetDetailsFromCohortDefinition <-
  function(cohortDefinitionExpression) {
    if ("expression" %in% names(cohortDefinitionExpression)) {
      expression <- cohortDefinitionExpression$expression
    }
    else {
      expression <- cohortDefinitionExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(dplyr::tibble())
    }
    
    conceptSetExpression <- expression$ConceptSets %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(json = RJSONIO::toJSON(x = .data$expression,
                                           pretty = TRUE))
    
    conceptSetExpressionDetails <- list()
    i <- 0
    for (id in conceptSetExpression$id) {
      i <- i + 1
      conceptSetExpressionDetails[[i]] <-
        getConceptSetDataFrameFromConceptSetExpression(conceptSetExpression = 
                                                         conceptSetExpression[i, ]$expression$items) %>%
        dplyr::mutate(id = conceptSetExpression[i,]$id) %>%
        dplyr::relocate(.data$id) %>% 
        dplyr::arrange(.data$id)
    }
    conceptSetExpressionDetails <-
      dplyr::bind_rows(conceptSetExpressionDetails)
    output <- list(conceptSetExpression = conceptSetExpression,
                   conceptSetExpressionDetails = conceptSetExpressionDetails)
    return(output)
  }


convertMdToHtml <- function(markdown) {
  markdown <- gsub("'", "%sq%", markdown)
  mdFile <- tempfile(fileext = ".md")
  htmlFile <- tempfile(fileext = ".html")
  SqlRender::writeSql(markdown, mdFile)
  rmarkdown::render(input = mdFile,
                    output_format = "html_fragment",
                    output_file = htmlFile,
                    clean = TRUE,
                    quiet = TRUE)
  html <- SqlRender::readSql(htmlFile) 
  unlink(mdFile)
  unlink(htmlFile)
  # Can't find a way to disable "smart quotes", so removing them afterwards:
  html <- gsub("%sq%", "'", html)
  # html <- stringi::stri_escape_unicode(html)
  # html <- gsub("\\\\u00e2\\\\u20ac\\\\u02dc|\\\\u00e2\\\\u20ac\\\\u2122", "'", html)
  # html <- stringi::stri_unescape_unicode(html)
  return(html)
}
