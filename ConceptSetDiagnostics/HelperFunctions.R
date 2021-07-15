camelCaseToTitleCase <- function(string) {
  string <- gsub("([A-Z])", " \\1", string)
  string <- gsub("([a-z])([0-9])", "\\1 \\2", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <-
      gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

standardDataTable <- function(data,
                              selectionMode = "single",
                              selected = NULL,
                              searching = TRUE,
                              pageLength = 10) {
  dataTableFilter =
    list(position = 'top',
         clear = TRUE,
         plain = FALSE)
  
  dataTableOption =
    list(
      pageLength = pageLength,
      lengthMenu = list(c(5, 10, 20,-1), c("5", "10", "20", "All")),
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
  listOfVariablesThatAreAlwaysFactors <- c(
    'domainId',
    'conceptClassId',
    'vocabularyId',
    'standardConcept',
    'conceptSetName',
    'conceptName',
    'conceptId',
    'standard',
    'invalidReason',
    'invalid',
    'conceptCode',
    'isExcluded',
    'excluded',
    'includeDescendants',
    'descendants',
    'includeMapped',
    'mapped'
  )
  
  convertVariableToFactor <- function(data, variables) {
    for (i in (1:length(variables))) {
      variable <- variables[i]
      if (variable %in% colnames(data)) {
        data[[variable]] <- as.factor(data[[variable]])
      }
    }
    return(data %>% dplyr::tibble())
  }
  
  data <- convertVariableToFactor(data = data,
                                  variables = listOfVariablesThatAreAlwaysFactors)
  
  colNamesData <- camelCaseToTitleCase(colnames(data))
  
  dataTable <- DT::datatable(
    data = data,
    class = "stripe compact order-column hover",
    rownames = FALSE,
    options = dataTableOption,
    colnames = colNamesData,
    filter = dataTableFilter,
    # style = 'bootstrap4',
    escape = FALSE,
    selection = list(
      mode = selectionMode,
      target = "row",
      selected = selected
    ),
    editable = FALSE,
    # container = sketch,
    extensions = c('Buttons', 'ColReorder', 'FixedColumns', 'FixedHeader'),
    plugins = c('natural') #'ellipsis'
    # escape = FALSE
  )
  return(dataTable)
}


copyToClipboardButton <-
  function(toCopyId,
           label = "Copy to clipboard",
           icon = shiny::icon("clipboard"),
           ...) {
    script <- sprintf(
      "
  text = document.getElementById('%s').textContent;
  html = document.getElementById('%s').innerHTML;
  function listener(e) {
    e.clipboardData.setData('text/html', html);
    e.clipboardData.setData('text/plain', text);
    e.preventDefault();
  }
  document.addEventListener('copy', listener);
  document.execCommand('copy');
  document.removeEventListener('copy', listener);
  return false;",
      toCopyId,
      toCopyId
    )
    
    tags$button(type = "button",
                class = "btn btn-default action-button",
                onclick = script,
                icon,
                label,
                ...)
  }
