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
  listOfVariablesThatAreAlwaysFactors <- c('Outcome', 'Data Source', 'Age', 'Sex')
  
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
  
  dataTable <- DT::datatable(
    data = data,
    class = "stripe compact order-column hover",
    rownames = FALSE,
    options = dataTableOption,
    colnames = colnames(data),
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
  return(dataTable)
}