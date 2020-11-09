createSimParamWidget <- function(simParam, results, suffix) {
  values <- unique(results[, simParam])
  values <- values[order(values)]
  checkboxGroupInput(paste0(simParam, suffix), simParam, choices = values, selected = values, inline = TRUE)
}
