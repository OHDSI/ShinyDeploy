loadEntireTable <- function(connection, schema, tableName) {
  tryCatch({
    table <- DatabaseConnector::dbReadTable(connection, 
                                            paste(schema, tableName, sep = "."))
  }, error = function(err) {
    stop("Error reading from ", paste(schema, tableName, sep = "."), ": ", err$message)
  })
  colnames(table) <- SqlRender::snakeCaseToCamelCase(colnames(table))
  return(table)
}

getVaccinations <- function(connection, schema) {
  sql <- "SELECT database_id,
    period_id,
    exposure_id,
    MAX(exposure_subjects) AS vaccinations
  FROM @schema.estimate 
  GROUP BY database_id,
    period_id,
    exposure_id;"
  sql <- SqlRender::render(sql, schema = schema)
  vaccinations <- DatabaseConnector::dbGetQuery(connection, sql)
  colnames(vaccinations) <- SqlRender::snakeCaseToCamelCase(colnames(vaccinations))
  return(vaccinations)
}


getEstimates <- function(connection, schema, databaseId, exposureId, timeAtRisk) {
  sql <- sprintf("SELECT estimate.* 
    FROM %s.estimate 
    INNER JOIN %s.analysis
      ON estimate.method = analysis.method
        AND estimate.analysis_id = analysis.analysis_id
    WHERE database_id = '%s'
      AND exposure_id = '%s'
      AND time_at_risk = '%s';",
                 schema,
                 schema,
                 databaseId,
                 exposureId,
                 timeAtRisk)
  subset <- DatabaseConnector::dbGetQuery(connection, sql)
  colnames(subset) <- SqlRender::snakeCaseToCamelCase(colnames(subset))
  
  # Find all controls that have some data in this database
  poweredControls <- subset %>%
    group_by(.data$databaseId, .data$exposureId, .data$outcomeId) %>%
    summarize(maxExposureOutcomes = max(.data$exposureOutcomes), .groups = "drop")
  
  # Add missing estimates so every analysis has full set of controls 
  subset <- subset %>%
    distinct(.data$method, .data$analysisId, .data$exposureId) %>%
    inner_join(subset %>% distinct(.data$periodId), by = character()) %>%
    inner_join(poweredControls, by = "exposureId") %>%
    left_join(subset, by = c("databaseId", "method", "analysisId", "exposureId", "outcomeId", "periodId"))
  
  return(subset)
}

getMontlyRates <- function(connection, schema, databaseId, startDate, endDate) {
  sql <- sprintf("SELECT * 
    FROM %s.monthly_rate
    WHERE database_id = '%s'
      AND start_date >= '%s'
      AND end_date <= '%s';",
                 schema,
                 databaseId,
                 format(startDate, "%Y-%m-%d"),
                 format(endDate, "%Y-%m-%d"))
  rates <- DatabaseConnector::dbGetQuery(connection, sql)
  colnames(rates) <- SqlRender::snakeCaseToCamelCase(colnames(rates))
  rates <- rates %>%
    mutate(ir = abs(1000 * .data$outcomes / (.data$days / 365.25)))
  return(rates)
}