# 04/2023 update: add BETTER-specific data pull functions

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

# 04/12/2023: add functions for BETTER results
getType1s <- function(connection, schema, databaseId, method, exposureId, timeAtRisk) {
  sql <- sprintf("SELECT * 
    FROM %s.type1s
    WHERE database_id = '%s'
      AND method = '%s'
      AND exposure_id = '%s'
      AND time_at_risk = '%s';",
                 schema,
                 databaseId,
                 method,
                 exposureId,
                 timeAtRisk)
  subset <- DatabaseConnector::dbGetQuery(connection, sql)
  #colnames(subset) <- SqlRender::snakeCaseToCamelCase(colnames(subset))
  
  return(subset)
}
#type1s_sub = getType1s(connectionPoolBetter, 'better_results', 'CCAE', 'SCCS', 21214, '1-28')

pullPower <- function(connection, schema, databaseId, method, exposureId, analysisId) {
  sql <- sprintf("SELECT * 
    FROM %s.powers
    WHERE database_id = '%s'
      AND method = '%s'
      AND exposure_id = '%s'
      AND analysis_id = '%s';",
                 schema,
                 databaseId,
                 method,
                 exposureId,
                 analysisId)
  subset <- DatabaseConnector::dbGetQuery(connection, sql)
  #colnames(subset) <- SqlRender::snakeCaseToCamelCase(colnames(subset))
  
  return(subset)
}

pullTTS <- function(connection, schema, databaseId, method, 
                    exposureId, timeAtRisk, sensitivity){
  sql <- sprintf("SELECT * 
    FROM %s.time_to_signal
    WHERE database_id = '%s'
      AND method = '%s'
      AND exposure_id = '%s'
      AND time_at_risk = '%s'
      AND sensitivity = '%s';",
                 schema,
                 databaseId,
                 method,
                 exposureId,
                 timeAtRisk,
                 sensitivity)
  subset <- DatabaseConnector::dbGetQuery(connection, sql)
  
  return(subset)
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

