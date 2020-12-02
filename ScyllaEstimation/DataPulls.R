# getExposureName <- function(connectionPool, exposureId) {
#   sql <- "SELECT exposure_name FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
#   UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
#   sql <- SqlRender::renderSql(sql, exposure_id = exposureId)$sql
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   exposureName <- querySql(connectionPool, sql)
#   return(exposureName[1, 1])
# }
# 
# getExposureDescription <- function(connectionPool, exposureId) {
#   sql <- "SELECT description FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
#   UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
#   sql <- SqlRender::renderSql(sql, exposure_id = exposureId)$sql
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   exposureDescription <- querySql(connectionPool, sql)
#   return(exposureDescription[1, 1])
# }
# 
# getOutcomeName <- function(connectionPool, outcomeId) {
#   sql <- "SELECT outcome_name FROM outcome_of_interest WHERE outcome_id = @outcome_id"
#   sql <- SqlRender::renderSql(sql, outcome_id = outcomeId)$sql
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   outcomeName <- querySql(connectionPool, sql)
#   return(outcomeName[1, 1])
# }
# 
# getIndications <- function(connectionPool) {
#   sql <- "SELECT indication_id, indication_name FROM indication"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   indications <- querySql(connectionPool, sql)
#   colnames(indications) <- SqlRender::snakeCaseToCamelCase(colnames(indications))
#   return(indications)
# }
# 
# getSubgroups <- function(connectionPool) {
#   sql <- "SELECT DISTINCT interaction_covariate_id AS subgroup_id, covariate_name AS subgroup_name
#   FROM (
#   SELECT DISTINCT interaction_covariate_id
#   FROM cm_interaction_result
#   ) ids
#   INNER JOIN covariate
#   ON interaction_covariate_id = covariate_id"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   subgroups <- querySql(connectionPool, sql)
#   colnames(subgroups) <- SqlRender::snakeCaseToCamelCase(colnames(subgroups))
#   subgroups$subgroupName <- gsub("Subgroup: ", "", subgroups$subgroupName)
#   return(subgroups)
# }
# 
# 
# getExposures <- function(connectionPool) {
#   sql <- "SELECT * FROM (
#   SELECT exposure_id, exposure_name, indication_id FROM single_exposure_of_interest
#   UNION ALL SELECT exposure_id, exposure_name, indication_id FROM combi_exposure_of_interest
#   ) exposure
#   INNER JOIN exposure_group
#   ON exposure.exposure_id = exposure_group.exposure_id;"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   exposures <- querySql(connectionPool, sql)
#   colnames(exposures) <- SqlRender::snakeCaseToCamelCase(colnames(exposures))
#   return(exposures)
# }
# 
# getOutcomes <- function(connectionPool) {
#   sql <- "SELECT outcome_id, outcome_name, indication_id FROM outcome_of_interest"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   outcomes <- querySql(connectionPool, sql)
#   colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
#   return(outcomes)
# }
# 
# getAnalyses <- function(connectionPool) {
#   sql <- "SELECT analysis_id, description FROM cohort_method_analysis"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   analyses <- querySql(connectionPool, sql)
#   colnames(analyses) <- SqlRender::snakeCaseToCamelCase(colnames(analyses))
#   return(analyses)
# }
# 
# getDatabases <- function(connectionPool) {
#   sql <- "SELECT * FROM database"
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   databases <- querySql(connectionPool, sql)
#   colnames(databases) <- SqlRender::snakeCaseToCamelCase(colnames(databases))
#   return(databases)
# }
# 
# getDatabaseDetails <- function(connectionPool, databaseId) {
#   sql <- "SELECT * FROM database WHERE database_id = '@database_id'"
#   sql <- SqlRender::renderSql(sql, database_id = databaseId)$sql
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   databaseDetails <- querySql(connectionPool, sql)
#   colnames(databaseDetails) <- SqlRender::snakeCaseToCamelCase(colnames(databaseDetails))
#   databaseDetails$description <- sub("\\n", " ", databaseDetails$description)
#   databaseDetails$description <- sub("JDMC",
#                                      "JMDC",
#                                      databaseDetails$description)  # TODO Fix in schema
#   return(databaseDetails)
# }
# 
# getIndicationForExposure <- function(connectionPool, exposureIds = c()) {
#   sql <- "SELECT exposure_id, indication_id FROM single_exposure_of_interest WHERE"
#   sql <- paste(sql, paste0("exposure_id IN (", paste(exposureIds, collapse = ", "), ")"))
# 
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   indications <- querySql(connectionPool, sql)
#   colnames(indications) <- SqlRender::snakeCaseToCamelCase(colnames(indications))
#   return(indications)
# }
# 
# getTcoDbs <- function(connectionPool,
#                       targetIds = c(),
#                       comparatorIds = c(),
#                       outcomeIds = c(),
#                       databaseIds = c(),
#                       operator = "AND") {
#   sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
#   parts <- c()
#   if (length(targetIds) != 0) {
#     parts <- c(parts, paste0("target_id IN (", paste(targetIds, collapse = ", "), ")"))
#   }
#   if (length(comparatorIds) != 0) {
#     parts <- c(parts, paste0("comparator_id IN (", paste(comparatorIds, collapse = ", "), ")"))
#   }
#   if (length(outcomeIds) != 0) {
#     parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")"))
#   }
#   if (length(databaseIds) != 0) {
#     parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')"))
#   }
#   if (length(parts) != 0) {
#     if (operator == "AND") {
#       sql <- paste(sql, "AND", paste(parts, collapse = " AND "))
#     } else {
#       sql <- paste(sql, "AND", paste(parts, collapse = " OR "))
#     }
#   }
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   tcoDbs <- querySql(connectionPool, sql)
#   colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
#   return(tcoDbs)
# }
# 
# getTcoDbsStrict <- function(connectionPool, exposureIds = c(), outcomeIds = c(), databaseIds = c()) {
#   sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
#   parts <- c()
#   if (length(exposureIds) != 0) {
#     for (exposureId in exposureIds) {
#       parts <- c(parts,
#                  paste0("(target_id = ", exposureId, " OR comparator_id = ", exposureId, ")"))
#     }
#   }
#   if (length(outcomeIds) != 0) {
#     parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")"))
#   }
#   if (length(databaseIds) != 0) {
#     parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')"))
#   }
#   if (length(parts) != 0) {
#     sql <- paste(sql, "AND", paste(parts, collapse = " AND "))
#   }
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   tcoDbs <- querySql(connectionPool, sql)
#   colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
#   return(tcoDbs)
# }
# 
getMainResults <- function(connectionPool,
                           targetIds = c(),
                           comparatorIds = c(),
                           outcomeIds = c(),
                           databaseIds = c(),
                           analysisIds = c()) {
  sql <- sprintf("SELECT * FROM %s.cohort_method_result", resultsDatabaseSchema)
  parts <- c()
  if (length(targetIds) != 0) {
    parts <- c(parts, paste0("target_id IN (", paste(targetIds, collapse = ", "), ")"))
  }
  if (length(comparatorIds) != 0) {
    parts <- c(parts, paste0("comparator_id IN (", paste(comparatorIds, collapse = ", "), ")"))
  }
  if (length(outcomeIds) != 0) {
    parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")"))
  }
  if (length(databaseIds) != 0) {
    parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')"))
  }
  if (length(analysisIds) != 0) {
    parts <- c(parts, paste0("analysis_id IN (", paste(analysisIds, collapse = ", "), ")"))
  }
  if (length(parts) != 0) {
    sql <- paste(sql, "WHERE", paste(parts, collapse = " AND "))
  }
  results <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  return(results)
}

# getSubgroupResults <- function(connectionPool,
#                                targetIds = c(),
#                                comparatorIds = c(),
#                                outcomeIds = c(),
#                                databaseIds = c(),
#                                analysisIds = c(),
#                                subgroupIds = c(),
#                                estimatesOnly = FALSE) {
#   idx <- rep(TRUE, nrow(cmInteractionResult))
#   if (length(targetIds) != 0) {
#     idx <- idx & cmInteractionResult$targetId %in% targetIds
#   }
#   if (length(comparatorIds) != 0) {
#     idx <- idx & cmInteractionResult$comparatorId %in% comparatorIds
#   }
#   if (length(outcomeIds) != 0) {
#     idx <- idx & cmInteractionResult$outcomeId %in% outcomeIds
#   }
#   if (length(databaseIds) != 0) {
#     idx <- idx & cmInteractionResult$databaseId %in% databaseIds
#   }
#   if (length(analysisIds) != 0) {
#     idx <- idx & cmInteractionResult$analysisId %in% analysisIds
#   }
#   if (length(subgroupIds) != 0) {
#     idx <- idx & cmInteractionResult$interactionCovariateId %in% subgroupIds
#   }
#   result <- cmInteractionResult[idx, ]
#   result <- merge(result, data.frame(interactionCovariateId = covariate$covariateId,
#                                      databaseId = covariate$databaseId,
#                                      covariateName = covariate$covariateName))
#   result <- result[, c("covariateName",
#                        "targetSubjects",
#                        "comparatorSubjects",
#                        "rrr",
#                        "ci95Lb",
#                        "ci95Ub",
#                        "p",
#                        "calibratedP")]
#   colnames(result) <- c("interactionCovariateName",
#                         "targetSubjects",
#                         "comparatorSubjects",
#                         "rrr",
#                         "ci95Lb",
#                         "ci95Ub",
#                         "p",
#                         "calibratedP")
#   return(result)
# }
# 
getControlResults <- function(connectionPool, targetId, comparatorId, analysisId, databaseId) {
  sql <- "SELECT *
  FROM @results_database_schema.cohort_method_result
  INNER JOIN @results_database_schema.negative_control_outcome
    ON negative_control_outcome.outcome_id = cohort_method_result.outcome_id
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           database_id = databaseId,
                           analysis_id = analysisId,
                           results_database_schema = resultsDatabaseSchema)
  results <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  results$effectSize <- 1
  return(results)
}

getCmFollowUpDist <- function(connectionPool,
                              targetId,
                              comparatorId,
                              outcomeId,
                              databaseId,
                              analysisId) {
  sql <- "SELECT target_min_days,
  target_p10_days,
  target_p25_days,
  target_median_days,
  target_p75_days,
  target_p90_days,
  target_max_days,
  comparator_min_days,
  comparator_p10_days,
  comparator_p25_days,
  comparator_median_days,
  comparator_p75_days,
  comparator_p90_days,
  comparator_max_days
  FROM @results_database_schema.cm_follow_up_dist
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND outcome_id = @outcome_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           outcome_id = outcomeId,
                           database_id = databaseId,
                           analysis_id = analysisId,
                           results_database_schema = resultsDatabaseSchema)
  followUpDist <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(followUpDist) <- SqlRender::snakeCaseToCamelCase(colnames(followUpDist))
  return(followUpDist)
}

getCovariateBalance <- function(connectionPool,
                                targetId,
                                comparatorId,
                                databaseId,
                                analysisId,
                                outcomeId = NULL) {
  
  sql <- "SELECT covariate.covariate_id, covariate_name, covariate_analysis_id,
        target_mean_before,
        comparator_mean_before,
        std_diff_before,
        target_mean_after,
        comparator_mean_after,
        std_diff_after
      FROM @results_database_schema.covariate_balance
      INNER JOIN @results_database_schema.covariate
      ON covariate_balance.covariate_id = covariate.covariate_id
      AND covariate_balance.database_id = covariate.database_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND covariate.database_id = '@database_id'
      AND analysis_id = @analysis_id
      {@outcome_id == \"\"} ? {AND outcome_id = -1} : {AND outcome_id = @outcome_id}"
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           database_id = databaseId,
                           analysis_id = analysisId,
                           outcome_id = outcomeId,
                           results_database_schema = resultsDatabaseSchema)
  balance <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(balance) <- c("covariateId",
                         "covariateName",
                         "analysisId",
                         "beforeMatchingMeanTreated",
                         "beforeMatchingMeanComparator",
                         "beforeMatchingStdDiff",
                         "afterMatchingMeanTreated",
                         "afterMatchingMeanComparator",
                         "afterMatchingStdDiff")
  balance$absBeforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
  balance$absAfterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  return(balance)
}

getPs <- function(connectionPool, targetIds, comparatorIds, analysisId, databaseId) {
  sql <- "SELECT database_id,
      target_id,
      comparator_id,
      preference_score,
      target_density,
      comparator_density
      FROM @results_database_schema.preference_score_dist
      WHERE target_id IN (@target_ids)
      AND comparator_id IN (@comparator_ids)
      AND analysis_id IN (@analysis_ids)
      {@database_id != \"\"} ? {AND database_id = '@database_id'};"
  sql <- SqlRender::render(sql,
                           target_ids = targetIds,
                           comparator_ids = comparatorIds,
                           analysis_ids = analysisId,
                           database_id = databaseId,
                           results_database_schema = resultsDatabaseSchema)
  ps <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  if (databaseId != "") {
    ps$databaseId <- NULL
  }
  return(ps)
}
# 
# getKaplanMeier <- function(connectionPool, targetId, comparatorId, outcomeId, databaseId, analysisId) {
#   file <- sprintf("kaplan_meier_dist_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
#   km <- readRDS(file.path(dataFolder, file))
#   colnames(km) <- SqlRender::snakeCaseToCamelCase(colnames(km))
#   km <- km[km$outcomeId == outcomeId & km$analysisId == analysisId, ]
# 
#   return(km)
# }
# 
getAttrition <- function(connectionPool, targetId, comparatorId, outcomeId, analysisId, databaseId) {
  sql <- "SELECT exposure_id,
  sequence_number,
  description,
  subjects
  FROM @results_database_schema.attrition
  WHERE (target_id IS NULL OR target_id = @target_id)
  AND (comparator_id IS NULL OR comparator_id = @comparator_id)
  AND (outcome_id IS NULL OR outcome_id = @outcome_id)
  AND (exposure_id = @target_id OR exposure_id = @comparator_id)
  AND (analysis_id IS NULL OR analysis_id = @analysis_id)
  AND database_id = '@database_id'"
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           outcome_id = outcomeId,
                           analysis_id = analysisId,
                           database_id = databaseId,
                           results_database_schema = resultsDatabaseSchema)
  attrition <- DatabaseConnector::dbGetQuery(connectionPool , sql)
  colnames(attrition) <- SqlRender::snakeCaseToCamelCase(colnames(attrition))
  targetAttrition <- attrition[attrition$exposureId == targetId, ]
  comparatorAttrition <- attrition[attrition$exposureId == comparatorId, ]
  colnames(targetAttrition)[colnames(targetAttrition) == "subjects"] <- "targetPersons"
  targetAttrition$exposureId <- NULL
  colnames(comparatorAttrition)[colnames(comparatorAttrition) == "subjects"] <- "comparatorPersons"
  comparatorAttrition$exposureId <- NULL
  attrition <- merge(targetAttrition, comparatorAttrition)
  attrition <- attrition[order(attrition$sequenceNumber), ]
  return(attrition)
}
# 
# getStudyPeriod <- function(connectionPool, targetId, comparatorId, databaseId) {
#   sql <- "SELECT min_date,
#   max_date
#   FROM comparison_summary
#   WHERE target_id = @target_id
#   AND comparator_id = @comparator_id
#   AND database_id = '@database_id'"
#   sql <- SqlRender::renderSql(sql,
#                               target_id = targetId,
#                               comparator_id = comparatorId,
#                               database_id = databaseId)$sql
#   sql <- SqlRender::translateSql(sql, targetDialect = connectionPool@dbms)$sql
#   studyPeriod <- querySql(connectionPool, sql)
#   colnames(studyPeriod) <- SqlRender::snakeCaseToCamelCase(colnames(studyPeriod))
#   return(studyPeriod)
# }
# 
getPropensityModel <- function(connectionPool, targetId, comparatorId, analysisId, databaseId) {
  sql <- "SELECT coefficient, covariate.covariate_id, covariate_name
      FROM @results_database_schema.propensity_model
      INNER JOIN @results_database_schema.covariate
      ON propensity_model.covariate_id = covariate.covariate_id
      AND propensity_model.database_id = covariate.database_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND covariate.database_id = '@database_id'
      AND analysis_id = @analysis_id;"
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           database_id = databaseId,
                           analysis_id = analysisId,
                           results_database_schema = resultsDatabaseSchema)
  model <- DatabaseConnector::dbGetQuery(connectionPool, sql)
  colnames(model) <- SqlRender::snakeCaseToCamelCase(colnames(model))
  return(model)
}
