getExposureName <- function(connection, exposureId) {
  sql <- "SELECT exposure_name FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
  UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
  sql <- SqlRender::renderSql(sql, exposure_id = exposureId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  exposureName <- querySql(connection, sql)
  return(exposureName[1, 1])
}

getExposureDescription <- function(connection, exposureId) {
  sql <- "SELECT description FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
  UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
  sql <- SqlRender::renderSql(sql, exposure_id = exposureId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  exposureDescription <- querySql(connection, sql)
  return(exposureDescription[1, 1])
}

getOutcomeName <- function(connection, outcomeId) {
  sql <- "SELECT outcome_name FROM outcome_of_interest WHERE outcome_id = @outcome_id"
  sql <- SqlRender::renderSql(sql, outcome_id = outcomeId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  outcomeName <- querySql(connection, sql)
  return(outcomeName[1, 1])
}

getIndications <- function(connection) {
  sql <- "SELECT indication_id, indication_name FROM indication"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  indications <- querySql(connection, sql)
  colnames(indications) <- SqlRender::snakeCaseToCamelCase(colnames(indications))
  return(indications)
}

getSubgroups <- function(connection) {
  sql <- "SELECT DISTINCT interaction_covariate_id AS subgroup_id, covariate_name AS subgroup_name
    FROM (
      SELECT DISTINCT interaction_covariate_id
      FROM cm_interaction_result
    ) ids
    INNER JOIN covariate
    ON interaction_covariate_id = covariate_id"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  subgroups <- querySql(connection, sql)
  colnames(subgroups) <- SqlRender::snakeCaseToCamelCase(colnames(subgroups))
  subgroups$subgroupName <- gsub("Subgroup: ", "", subgroups$subgroupName)
  return(subgroups)
}


getExposures <- function(connection, filterByCmResults = TRUE) {
  sql <- "SELECT * FROM (
    SELECT exposure_id, exposure_name, indication_id FROM single_exposure_of_interest
    UNION ALL SELECT exposure_id, exposure_name, indication_id FROM combi_exposure_of_interest
  ) exposure
  INNER JOIN exposure_group
  ON exposure.exposure_id = exposure_group.exposure_id
  {@filter_by_cm_results} ? {
    INNER JOIN exposure_ids
    ON exposure_ids.exposure_id = exposure.exposure_id
  }
  ;"
  sql <- SqlRender::renderSql(sql, filter_by_cm_results = filterByCmResults)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  exposures <- querySql(connection, sql)
  colnames(exposures) <- SqlRender::snakeCaseToCamelCase(colnames(exposures))
  return(exposures)
}

getOutcomes <- function(connection) {
  sql <- "SELECT outcome_id, outcome_name, indication_id FROM outcome_of_interest"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  outcomes <- querySql(connection, sql)
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  return(outcomes)
}

getAnalyses <- function(connection) {
  sql <- "SELECT analysis_id, description FROM cohort_method_analysis"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  analyses <- querySql(connection, sql)
  colnames(analyses) <- SqlRender::snakeCaseToCamelCase(colnames(analyses))
  return(analyses)
}

getDatabases <- function(connection) {
  sql <- "SELECT * FROM database"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  databases <- querySql(connection, sql)
  colnames(databases) <- SqlRender::snakeCaseToCamelCase(colnames(databases))
  return(databases)
}

getDatabaseDetails <- function(connection, databaseId) {
  sql <- "SELECT * FROM database WHERE database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql, database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  databaseDetails <- querySql(connection, sql)
  colnames(databaseDetails) <- SqlRender::snakeCaseToCamelCase(colnames(databaseDetails))
  databaseDetails$description <- sub("\\n", " ", databaseDetails$description)
  databaseDetails$description <- sub("JDMC", "JMDC", databaseDetails$description) # TODO Fix in schema
  return(databaseDetails)
}

getIndicationForExposure <- function(connection,
                                     exposureIds = c()) {
  sql <- "SELECT exposure_id, indication_id FROM single_exposure_of_interest WHERE"
  sql <- paste(sql, paste0("exposure_id IN (", paste(exposureIds, collapse = ", "), ")"))

  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  indications <- querySql(connection, sql)
  colnames(indications) <- SqlRender::snakeCaseToCamelCase(colnames(indications))
  return(indications)
}

getTcoDbs <- function(connection,
                      targetIds = c(),
                      comparatorIds = c(),
                      outcomeIds = c(),
                      databaseIds = c(),
                      operator = "AND",
                      limit = 0) {
  sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
  if (limit != 0) {
    sql <- gsub("SELECT target_id", sprintf("SELECT TOP %s target_id", limit), sql)
  }
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
  if (length(parts) != 0) {
    if (operator == "AND") {
      sql <- paste(sql, "AND", paste(parts, collapse = " AND "))
    } else {
      sql <- paste(sql, "AND", paste(parts, collapse = " OR "))
    }
  }
  sql <- paste0(sql, ";")
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  tcoDbs <- querySql(connection, sql)
  colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
  return(tcoDbs)
}

getTcoDbsStrict <- function(connection, exposureIds = c(), outcomeIds = c(), databaseIds = c()) {
  sql <- "SELECT TOP 100 target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
  parts <- c()
  if (length(exposureIds) != 0) {
    for (exposureId in exposureIds) {
      parts <- c(parts,
                 paste0("(target_id = ", exposureId, " OR comparator_id = ", exposureId, ")"))
    }
  }
  if (length(outcomeIds) != 0) {
    parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")"))
  }
  if (length(databaseIds) != 0) {
    parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')"))
  }
  if (length(parts) != 0) {
    sql <- paste(sql, "AND", paste(parts, collapse = " AND "))
  }
  sql <- paste0(sql, ";")
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  tcoDbs <- querySql(connection, sql)
  colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
  return(tcoDbs)
}

getMainResults <- function(connection,
                           targetIds = c(),
                           comparatorIds = c(),
                           outcomeIds = c(),
                           databaseIds = c(),
                           analysisIds = c(),
                           estimatesOnly = FALSE) {
  if (estimatesOnly) {
    sql <- "SELECT calibrated_log_rr, calibrated_se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub FROM cohort_method_result"
  } else {
    sql <- "SELECT * FROM cohort_method_result"
  }
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
    parts <- c(parts, paste0("analysis_id IN ('", paste(analysisIds, collapse = "', '"), "')"))
  }
  if (length(parts) != 0) {
    sql <- paste(sql, "WHERE", paste(parts, collapse = " AND "))
  }
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  results <- querySql(connection, sql)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  return(results)
}

getSubgroupResults <- function(connection,
                               targetIds = c(),
                               comparatorIds = c(),
                               outcomeIds = c(),
                               databaseIds = c(),
                               analysisIds = c(),
                               subgroupIds = c(),
                               estimatesOnly = FALSE) {
  if (estimatesOnly) {
    sql <- "
      SELECT ci_95_lb,
        ci_95_ub,
        log_rrr,
        se_log_rrr
      FROM cm_interaction_result
    "
  } else {
    sql <- "SELECT target_id,
    comparator_id,
    outcome_id,
    cm_interaction_result.analysis_id,
    cohort_method_analysis.description AS analysis_description,
    cm_interaction_result.database_id,
    interaction_covariate_id,
    covariate_name AS interaction_covariate_name,
    rrr,
    ci_95_lb,
    ci_95_ub,
    p,
    calibrated_p,
    i_2,
    log_rrr,
    se_log_rrr,
    target_subjects,
    comparator_subjects,
    target_days,
    comparator_days,
    target_outcomes,
    comparator_outcomes
  FROM cm_interaction_result
  INNER JOIN covariate
  ON cm_interaction_result.interaction_covariate_id = covariate.covariate_id
  AND cm_interaction_result.database_id = covariate.database_id
  INNER JOIN cohort_method_analysis
  ON cm_interaction_result.analysis_id = cohort_method_analysis.analysis_id"
  }
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
    parts <- c(parts, paste0("cm_interaction_result.database_id IN ('",
                             paste(databaseIds, collapse = "', '"),
                             "')"))
  }
  if (length(analysisIds) != 0) {
    parts <- c(parts, paste0("cm_interaction_result.analysis_id IN (", paste(analysisIds, collapse = ", "), ")"))
  }
  if (length(subgroupIds) != 0) {
    parts <- c(parts, paste0("interaction_covariate_id IN (", paste(subgroupIds, collapse = ", "), ")"))
  }

  if (length(parts) != 0) {
    sql <- paste(sql, "WHERE", paste(parts, collapse = " AND "))
  }
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  results <- querySql(connection, sql)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  return(results)
}

getControlResults <- function(connection, targetId, comparatorId, analysisId, databaseId) {
  sql <- "SELECT *
  FROM cohort_method_result
  INNER JOIN (
  SELECT outcome_id,
    outcome_name,
    CAST(1 AS FLOAT) AS effect_size
  FROM negative_control_outcome

  UNION ALL

  SELECT outcome_id,
    outcome_name,
    effect_size
   FROM positive_control_outcome
  ) outcomes
  ON cohort_method_result.outcome_id = outcomes.outcome_id
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId,
                              analysis_id = analysisId)$sql
  results <- querySql(connection, sql)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  return(results)
}

getCmFollowUpDist <- function(connection,
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
  FROM cm_follow_up_dist
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND outcome_id = @outcome_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              outcome_id = outcomeId,
                              database_id = databaseId,
                              analysis_id = analysisId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  followUpDist <- querySql(connection, sql)
  colnames(followUpDist) <- SqlRender::snakeCaseToCamelCase(colnames(followUpDist))
  return(followUpDist)
}

getCovariateBalance <- function(connection,
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
      FROM covariate_balance
      INNER JOIN covariate
      ON covariate_balance.covariate_id = covariate.covariate_id
      AND covariate_balance.database_id = covariate.database_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND covariate.database_id = '@database_id'
      AND analysis_id = @analysis_id
      AND interaction_covariate_id IS NULL
      {@outcome_id == \"\"} ? {AND outcome_id IS NULL} : {AND outcome_id = @outcome_id}"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId,
                              analysis_id = analysisId,
                              outcome_id = outcomeId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  balance <- querySql(connection, sql)
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

getPs <- function(connection, targetIds, comparatorIds, databaseId) {
  sql <- "SELECT target_id,
      comparator_id,
      preference_score,
      target_density,
      comparator_density
      FROM preference_score_dist
      WHERE target_id IN (@target_ids)
      AND comparator_id IN (@comparator_ids)
      AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql,
                              target_ids = targetIds,
                              comparator_ids = comparatorIds,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  ps <- querySql(connection, sql)
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  return(ps)
}

getKaplanMeier <- function(connection, targetId, comparatorId, outcomeId, databaseId, analysisId) {
  sql <- "SELECT time,
  target_at_risk,
  comparator_at_risk,
  target_survival,
  target_survival_lb,
  target_survival_ub,
  comparator_survival,
  comparator_survival_lb,
  comparator_survival_ub
  FROM kaplan_meier_dist
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND outcome_id = @outcome_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              outcome_id = outcomeId,
                              database_id = databaseId,
                              analysis_id = analysisId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  ps <- querySql(connection, sql)
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  return(ps)
}

getAttrition <- function(connection, targetId, comparatorId, outcomeId, analysisId, databaseId) {
  sql <- "SELECT exposure_id,
  sequence_number,
  description,
  subjects
  FROM attrition
  WHERE (target_id IS NULL OR target_id = @target_id)
  AND (comparator_id IS NULL OR comparator_id = @comparator_id)
  AND (outcome_id IS NULL OR outcome_id = @outcome_id)
  AND (exposure_id = @target_id OR exposure_id = @comparator_id)
  AND (analysis_id IS NULL OR analysis_id = @analysis_id)
  AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              outcome_id = outcomeId,
                              analysis_id = analysisId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  attrition <- querySql(connection, sql)
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

getStudyPeriod <- function(connection, targetId, comparatorId, databaseId) {
  sql <- "SELECT min_date,
  max_date
  FROM comparison_summary
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  studyPeriod <- querySql(connection, sql)
  colnames(studyPeriod) <- SqlRender::snakeCaseToCamelCase(colnames(studyPeriod))
  return(studyPeriod)
}
