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


getExposures <- function(connection) {
  sql <- "SELECT * FROM (
    SELECT exposure_id, exposure_name, indication_id FROM single_exposure_of_interest
    UNION ALL SELECT exposure_id, exposure_name, indication_id FROM combi_exposure_of_interest
  ) exposure
  INNER JOIN exposure_group
  ON exposure.exposure_id = exposure_group.exposure_id;"
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
                      operator = "AND") {
  sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
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
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  tcoDbs <- querySql(connection, sql)
  colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
  return(tcoDbs)
}

getTcoDbsStrict <- function(connection, exposureIds = c(), outcomeIds = c(), databaseIds = c()) {
  sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
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
                           analysisIds = c()) {
  idx <- rep(TRUE, nrow(cohortMethodResult))
  if (length(targetIds) != 0) {
     idx <- idx & cohortMethodResult$targetId %in% targetIds
  }
  if (length(comparatorIds) != 0) {
    idx <- idx & cohortMethodResult$comparatorId %in% comparatorIds
  }
  if (length(outcomeIds) != 0) {
    idx <- idx & cohortMethodResult$outcomeId %in% outcomeIds
  }
  if (length(databaseIds) != 0) {
    idx <- idx & cohortMethodResult$databaseId %in% databaseIds
  }
  if (length(analysisIds) != 0) {
    idx <- idx & cohortMethodResult$analysisId %in% analysisIds
  }
  return(cohortMethodResult[idx, ])
}

getSubgroupResults <- function(connection,
                               targetIds = c(),
                               comparatorIds = c(),
                               outcomeIds = c(),
                               databaseIds = c(),
                               analysisIds = c(),
                               subgroupIds = c(),
                               estimatesOnly = FALSE) {
  idx <- rep(TRUE, nrow(cmInteractionResult))
  if (length(targetIds) != 0) {
    idx <- idx & cmInteractionResult$targetId %in% targetIds
  }
  if (length(comparatorIds) != 0) {
    idx <- idx & cmInteractionResult$comparatorId %in% comparatorIds
  }
  if (length(outcomeIds) != 0) {
    idx <- idx & cmInteractionResult$outcomeId %in% outcomeIds
  }
  if (length(databaseIds) != 0) {
    idx <- idx & cmInteractionResult$databaseId %in% databaseIds
  }
  if (length(analysisIds) != 0) {
    idx <- idx & cmInteractionResult$analysisId %in% analysisIds
  }
  if (length(subgroupIds) != 0) {
    idx <- idx & cmInteractionResult$interactionCovariateId %in% subgroupIds
  }
  result <- cmInteractionResult[idx, ]
  result <- merge(result, data.frame(interactionCovariateId = covariate$covariateId,
                                     databaseId = covariate$databaseId,
                                     covariateName = covariate$covariateName))
  result <- result[, c("covariateName",
                       "targetSubjects",
                       "comparatorSubjects",
                       "rrr",
                       "ci95Lb",
                       "ci95Ub",
                       "p",
                       "calibratedP")]
  colnames(result) <- c("interactionCovariateName",
                        "targetSubjects",
                        "comparatorSubjects",
                        "rrr",
                        "ci95Lb",
                        "ci95Ub",
                        "p",
                        "calibratedP")
  return(result)
}

getControlResults <- function(connection, targetId, comparatorId, analysisId, databaseId = NULL, includePositiveControls = TRUE) {
  results <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                  cohortMethodResult$comparatorId == comparatorId &
                                  cohortMethodResult$analysisId == analysisId, ]
  if (!is.null(databaseId))
    results <- results[results$databaseId == databaseId, ]
  
  results$effectSize <- NA
  idx <- results$outcomeId %in% negativeControlOutcome$outcomeId
  results$effectSize[idx] <- 1
  if (!is.null(positiveControlOutcome) && includePositiveControls) {
    idx <- results$outcomeId %in% positiveControlOutcome$outcomeId
    results$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
                                                                       positiveControlOutcome$outcomeId)]
  }
  
  results <- results[!is.na(results$effectSize), ]
  return(results)
}

getCmFollowUpDist <- function(connection,
                              targetId,
                              comparatorId,
                              outcomeId,
                              databaseId = NULL,
                              analysisId) {
  followUpDist <- cmFollowUpDist[cmFollowUpDist$targetId == targetId &
                                 cmFollowUpDist$comparatorId == comparatorId &
                                 cmFollowUpDist$outcomeId == outcomeId &
                                 cmFollowUpDist$analysisId == analysisId, ]
  if (!is.null(databaseId)) {
    followUpDist <- followUpDist[followUpDist$databaseId == databaseId, ]
  }
  return(followUpDist)
}

getCovariateBalance <- function(connection,
                                targetId,
                                comparatorId,
                                analysisId,
                                databaseId = NULL,
                                outcomeId = NULL) {
  
  loadCovariateBalance <- function(file, parseDatabaseName = FALSE) {
    balance <- readRDS(file)
    if (parseDatabaseName) {
      fileBaseName <- basename(file)
      databaseId <- gsub(pattern = "^covariate_balance_t\\d+_c\\d+|\\.rds", replacement = "", x = fileBaseName)
      if (is.null(databaseId) || databaseId == "")
        databaseId <- fileBaseName
      balance[, "databaseId"] <- databaseId
    }
    return(balance)
  }
  
  if (!is.null(databaseId)) {
    file <- sprintf("covariate_balance_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
    balance <- loadCovariateBalance(file.path(dataFolder, file))
  } else {
    balanceFiles <- list.files(dataFolder, pattern = sprintf("^covariate_balance_t%s_c%s_.*\\.rds", targetId, comparatorId), full.names = TRUE)
    balance <- do.call("rbind", lapply(balanceFiles, loadCovariateBalance, parseDatabaseName = TRUE))
  }
  colnames(balance) <- SqlRender::snakeCaseToCamelCase(colnames(balance))
  balance <- balance[balance$analysisId == analysisId, ]
  if (!is.null(outcomeId))
    balance <- balance[balance$outcomeId == outcomeId, ]
  
  if (!is.null(databaseId))
    balance <- merge(balance, covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId, 
                                      c("covariateId", "covariateAnalysisId", "covariateName")])
  else
    balance <- merge(balance, covariate[covariate$analysisId == analysisId, 
                                        c("covariateId", "covariateAnalysisId", "covariateName")])
  balance <- balance[ c("databaseId",
                        "covariateId",
                        "covariateName",
                        "covariateAnalysisId", 
                        "targetMeanBefore", 
                        "comparatorMeanBefore", 
                        "stdDiffBefore", 
                        "targetMeanAfter", 
                        "comparatorMeanAfter",
                        "stdDiffAfter")]
  colnames(balance) <- c("databaseId",
                         "covariateId",
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

getPs <- function(connection, targetIds, comparatorIds, analysisId, databaseId = "") {
  if(databaseId != "") {
    file <- sprintf("preference_score_dist_t%s_c%s_%s.rds", targetIds, comparatorIds, databaseId)
    ps <- readRDS(file.path(dataFolder, file))
  } else {
    psFiles <- list.files(dataFolder, pattern = sprintf("^preference_score_dist_t%s_c%s_.*\\.rds", targetIds, comparatorIds), full.names = TRUE)
    ps <- do.call("rbind", lapply(psFiles, readRDS))
  }
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  ps <- ps[ps$analysisId == analysisId, ]
  if (databaseId != "") {
    ps$databaseId <- NULL
  }
  return(ps)
}

getKaplanMeier <- function(connection, targetId, comparatorId, outcomeId, databaseId, analysisId) {
  file <- sprintf("kaplan_meier_dist_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
  km <- readRDS(file.path(dataFolder, file))
  colnames(km) <- SqlRender::snakeCaseToCamelCase(colnames(km))
  km <- km[km$outcomeId == outcomeId &
             km$analysisId == analysisId, ]
  
  return(km)
}

getAttrition <- function(connection, targetId, comparatorId, outcomeId, analysisId, databaseId) {
  result <- attrition[attrition$targetId == targetId &
                        attrition$comparatorId == comparatorId &
                        attrition$outcomeId == outcomeId &
                        attrition$analysisId == analysisId &
                        attrition$databaseId == databaseId, ]
  targetAttrition <- result[result$exposureId == targetId, ]
  comparatorAttrition <- result[result$exposureId == comparatorId, ]
  colnames(targetAttrition)[colnames(targetAttrition) == "subjects"] <- "targetPersons"
  targetAttrition$exposureId <- NULL
  colnames(comparatorAttrition)[colnames(comparatorAttrition) == "subjects"] <- "comparatorPersons"
  comparatorAttrition$exposureId <- NULL
  result <- merge(targetAttrition, comparatorAttrition)
  result <- result[order(result$sequenceNumber), ]
  return(result)
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

getPropensityModel <- function(connection, targetId, comparatorId, analysisId, databaseId) {
  model <- propensityModel[propensityModel$targetId == targetId &
                             propensityModel$comparatorId == comparatorId &
                             propensityModel$analysisId == analysisId &
                             propensityModel$databaseId == databaseId, ]
  covariateSubset <- covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId, 
                               c("covariateId", "covariateName")]
  covariateSubset <- rbind(covariateSubset, 
                           data.frame(covariateId = 0,
                                      covariateName = "Intercept"))
  model <- merge(model, covariateSubset)
  model <- model[, c("coefficient", "covariateId", "covariateName")]
  return(model)
}

getCovariateBalanceSummary <- function(connection, targetId, comparatorId, analysisId,
                                       beforeLabel = "Before matching",
                                       afterLabel = "After matching") {
  
  balance <- getCovariateBalance(connection = connection,
                                 targetId = targetId,
                                 comparatorId = comparatorId,
                                 analysisId = analysisId,
                                 outcomeId = NULL)
  balanceBefore <- balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = quantile(.data$beforeMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceBefore[, "type"] <- beforeLabel
  balanceAfter <-  balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = quantile(.data$afterMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceAfter[, "type"] <- afterLabel
  
  balanceSummary <- rbind(balanceBefore, balanceAfter) %>%
    dplyr::ungroup()
  
  return(balanceSummary)
  
}

getNegativeControlEstimates <- function(connection, targetId, comparatorId, analysisId) {
  subset <- getControlResults(connection, targetId, comparatorId, analysisId, includePositiveControls = FALSE)
  subset <- subset[, c("databaseId", "logRr", "seLogRr")]

  # subset <- merge(cohortMethodResult, negativeControlOutcome, by = "outcomeId")
  # # subset <- with(subset, subset[(subset$targetId == targetId &
  # #                      subset$comparatorId == comparatorId &
  # #                      subset$analysisId == analysisId), c("databaseId", "logRr", "seLogRr")])
  # subset <- subset[subset$targetId == targetId &
  #                                  subset$comparatorId == comparatorId &
  #                                  subset$analysisId == analysisId, c("databaseId", "logRr", "seLogRr")]
  # # subset <- cohortMethodResult %>%
  # #   dplyr::inner_join(negativeControlOutcome, by = "outcomeId") %>%
  # #   dplyr::filter(.data$targetId == targetId &
  # #            .data$comparatorId == comparatorId &
  # #            .data$analysisId == analysisId) %>%
  # #   dplyr::select(.data$databaseId, .data$logRr, .data$seLogRr)
  # 
  # getControlResults()
  if(nrow(subset) == 0)
    return(NULL)
  return(subset)
}
