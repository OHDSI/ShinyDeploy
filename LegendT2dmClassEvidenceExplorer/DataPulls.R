getExposureName <- function(connection, exposureId) {
  sql <- "SELECT exposure_name FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
  UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
  sql <- SqlRender::render(sql, exposure_id = exposureId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  exposureName <- DatabaseConnector::querySql(connection, sql)
  return(exposureName[1, 1])
}

getExposureDescription <- function(connection, exposureId) {
  sql <- "SELECT description FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
  UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
  sql <- SqlRender::render(sql, exposure_id = exposureId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  exposureDescription <- DatabaseConnector::querySql(connection, sql)
  return(exposureDescription[1, 1])
}

getOutcomeName <- function(connection, outcomeId) {
  sql <- "SELECT outcome_name FROM outcome_of_interest WHERE outcome_id = @outcome_id"
  sql <- SqlRender::render(sql, outcome_id = outcomeId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  outcomeName <- DatabaseConnector::querySql(connection, sql)
  return(outcomeName[1, 1])
}

getIndications <- function(connection) {
  sql <- "SELECT indication_id, indication_name FROM indication"
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  indications <- DatabaseConnector::querySql(connection, sql)
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
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  subgroups <- DatabaseConnector::querySql(connection, sql)
  colnames(subgroups) <- SqlRender::snakeCaseToCamelCase(colnames(subgroups))
  subgroups$subgroupName <- gsub("Subgroup: ", "", subgroups$subgroupName)
  return(subgroups)
}


getExposures <- function(connection) {
  sql <- "SELECT exposure_id, exposure_name FROM exposure_of_interest"
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  exposures <- DatabaseConnector::querySql(connection, sql)
  colnames(exposures) <- SqlRender::snakeCaseToCamelCase(colnames(exposures))
  return(exposures)
}

getOutcomes <- function(connection) {
  sql <- "SELECT outcome_id, outcome_name FROM outcome_of_interest"
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  outcomes <- DatabaseConnector::querySql(connection, sql)
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  return(outcomes)
}

getAnalyses <- function(connection) {
  sql <- "SELECT analysis_id, description FROM cohort_method_analysis"
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  analyses <- DatabaseConnector::querySql(connection, sql)
  colnames(analyses) <- SqlRender::snakeCaseToCamelCase(colnames(analyses))
  return(analyses)
}

getDatabases <- function(connection) {
  sql <- "SELECT * FROM database"
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  databases <- DatabaseConnector::querySql(connection, sql)
  colnames(databases) <- SqlRender::snakeCaseToCamelCase(colnames(databases))
  return(databases)
}

getDatabaseDetails <- function(connection, databaseId) {
  sql <- "SELECT * FROM database WHERE database_id = '@database_id'"
  sql <- SqlRender::render(sql, database_id = databaseId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  databaseDetails <- DatabaseConnector::querySql(connection, sql)
  colnames(databaseDetails) <- SqlRender::snakeCaseToCamelCase(colnames(databaseDetails))
  databaseDetails$description <- sub("\\n", " ", databaseDetails$description)
  databaseDetails$description <- sub("JDMC", "JMDC", databaseDetails$description) # TODO Fix in schema
  return(databaseDetails)
}

getIndicationForExposure <- function(connection,
                                     exposureIds = c()) {
  sql <- "SELECT exposure_id, indication_id FROM single_exposure_of_interest WHERE"
  sql <- paste(sql, paste0("exposure_id IN (", paste(exposureIds, collapse = ", "), ")"))

  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  indications <- DatabaseConnector::querySql(connection, sql)
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
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  tcoDbs <- DatabaseConnector::querySql(connection, sql)
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
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  tcoDbs <- DatabaseConnector::querySql(connection, sql)
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

  if (is.null(connection)) {
    results <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                    cohortMethodResult$comparatorId == comparatorId &
                                    cohortMethodResult$analysisId == analysisId, ]
    if (!is.null(databaseId))
      results <- results[results$databaseId == databaseId, ]

    results$effectSize <- NA
    idx <- results$outcomeId %in% negativeControlOutcome$outcomeId
    results$effectSize[idx] <- 1
    # if (!is.null(positiveControlOutcome) && includePositiveControls) {
    #   idx <- results$outcomeId %in% positiveControlOutcome$outcomeId
    #   results$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
    #                                                                      positiveControlOutcome$outcomeId)]
    # }

    results <- results[!is.na(results$effectSize), ]
  } else {
      sql <- "SELECT *
        FROM cohort_method_result
        INNER JOIN (
          SELECT outcome_id,
            outcome_name,
            CAST(1 AS FLOAT) AS effect_size
          FROM negative_control_outcome
        ) outcomes
      ON cohort_method_result.outcome_id = outcomes.outcome_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND database_id = '@database_id'
      AND analysis_id = @analysis_id"
      sql <- SqlRender::render(sql,
                               target_id = targetId,
                               comparator_id = comparatorId,
                               database_id = databaseId,
                               analysis_id = analysisId)
      results <- DatabaseConnector::querySql(connection, sql)
      colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))

  }
  return(results)
}

getCmFollowUpDist <- function(connection,
                              targetId,
                              comparatorId,
                              outcomeId,
                              databaseId,
                              analysisId) {
  if (is.null(connection)) {
    followUpDist <- cmFollowUpDist[cmFollowUpDist$targetId == targetId &
                                     cmFollowUpDist$comparatorId == comparatorId &
                                     cmFollowUpDist$outcomeId == outcomeId &
                                     cmFollowUpDist$analysisId == analysisId &
                                     cmFollowUpDist$databaseId == databaseId, ]
  } else { # use connection
    sql <- "
      SELECT target_min_days,
        target_p10_days,
        target_p25_days,
        target_median_days,
        target_p75_days,
        target_p90_days,
        target_max_days,
        target_zero_days,
        comparator_min_days,
        comparator_p10_days,
        comparator_p25_days,
        comparator_median_days,
        comparator_p75_days,
        comparator_p90_days,
        comparator_max_days,
        comparator_zero_days
      FROM cm_follow_up_dist
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
                             analysis_id = analysisId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    followUpDist <- DatabaseConnector::querySql(connection, sql)
    colnames(followUpDist) <- SqlRender::snakeCaseToCamelCase(colnames(followUpDist))
  }
  return(followUpDist)
}

getCovariateBalance <- function(connection,
                                targetId,
                                comparatorId,
                                analysisId,
                                databaseId = NULL,
                                outcomeId = NULL) {

  if (is.null(connection)) {
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
    if (!is.null(outcomeId)) {
      balance <- balance[balance$outcomeId == outcomeId, ]
    } else {
      balance <- balance[is.na(balance$outcomeId), ]
    }

    if (!is.null(databaseId))
      balance <- merge(balance, covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId,
                                          c("covariateId", "covariateAnalysisId", "covariateName")])
    else
      balance <- merge(balance, covariate[covariate$analysisId == analysisId,
                                          c("covariateId", "covariateAnalysisId", "covariateName")])
    balance <- balance[ c("databaseId",
                          "covariateId",
                          "covariateName",
                          "analysisId",
                          "targetMeanBefore",
                          "comparatorMeanBefore",
                          "stdDiffBefore",
                          "targetMeanAfter",
                          "comparatorMeanAfter",
                          "stdDiffAfter")]
  } else { # use connection

    if (is.null(outcomeId)) {
      outcomeId <- 0
    }

    sql <- "
      SELECT covariate.database_id, covariate.covariate_id, covariate_name, covariate_analysis_id,
        target_mean_before, comparator_mean_before, std_diff_before,
        target_mean_after, comparator_mean_after, std_diff_after
      FROM covariate_balance
      INNER JOIN covariate
      ON covariate_balance.covariate_id = covariate.covariate_id
      AND covariate_balance.database_id = covariate.database_id
      AND covariate_balance.analysis_id = covariate.analysis_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND covariate.database_id IN (@database_id)
      AND covariate.analysis_id = @analysis_id
      AND outcome_id = @outcome_id"
    sql <- SqlRender::render(sql,
                             target_id = targetId,
                             comparator_id = comparatorId,
                             database_id = paste0("'",
                                                  paste0(databaseId, collapse = "', '"),
                                                  "'"),
                             # database_id = databaseId,
                             analysis_id = analysisId,
                             outcome_id = outcomeId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    balance <- DatabaseConnector::querySql(connection, sql)
  }

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

  if (is.null(connection)) {
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
  } else { # use connection
    sql <- "SELECT database_id,
      target_id,
      comparator_id,
      preference_score,
      target_density,
      comparator_density
      FROM preference_score_dist
      WHERE target_id IN (@target_ids)
      AND comparator_id IN (@comparator_ids)
      {@database_id != \"\"} ? {AND database_id = '@database_id'};"
    sql <- SqlRender::render(sql,
                             target_ids = targetIds,
                             comparator_ids = comparatorIds,
                             database_id = databaseId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    ps <- DatabaseConnector::querySql(connection, sql)
    colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
    if (databaseId != "") {
      ps$databaseId <- NULL
    }
  }
  return(ps)
}

getKaplanMeier <- function(connection, targetId, comparatorId, outcomeId, databaseId, analysisId) {
  
  if(is.null(connection)){
    # reading from local results folder
    file <- sprintf("kaplan_meier_dist_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
    km <- readRDS(file.path(dataFolder, file))
    colnames(km) <- SqlRender::snakeCaseToCamelCase(colnames(km))
    km <- km[km$outcomeId == outcomeId &
               km$analysisId == analysisId, ]
  }else{
    sql <- "SELECT * FROM kaplan_meier_dist
         WHERE (target_id = @target_id) AND
          (comparator_id = @comparator_id) AND
          (analysis_id = @analysis_id) AND
          (outcome_id = @outcome_id) AND
          (database_id = '@database_id');"
    sql <- SqlRender::render(sql,
                             target_id = targetId,
                             comparator_id = comparatorId,
                             outcome_id = outcomeId,
                             database_id = databaseId,
                             analysis_id = analysisId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    km <- DatabaseConnector::querySql(connection, sql)
    names(km) <- SqlRender::snakeCaseToCamelCase(names(km))
  }
  
  return(km)
}

getAttrition <- function(connection, targetId, comparatorId, outcomeId, analysisId, databaseId) {

  if (is.null(connection)) {
    result <- attrition[attrition$targetId == targetId &
                          attrition$comparatorId == comparatorId &
                          attrition$outcomeId == outcomeId &
                          attrition$analysisId == analysisId &
                          attrition$databaseId == databaseId, ]
  } else { # use connection
    sql <- "
      SELECT exposure_id,
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
    sql <- SqlRender::render(sql,
                             target_id = targetId,
                             comparator_id = comparatorId,
                             outcome_id = outcomeId,
                             analysis_id = analysisId,
                             database_id = databaseId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(connection, sql)
    colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  }

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
  sql <- SqlRender::render(sql,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           database_id = databaseId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  studyPeriod <- DatabaseConnector::querySql(connection, sql)
  colnames(studyPeriod) <- SqlRender::snakeCaseToCamelCase(colnames(studyPeriod))
  return(studyPeriod)
}

getPropensityModel <- function(connection, targetId, comparatorId, analysisId, databaseId) {

  if (is.null(connection)) {
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
  } else { # use connection
    sql <- "
      SELECT
        coefficient,
        propensity_model.covariate_id,
        covariate_name
      FROM propensity_model
      INNER JOIN covariate
      ON propensity_model.covariate_id = covariate.covariate_id
      AND propensity_model.database_id = covariate.database_id
      AND propensity_model.analysis_id = covariate.analysis_id
      WHERE target_id = @target_id
      AND comparator_id = @comparator_id
      AND propensity_model.analysis_id = @analysis_id
      AND propensity_model.database_id = '@database_id'"

    sql <- SqlRender::render(sql,
                             target_id = targetId,
                             comparator_id = comparatorId,
                             analysis_id = analysisId,
                             database_id = databaseId)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    model <- DatabaseConnector::querySql(connection, sql)
    colnames(model) <- SqlRender::snakeCaseToCamelCase(colnames(model))
  }

  model <- model[order(abs(model$coefficient), decreasing = TRUE), ]
  return(model)
}

getCovariateBalanceSummary <- function(connection, targetId, comparatorId, analysisId,
                                       databaseIds,
                                       beforeLabel = "Before matching",
                                       afterLabel = "After matching") {

  balance <- getCovariateBalance(connection = connection,
                                 targetId = targetId,
                                 comparatorId = comparatorId,
                                 analysisId = analysisId,
                                 databaseId = databaseIds,
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

loadResultsTable <- function(tableName, required = TRUE) {
  if (required || tableName %in% resultsTablesOnServer) {
    tryCatch({
      table <- DatabaseConnector::dbReadTable(connectionPool,
                                              paste(resultsDatabaseSchema, tableName, sep = "."))
    }, error = function(err) {
      stop(
        "Error reading from ",
        paste(resultsDatabaseSchema, tableName, sep = "."),
        ": ",
        err$message
      )
    })
    colnames(table) <-
      SqlRender::snakeCaseToCamelCase(colnames(table))
    if (nrow(table) > 0) {
      assign(
        SqlRender::snakeCaseToCamelCase(tableName),
        dplyr::as_tibble(table),
        envir = .GlobalEnv
      )
    }
  }
}
