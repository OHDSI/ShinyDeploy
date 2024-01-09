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


getControlResults <- function(connection,
                              targetId,
                              comparatorId,
                              analysisId,
                              databaseId) {
  results <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                  cohortMethodResult$comparatorId == comparatorId &
                                  cohortMethodResult$analysisId == analysisId &
                                  cohortMethodResult$databaseId == databaseId, ]
  results$effectSize <- NA
  idx <- results$outcomeId %in% negativeControlOutcome$outcomeId
  results$effectSize[idx] <- 1
  # if (!is.null(positiveControlOutcome)) {
  #   idx <- results$outcomeId %in% positiveControlOutcome$outcomeId
  #   results$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
  #                                                                      positiveControlOutcome$outcomeId)]
  # }
  results <- results[!is.na(results$effectSize), ]
  return(results)
}


getCmFollowUpDist <- function(connection,
                              targetId,
                              comparatorId,
                              outcomeId,
                              databaseId,
                              analysisId) {
  followUpDist <- cmFollowUpDist[cmFollowUpDist$targetId == targetId &
                                   cmFollowUpDist$comparatorId == comparatorId &
                                   cmFollowUpDist$outcomeId == outcomeId &
                                   cmFollowUpDist$analysisId == analysisId &
                                   cmFollowUpDist$databaseId == databaseId, ]
  return(followUpDist)
}


getCovariateBalance <- function(connection,
                                targetId,
                                comparatorId,
                                databaseId,
                                analysisId,
                                outcomeId = NULL) {
  file <- sprintf("covariate_balance_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
  file <- file.path(dataFolder, file)
  if (!file.exists(file)) {
    return(NULL)
  } else {
    balance <- readRDS(file)
    colnames(balance) <- SqlRender::snakeCaseToCamelCase(colnames(balance))
    balance <- balance[balance$analysisId == analysisId & balance$outcomeId == outcomeId, ]
    balance <- merge(balance, covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId,
                                        c("covariateId", "covariateAnalysisId", "covariateName")])
    balance <- balance[ c("covariateId",
                          "covariateName",
                          "covariateAnalysisId",
                          "targetMeanBefore",
                          "comparatorMeanBefore",
                          "stdDiffBefore",
                          "targetMeanAfter",
                          "comparatorMeanAfter",
                          "stdDiffAfter")]
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
}


getPs <- function(connection,
                  targetIds,
                  comparatorIds,
                  analysisId,
                  databaseId) {
  file <- sprintf("preference_score_dist_t%s_c%s_%s.rds", targetIds, comparatorIds, databaseId)
  filePath <- file.path(dataFolder, file)
  if (!file.exists(filePath)) {
    return(NULL)
  }
  ps <- readRDS(filePath)
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  ps <- ps[ps$analysisId == analysisId, ]
  return(ps)
}


getKaplanMeier <- function(connection,
                           targetId,
                           comparatorId,
                           outcomeId,
                           databaseId,
                           analysisId) {
  file <- sprintf("kaplan_meier_dist_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
  km <- readRDS(file.path(dataFolder, file))
  colnames(km) <- SqlRender::snakeCaseToCamelCase(colnames(km))
  km <- km[km$outcomeId == outcomeId &
             km$analysisId == analysisId, ]

  return(km)
}


getAttrition <- function(connection,
                         targetId,
                         comparatorId,
                         outcomeId,
                         analysisId,
                         databaseId) {
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


getPropensityModel <- function(connection,
                               targetId,
                               comparatorId,
                               analysisId,
                               databaseId) {
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

getTaBalance <- function(connection,
                         targetId,
                         comparatorId,
                         analysisId,
                         databaseId) {
  taBal <- taBalance[taBalance$targetId == targetId &
                       taBalance$comparatorId == comparatorId &
                       taBalance$analysisId == analysisId &
                       taBalance$databaseId == databaseId, ]
  return(taBal)
}
