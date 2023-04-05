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

getValidationResults <- function(connection,
                                 targetId,
                                 comparatorId,
                                 outcomeId,
                                 databaseIds = c()) {
  targetName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == targetId]
  targetExposure <- sub(" .*", "", targetName)
  comparatorName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparatorId]
  comparatorExposure <- sub(" .*", "", comparatorName)
  result <- validationResult[validationResult$databaseId %in% databaseIds &
                               validationResult$outcomeCohortId == outcomeId &
                               (validationResult$exposureCohort %in% c(targetName, targetExposure, comparatorName, comparatorExposure) | is.na(validationResult$exposureCohort)), ]
  result <- result[, c("databaseId",
                       "exposureCohort",
                       #"estimatedPrevalence",
                       "sens", "spec", "ppv", "npv",
                       "TP", "FP", "FN", "TN")]
  names(result)[names(result) == "exposureCohort"] <- "strata"
  result$strata[is.na(result$strata)] <- "Database population"
  return(result)
}

getForestPlotData <- function(connection,
                              targetIds = c(),
                              comparatorIds = c(),
                              outcomeIds = c(),
                              analysisIds = c(),
                              databaseIds = c()) {
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

getCovariateBalance <- function(connection,
                                targetId,
                                comparatorId,
                                analysisId,
                                databaseId = NULL,
                                outcomeId = NULL) {
  
  file <- sprintf("covariate_balance_%s.rds", databaseId)
  balance <- readRDS(file.path(dataFolder, file))
  
  colnames(balance) <- SqlRender::snakeCaseToCamelCase(colnames(balance))
  balance <- balance[balance$targetId == targetId &
                       balance$comparatorId == comparatorId &
                       balance$analysisId == analysisId & 
                       balance$outcomeId == outcomeId, ]
  balance <- merge(balance, covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId,
                                      c("covariateId", "covariateAnalysisId", "covariateName")])

  balance <- balance[ c("databaseId",
                        "analysisId",
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
                         "cmAnalysisId",
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
  if(analysisId %in% c(1, 2, 5, 6, 9, 10)) {
    balance[, c("afterMatchingMeanTreated", "afterMatchingMeanComparator", "afterMatchingStdDiff")] <- NA
  }
  return(balance)
}

getPs <- function(connection,
                  targetId,
                  comparatorId,
                  analysisId,
                  databaseId = "") {
  if (databaseId != "") {
    file <- sprintf("preference_score_dist_%s.rds", databaseId)
    fileName <- file.path(dataFolder, file) 
    if (!file.exists(fileName)) {
      return(NULL)
    }
    ps <- readRDS(fileName)
  }
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  ps <- ps[ps$analysisId == analysisId &
             ps$targetId == targetId &
             ps$comparatorId == comparatorId, ]
  if (databaseId != "") {
    ps$databaseId <- NULL
  }
  return(ps)
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

getContourData <- function(connection,
                           contourData,
                           incidence,
                           or) {
  result <- contourData[contourData$incidence %in% incidence & contourData$or %in% or, ]
  return(result)
}