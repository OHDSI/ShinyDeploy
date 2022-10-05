os <- cohortMethodResult$outcomeId %in% outcomeOfInterest$outcomeId
rows <- unique(cohortMethodResult[os, c("databaseId", "targetId", "comparatorId", "outcomeId", "analysisId")])
rowsList <- split(rows, seq(nrow(rows)))

# mdrr
alpha <- 0.05
power <- 0.8
z1MinAlpha <- qnorm(1 - alpha/2)
zBeta <- -qnorm(1 - power)
pA <- cohortMethodResult$targetSubjects/(cohortMethodResult$targetSubjects + cohortMethodResult$comparatorSubjects)
pB <- 1 - pA
totalEvents <- abs(cohortMethodResult$targetOutcomes) + abs(cohortMethodResult$comparatorOutcomes)
cohortMethodResult$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
mdrr <- merge(rows, cohortMethodResult[, c("databaseId", "targetId", "comparatorId", "outcomeId", "analysisId", "mdrr")])
mdrr$mdrrPass <- ifelse(mdrr$mdrr < 3, 1, 0)
saveRDS(mdrr, "mdrr.rds")

# asmd
getAsmd <- function(row) {
  balance <- getCovariateBalance(connection = connection,
                                 targetId = row$targetId,
                                 comparatorId = row$comparatorId,
                                 databaseId = row$databaseId,
                                 analysisId = row$analysisId,
                                 outcomeId = row$outcomeId)
  maxAsmd <- max(balance$absAfterMatchingStdDiff)
  row$asmdPass <- ifelse(maxAsmd < 0.1, 1, 0)
  return(row)
}
asmd <- lapply(rowsList, getAsmd) # time consuming 
asmd <- dplyr::bind_rows(asmd)
saveRDS(asmd, "asmd.rds")

# equipoise
getEquipoise <- function(row) { # row <- rowsList[[1]]
  patientCounts <- getAttrition(connection = connection,
                                targetId = row$targetId,
                                comparatorId = row$comparatorId,
                                outcomeId = row$outcomeId,
                                analysisId = row$analysisId,
                                databaseId = row$databaseId)
  targetSize <- patientCounts$targetPersons[patientCounts$description == "Have at least 1 days at risk"]
  comparatorSize <- patientCounts$comparatorPersons[patientCounts$description == "Have at least 1 days at risk"]
  ps <- getPs(connection = connection,
              targetIds = row$targetId,
              comparatorIds = row$comparatorId,
              analysisId = row$analysisId,
              databaseId = row$databaseId)
  if (nrow(ps) == 0) {
    row$inEquipoise <- NA
    row$equipoisePass <- NA
    return(row)
  } else {
    psFiltered <- ps[ps$preferenceScore >= 0.3 & ps$preferenceScore <= 0.7, ]
    targetFraction <- sum(psFiltered$targetDensity) / sum(ps$targetDensity)
    comparatorFraction <- sum(psFiltered$comparatorDensity) / sum(ps$comparatorDensity)
    totalFraction <- (targetFraction * targetSize + comparatorFraction * comparatorSize) / (targetSize + comparatorSize)
    row$inEquipoise <- totalFraction
    row$equipoisePass <- ifelse(totalFraction > 0.5, 1, 0)
    return(row)
  }
}
equipoise <- lapply(rowsList, getEquipoise) # time consuming
equipoise <- dplyr::bind_rows(equipoise)
saveRDS(equipoise, "equipoise.rds")

