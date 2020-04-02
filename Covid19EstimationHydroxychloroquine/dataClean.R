outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

cohortMethodResult$i2 <- round(cohortMethodResult$i2, 2)

drops <- cohortMethodResult$databaseId == "PanTher" & cohortMethodResult$analysisId == 1
cohortMethodResult <- cohortMethodResult[!drops, ]

database$order <- match(database$databaseId, c(database$databaseId[database$databaseId != "Meta-analysis"], "Meta-analysis"))
database <- database[order(database$order), ]
database$order <- NULL

blinds <-
  (cohortMethodResult$databaseId == "CPRD" & cohortMethodResult$targetId == 137) |
  (cohortMethodResult$databaseId == "JMDC" & cohortMethodResult$targetId == 2) |
  (cohortMethodResult$databaseId == "DA_GERMANY" & cohortMethodResult$targetId == 137)

cohortMethodResult$rr[blinds] <- NA
cohortMethodResult$ci95Ub[blinds] <- NA
cohortMethodResult$ci95Lb[blinds] <- NA
cohortMethodResult$logRr[blinds] <- NA
cohortMethodResult$seLogRr[blinds] <- NA
cohortMethodResult$p[blinds] <- NA
cohortMethodResult$calibratedRr[blinds] <- NA
cohortMethodResult$calibratedCi95Ub[blinds] <- NA
cohortMethodResult$calibratedCi95Lb[blinds] <- NA
cohortMethodResult$calibratedLogRr[blinds] <- NA
cohortMethodResult$calibratedSeLogRr[blinds] <- NA
cohortMethodResult$calibratedP[blinds] <- NA