source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "./data", blind = FALSE)
dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

load(file.path(dataFolder, "PreMergedShinyData.RData"))
tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]

# data clean
metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]
outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]
exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]
cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]
cohortMethodResult$ci95Ub <- as.numeric(cohortMethodResult$ci95Ub)

# blinding
mdrr <- readRDS("mdrr.rds")
asmd <- readRDS("asmd.rds")
equipoise <- readRDS("equipoise.rds")
cohortMethodResult <- merge(cohortMethodResult, mdrr, all.x = TRUE)
cohortMethodResult <- merge(cohortMethodResult, asmd, all.x = TRUE)
cohortMethodResult <- merge(cohortMethodResult, equipoise, all.x = TRUE)
rm(mdrr, asmd, equipoise)

blinds <- cohortMethodResult$mdrrPass == 1 & cohortMethodResult$asmdPass == 1 & cohortMethodResult$equipoisePass == 1
cohortMethodResult$rr[!blinds] <- NA
cohortMethodResult$ci95Ub[!blinds] <- NA
cohortMethodResult$ci95Lb[!blinds] <- NA
cohortMethodResult$logRr[!blinds] <- NA
cohortMethodResult$seLogRr[!blinds] <- NA
cohortMethodResult$p[!blinds] <- NA
cohortMethodResult$calibratedRr[!blinds] <- NA
cohortMethodResult$calibratedCi95Ub[!blinds] <- NA
cohortMethodResult$calibratedCi95Lb[!blinds] <- NA
cohortMethodResult$calibratedLogRr[!blinds] <- NA
cohortMethodResult$calibratedSeLogRr[!blinds] <- NA
cohortMethodResult$calibratedP[!blinds] <- NA

# os <- cohortMethodResult$outcomeId %in% outcomeOfInterest$outcomeId
# asmdPassed <- cohortMethodResult[os & cohortMethodResult$asmdPass == 1,
#                                  c("targetId", "comparatorId", "outcomeId", "analysisId", "databaseId", "mdrrPass", "asmdPass", "equipoisePass")]
# 
# asmdPassed <- merge(asmdPassed, exposureOfInterest, by.x = "targetId", by.y = "exposureId")
# asmdPassed <- dplyr::rename(asmdPassed, targetName = exposureName)
# asmdPassed <- merge(asmdPassed, exposureOfInterest, by.x = "comparatorId", by.y = "exposureId")
# asmdPassed <- dplyr::rename(asmdPassed, comparatorName = exposureName)
# asmdPassed <- merge(asmdPassed, outcomeOfInterest)
# asmdPassed <- merge(asmdPassed, cohortMethodAnalysis)
# asmdPassed <- asmdPassed[, c("databaseId","analysisId", "description",
#                              "targetId", "targetName", "comparatorId", "comparatorName",
#                              "outcomeId", "outcomeName", "mdrrPass", "asmdPass", "equipoisePass")]
# write.csv(asmdPassed, "asmdPassed.csv", row.names = FALSE)










