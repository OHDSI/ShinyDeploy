source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "./data", blind = TRUE)
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