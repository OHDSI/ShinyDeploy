source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "./data", blind = FALSE)
dataFolder <- shinySettings$dataFolder
#dataFolder <- "G:/StudyResults/mskai/shinyData"

blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

load(file.path(dataFolder, "PreMergedShinyData.RData"))
tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]

outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]
exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]
cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

rm(cohortMethodResult)
cohortMethodResult <- readRDS(file.path(dataFolder, "cohortMethodResultCal.rds"))
cohortMethodResult$ci95Ub <- as.numeric(cohortMethodResult$ci95Ub)
cohortMethodResult$sources <- ""

cohortMethodResultMa <- readRDS(file.path(dataFolder, "cohort_method_result_Meta-analysis.rds"))
cohortMethodResult <- rbind(cohortMethodResult, cohortMethodResultMa)

databaseMa <- readRDS(file.path(dataFolder, "database_Meta-analysis.rds"))
names(databaseMa) <- SqlRender::snakeCaseToCamelCase(names(databaseMa))
database <- rbind(database, databaseMa)
metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]