source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "./data", blind = FALSE)

dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")

files <- list.files(dataFolder, pattern = ".rds")

# Find part to remove from all file names (usually databaseId):
databaseFileName <- files[grepl("^database", files)]
removeParts <- paste0(gsub("database", "", databaseFileName), "$")

# Remove data already in global environment:
for (removePart in removeParts) {
  tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)])) 
  camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
  camelCaseNames <- unique(camelCaseNames)
  camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
  suppressWarnings(
    rm(list = camelCaseNames)
  )
}

# Load data from data folder. R data objects will get names derived from the filename:
loadFile <- function(file, removePart) {
  tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file)) 
  camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
  if (!(tableName %in% splittableTables)) {
    newData <- readRDS(file.path(dataFolder, file))
    colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
    if (exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      newData <- rbind(existingData, newData)
      newData <- unique(newData)
    }
    assign(camelCaseName, newData, envir = .GlobalEnv)
  }
  invisible(NULL)
}
for (removePart in removeParts) {
  lapply(files[grepl(removePart, files)], loadFile, removePart)
}

tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]

outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

keeps <- ((cohortMethodResult$outcomeId %in% c(187, 193, 197, 203, 253) & cohortMethodResult$analysisId %in% c(7:10)) | # infections, leukopenia, pancytopenia
  (cohortMethodResult$outcomeId %in% c(182:185) & cohortMethodResult$analysisId %in% c(1, 2, 4, 5)) | # cardiovacular
  (cohortMethodResult$outcomeId %in% c(212, 223, 216, 218, 201) & cohortMethodResult$analysisId %in% c(3, 6)) | # oncology
  cohortMethodResult$outcomeId %in% negativeControlOutcome$outcomeId) & # controls
  cohortMethodResult$comparatorId == 219 # keep only methotrexate as comparator
cohortMethodResult <- cohortMethodResult[keeps, ]

toBlind <- readRDS(file.path(dataFolder, "to_blind.rds"))
toBlind <- toBlind[, c("database_id", "analysis_id", "target_id", "comparator_id")] 
toBlind$to_blind <- 1
colnames(toBlind) <- SqlRender::snakeCaseToCamelCase(colnames(toBlind))

cohortMethodResult <- merge(cohortMethodResult, toBlind, all.x = TRUE)
cohortMethodResult$toBlind[is.na(cohortMethodResult$toBlind)] <- 0

dbBlinds <- cohortMethodResult$databaseId %in% c("BELGIUM", "GERMANY", "THIN", "PanTher", "IPCI-HI-LARIOUS-RA") & cohortMethodResult$analysisId %in% c(1,4,7,9)

# create toBlind reference to null KM plots

cohortMethodResult$rr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$ci95Ub[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$ci95Lb[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$logRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$seLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$p[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedCi95Ub[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedCi95Lb[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedSeLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedP[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA

cohortMethodResult$i2 <- round(cohortMethodResult$i2, 2)

source("dataClean.R")