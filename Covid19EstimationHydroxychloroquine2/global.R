source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "data", blind = FALSE)

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



# data clean -----------------------------------------------------------------------------------------
outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

cohortMethodResult$i2 <- round(cohortMethodResult$i2, 2)

exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI-Covid19] Hydroxychloroquine + Azithromycin"] <- "Hydroxychloroquine + Azithromycin with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI Cov19] New users of Hydroxychloroquine with prior rheumatoid arthritis"] <- "Hydroxychloroquine with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI-Covid19] Hydroxychloroquine + Amoxicillin"] <- "Hydroxychloroquine + Amoxicillin with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI Cov19] New users of sulfasazine with prior rheumatoid arthritis"] <- "Sulfasalazine with prior RA"
exposureOfInterest <- exposureOfInterest[order(exposureOfInterest$exposureId), ]

cohortMethodAnalysis$description[cohortMethodAnalysis$description == "No prior outcome in last 30d, 5 PS strata, TAR on-treatment+14d"] <- "5 PS strata, on-treatment + 14 days follow-up"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "No prior outcome in last 30d, 5 PS strata, TAR 30d fixed"] <- "5 PS strata, 30 days follow-up"
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$analysisId, decreasing = TRUE), ]

dbOrder <- c("AmbEMR", "CCAE", "Clinformatics", "CPRD", "DAGermany", "IMRD", "MDCD", "MDCR", "OpenClaims", "OptumEHR", "Meta-analysis")
database$dbOrder <- match(database$databaseId, dbOrder)
database <- database[order(database$dbOrder), ]
database$dbOrder <- NULL
