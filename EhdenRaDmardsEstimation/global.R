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
    if ("databaseId" %in% colnames(newData)) {
      newData$databaseId[newData$databaseId == "Amb_EMR"] <- "AmbEMR"
      newData$databaseId[newData$databaseId == "BELGIUM"] <- "DABelgium"
      newData$databaseId[newData$databaseId == "GERMANY"] <- "DAGermany"
      newData$databaseId[newData$databaseId == "IPCI-HI-LARIOUS-RA"] <- "ICPI"
      newData$databaseId[newData$databaseId == "LPDFRANCE"] <- "LPDFrance"
      newData$databaseId[newData$databaseId == "Optum"] <- "ClinFormatics"
      newData$databaseId[newData$databaseId == "PanTher"] <- "OptumEHR"
    }
    if ("sources" %in% colnames(newData)) {
      newData$sources <- gsub("Amb_EMR", "AmbEMR", newData$sources)
      newData$sources <- gsub("GERMANY", "DAGermany", newData$sources)
      newData$sources <- gsub("Optum", "ClinFormatics", newData$sources)
      newData$sources <- gsub("PanTher", "OptumEHR", newData$sources)
    }
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

source("dataClean.R")