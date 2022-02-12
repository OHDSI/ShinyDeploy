source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "G:/OutcomeMisclassificationEval/mergedShinyDataFolder")
#shinySettings <- list(dataFolder = "./data")
dataFolder <- shinySettings$dataFolder
connection <- NULL

splittableTables <- c("covariate_balance", "preference_score_dist")
files <- list.files(dataFolder, pattern = ".rds")
idxDrop1 <- grep("covariate_balance", files)
idxDrop2 <- grep("preference_score_dist", files)
files <- files[-c(idxDrop1, idxDrop2)]

# Find part to remove from all file names (usually databaseId):
databaseFileName <- files[grepl("^database", files)]
removeParts <- paste0(gsub("database", "", databaseFileName), "$")
removePart <- removeParts

# Remove data already in global environment:
tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)])) 
camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
camelCaseNames <- unique(camelCaseNames)
camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
suppressWarnings(
  rm(list = camelCaseNames)
)

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
dummy <- lapply(files[grepl(removePart, files)], loadFile, removePart)

tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
               
source("DataClean.R")