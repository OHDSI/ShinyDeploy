source("DataPulls.R")
source("PlotsAndTables.R")

shinySettings <- list(dataFolder = "./data", blind = FALSE)

dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")

files <- list.files(dataFolder, pattern = ".rds")

# Remove data already in global environment:
tableNames <- gsub("(_t[0-9]+_c[0-9]+)*\\.rds", "", files) 
camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
camelCaseNames <- unique(camelCaseNames)
camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
rm(list = camelCaseNames)

# Load data from data folder:
loadFile <- function(file) {
  # file = files[8]
  tableName <- gsub("(_t[0-9]+_c[0-9]+)*\\.rds", "", file) 
  camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
  if (!(tableName %in% splittableTables)) {
    newData <- readRDS(file.path(dataFolder, file))
    colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
    if (exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      newData <- rbind(existingData, newData)
    }
    assign(camelCaseName, newData, envir = .GlobalEnv)
  }
  invisible(NULL)
}
lapply(files, loadFile)

tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]

relabel <- function(var, oldLabel, newLabel) {
  levels(var)[levels(var) == oldLabel] <- newLabel
  return(var)
}

analysisOrder <- c(1, 3, 5, 2, 4, 6)
cohortMethodAnalysis$analysisOrder <- match(cohortMethodAnalysis$analysisId, analysisOrder)
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$analysisOrder), ]

# drop <65 population for this report
exposureOfInterest <- exposureOfInterest[!(exposureOfInterest$exposureId %in% c(8173, 8174, 8571, 8572, 8176, 8573)), ]
database <- database[database$databaseId == "MDCR", ]

# relabel
exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T3. New users of typical antipsychotics age 65 years and older", "T1. Typical antipsychotics, 65 years and older")
exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T4. New users of haloperidol age 65 years and older", "T2. Haloperidol, 65 years and older")
exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] C2. New users of atypical antipsychotics aged 65 years and older", "C1. Atypical antipsychotics, 65 years and older")

outcomeOfInterest$outcomeName <- relabel(outcomeOfInterest$outcomeName, "[Epi 581] O2: Stroke as a Principal Inpatient Diagnosis ICD10 Update", "O1. Primary, inpatient stroke")

cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "1. Unadjusted", "[2001.01.01-2017.12.31] Unadjusted")
cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "2. Unadjusted 2015", "[2001.01.01-2015.09.30] Unadjusted")
cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "3. 1:1 PS Match on Selected Covariates", "[2001.01.01-2017.12.31] 1:1 PS match on select covariates")
cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "4. 1:1 PS Match on Selected Covariates 2015", "[2001.01.01-2015.09.30] 1:1 PS match on select covariates")
cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "5. 1:10 PS Match on Full Covariates", "[2001.01.01-2017.12.31] 1:10 PS match on full covariates")
cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "6. 1:10 PS Match on Full Covariates 2015", "[2001.01.01-2015.09.30] 1:10 PS match on full covariates")




