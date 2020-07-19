# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(storage = "filesystem", dataFolder = "data", dataFile = "PreMerged.RData")
  } else if (is_installed("aws.s3") && is_installed("aws.ec2metadata")){
    library("aws.ec2metadata")
    shinySettings <- list(storage = "s3", dataFolder = Sys.getenv("OHDSI_SHINY_DATA_BUCKET"), dataFile = "Covid19CharacterizationCharybdis/14yov8ej_PreMerged.RData")
  } else {
    shinySettings <- list(storage = "filesystem", dataFolder = "c:/temp/exampleStudy", dataFile = "PreMerged.RData")
  }
}
dataStorage <- shinySettings$storage
dataFolder <- shinySettings$dataFolder
dataFile <- shinySettings$dataFile

suppressWarnings(rm("cohort", "cohortCount", "cohortOverlap", "conceptSets", "database", "incidenceRate", "includedSourceConcept", "inclusionRuleStats", "indexEventBreakdown", "orphanConcept", "timeDistribution"))

if (dataStorage == "s3") {
  fileExists <- aws.s3::head_object(dataFile, bucket = dataFolder)
  if (fileExists) {
    writeLines("Using merged data detected in S3 Bucket")
    aws.s3::s3load(dataFile, bucket = dataFolder)
  } else {
    writeLines(paste0("Could not find ", dataFile, " in S3 Bucket"))
  }
} else {
  if (file.exists(file.path(dataFolder, dataFile))) {
    writeLines("Using merged data detected in data folder")
    load(file.path(dataFolder, dataFile))
  } else {
    zipFiles <- list.files(dataFolder, pattern = ".zip", full.names = TRUE)
    
    loadFile <- function(file, folder, overwrite) {
      # print(file)
      tableName <- gsub(".csv$", "", file)
      camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
      data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
      colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
      
      if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
        existingData <- get(camelCaseName, envir = .GlobalEnv)
        if (nrow(existingData) > 0) {
          if (nrow(data) > 0 &&
              all(colnames(existingData) %in% colnames(data)) &&
              all(colnames(data) %in% colnames(existingData))) {
            data <- data[, colnames(existingData)]
          }
          
          if (!isTRUE(all.equal(colnames(data), colnames(existingData), check.attributes = FALSE))) {
            stop("Table columns do no match previously seen columns. Columns in ", 
                 file, 
                 ":\n", 
                 paste(colnames(data), collapse = ", "), 
                 "\nPrevious columns:\n",
                 paste(colnames(existingData), collapse = ", "))
          }
        }
        data <- rbind(existingData, data)
      }
      assign(camelCaseName, data, envir = .GlobalEnv)
      
      invisible(NULL)
    }
    
    for (i in 1:length(zipFiles)) {
      writeLines(paste("Processing", zipFiles[i]))
      tempFolder <- tempfile()
      dir.create(tempFolder)
      unzip(zipFiles[i], exdir = tempFolder)
      
      csvFiles <- list.files(tempFolder, pattern = ".csv")
      lapply(csvFiles, loadFile, folder = tempFolder, overwrite = (i == 1))
      
      unlink(tempFolder, recursive = TRUE)
    }
  }  
}


if (exists("covariate")) {
  covariate <- unique(covariate)
  covariate$windowId <- as.numeric(substr(covariate$covariateId, nchar(covariate$covariateId), nchar(covariate$covariateId)))
}

# Setup filters
domain <- data.frame()
domain <- rbind(domain,data.frame(name = "All", covariateAnalysisId = c(1:1000)))
domain <- rbind(domain,data.frame(name = "Cohort", covariateAnalysisId = c(10000)))
domain <- rbind(domain,data.frame(name = "Demographics", covariateAnalysisId = c(1:99)))
domain <- rbind(domain,data.frame(name = "Drug", covariateAnalysisId = c(412)))
domain <- rbind(domain,data.frame(name = "Condition", covariateAnalysisId = c(212)))
domain$name <- as.character(domain$name)
domainName <- "All"

# This must match the featureTimeWindow.csv from the Charybdis study
timeWindow <- data.frame(windowId=c(1:4), name=c("-365d to -1d", "-30d to -1d", "index", "index to 30d"))
timeWindow$name <- as.character(timeWindow$name)

cohortXref <- readr::read_csv("./cohortXref.csv", col_types = readr::cols())
targetCohort <- cohortXref[,c("targetId","targetName")]
targetCohort <- unique(targetCohort)
targetCohort <- targetCohort[order(targetCohort$targetName),]
strataCohort <- cohortXref[,c("strataId","strataName")]
strataCohort <- unique(strataCohort)
strataCohort <- strataCohort[order(strataCohort$strataId),]

# Cohort characterization & comparison will be restricted
# to those cohorts where the count is >= 140 per the study
# protocol 
hasCharacterization <- cohortCount[cohortCount$cohortSubjects >= 140,] # Filter to those cohorts that are characterized in Charybdis
characterizationCohortIds <- unique(hasCharacterization$cohortId) # Get the unique cohorts across all databases
characterizationTargetCohort <- unique(cohortXref[cohortXref$cohortId %in% characterizationCohortIds,c("targetId","targetName")])
characterizationTargetCohort <- characterizationTargetCohort[order(characterizationTargetCohort$targetName),]
characterizationStrataCohort <- unique(cohortXref[cohortXref$cohortId %in% characterizationCohortIds,c("strataId","strataName")])
characterizationStrataCohort <- characterizationStrataCohort[order(characterizationStrataCohort$strataId),]

initCharCohortId <- characterizationTargetCohort$targetId[1]
initCharCompareCohortId <- characterizationTargetCohort$targetId[2]
targetName <- cohortXref[cohortXref$cohortId == initCharCohortId,c("targetName")][1]
strataName <- cohortXref[cohortXref$cohortId == initCharCohortId,c("strataName")][1]
comparatorName <- cohortXref[cohortXref$cohortId == initCharCompareCohortId,c("targetName")][1]
comparatorStrataName <- cohortXref[cohortXref$cohortId == initCharCompareCohortId,c("strataName")][1]

cohortInfo <- readr::read_csv("./cohorts.csv", col_types = readr::cols())
cohortInfo <- cohortInfo[order(cohortInfo$name),]
