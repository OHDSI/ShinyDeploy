library(shiny)
library(pool)
library(DatabaseConnector)
library(data.table)
source("DataPulls.R")

connPool <- NULL # Will be initialized if using a DB

# Cleanup the database connPool if it was created
onStop(function() {
  if (!is.null(connPool)) {
    if (DBI::dbIsValid(connPool)) {
      writeLines("Closing database pool")
      poolClose(connPool)
    }
  }
})

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

usingDbStorage <- function() {
  return(shinySettings$storage=='database')
}

# Data Loading Priority: Database, "/data" folder, S3
if (!exists("shinySettings")) {
 if (file.exists("data")) {
    shinySettings <- list(storage = "filesystem", dataFolder = "data", dataFile = "PreMerged.RData")
  } else if (is_installed("aws.s3") && is_installed("aws.ec2metadata")){
    library("aws.ec2metadata")
    shinySettings <- list(storage = "s3", dataFolder = Sys.getenv("OHDSI_SHINY_DATA_BUCKET"), dataFile = "PioneerWatchfulWaiting/PreMerged.RData")
  } else {
    stop("Results data not found")
  }
}
dataStorage <- shinySettings$storage
dataFolder <- shinySettings$dataFolder
dataFile <- shinySettings$dataFile

suppressWarnings(rm("cohort", "cohortCount", "database"))

if (dataStorage == "database") {
  connPool <- dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = shinySettings$connectionDetails$dbms,
    server = shinySettings$connectionDetails$server,
    port = shinySettings$connectionDetails$port,
    user = shinySettings$connectionDetails$user,
    password = shinySettings$connectionDetails$password
  )  
  loadDataFromDB(connPool)
} else if (dataStorage == "s3") {
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
      data <- data.table::fread(file.path(folder, file))
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
  covNameWithId <- grepl(': \\d\\d\\d$', covariate$covariateName)
  if (any(covNameWithId)) {
    # Temporary quick fix to replace cohortId with atlasName
    cohorts <- readr::read_csv("./cohorts.csv", col_types = readr::cols())
    newCovariateName <- sapply(covariate$covariateName[covNameWithId],
                       function(x) {
                         cohortId <- substr(x, nchar(x)-2, nchar(x))
                         cohort <- cohorts[cohorts$cohortId==cohortId,]
                         if (nrow(cohort) > 0) {
                           return(paste0(substr(x, 1, nchar(x)-3), cohort$name))
                         } else {
                           # cohortId not found, return original
                           return(x)
                         }
                       }
   )
   covariate$covariateName[covNameWithId] <- newCovariateName
  }
  covariate$windowId <- covariate$covariateId %% 10
}

# Setup filters
domain <- data.table()
domain <- rbind(domain,data.table(name = "All", covariateAnalysisId = c(1:10000)))
domain <- rbind(domain,data.table(name = "Cohort", covariateAnalysisId = c(10000)))
domain <- rbind(domain,data.table(name = "Demographics", covariateAnalysisId = c(1:99)))
domain <- rbind(domain,data.table(name = "Drug", covariateAnalysisId = c(412)))
domain <- rbind(domain,data.table(name = "Condition", covariateAnalysisId = c(212)))
domain <- rbind(domain,data.table(name = 'Procedure', covariateAnalysisId = c(712)))
domain$name <- as.character(domain$name)
domainName <- "All"

# This must match the featureTimeWindow.csv from the Pioneer study
timeWindow <- data.table(windowId=c(1:4), name=c("-365 to index", "index to 365", "366d to 730d", "731d+"))
timeWindow$name <- as.character(timeWindow$name)

cohortXref <- data.table::fread("./cohortXref.csv")
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

cohortInfo <- data.table::fread("./cohorts.csv")
cohortInfo <- cohortInfo[order(cohortInfo$name),]

# Read in the database terms of use
dbTermsOfUse <- readr::read_csv("./databaseTermsOfUse.csv", col_types = readr::cols())
colnames(dbTermsOfUse) <- SqlRender::snakeCaseToCamelCase(colnames(dbTermsOfUse))
database <- dplyr::left_join(database, dbTermsOfUse, by="databaseId")
database <- database[order(database$databaseId),]


# Add Time to Event names and ids

# Gather unique outcome ids in time to event table
ids <- unique(cohortTimeToEvent$outcomeId)
# Find corresponding cohort names
names <- sapply(ids,function(id){ cohortStagingCount$name[cohortStagingCount$cohortId == id ][1]})

# hack/fix which I don't understand
#if(length(cohortStagingCount$name[cohortStagingCount$cohortId == max(ids)]) == 0){
#  names <- c(names, 'Symptomatic progr. free surv.')
#}

KMIds <- data.table(id = ids,
                    name = names)

#Filter out NA value in name which leads to problems with computations and plotting
KMIds <- KMIds[!is.na(KMIds$name)]