#
# Source R files
#

source("DataPulls.R")
source("PlotsAndTables.R")

getConfiguration <- function(label) {
  sourceFile <- "config.json"
  if (file.exists(sourceFile)) {
    RJSONIO::fromJSON(readChar(sourceFile, file.info(sourceFile)$size))[[label]]
  } else {
    ""
  }
}

makeOt2 <- function(id) {
  string <- as.character(id)
  string <- paste0(substring(string, 1, 2),
                   "2",
                   substring(string, 4, 9))
  as.integer(string)
}

#
# Load general configuration information
#

cohortMask <- readr::read_csv(file.path("settings", "masks.csv"))
propensityScoreMask <- tibble::tibble(
  label = c("unadjusted", "matched", "stratified"),
  index = c(1, 2, 3)
)

timeAtRiskMask <- tibble::tibble(
  label = c("Intent-to-treat (ITT)", "On-treatment (OT)", "OT and censor at +agent"),
  multiplier = c(1, 0, 2)
)

mapAnalysisIdForBalance <- function(analysisId) {
  map <- c(1,5,6,
           4,5,6,
           7,5,6,
           0,
           11,5,6,
           14,5,6,
           17,5,6)
  return(map[analysisId])
}

outcomeInfo <- readr::read_csv(file.path("settings", "OutcomesOfInterest.csv"))

connectionPool <- NULL
connection <- NULL

#
# Set defaults when running on ShinyDeploy server
#

defaultDatabaseMode <- TRUE
defaultDataFolder <- "data"
defaultServer <- Sys.getenv("shinydbServer")
defaultDatabase <- Sys.getenv("shinydbDatabase")
defaultPort <- 5432
defaultUser <- Sys.getenv("shinydbUser")
defaultPassword <- Sys.getenv("shinydbPw")
defaultResultsSchema <- getConfiguration("resultsSchema")
defaultBlind <- FALSE
defaultHeaderText <- getConfiguration("headerText")
defaultMainMask <- getConfiguration("mainMask")

if (!exists("shinySettings")) { # Running on ShinyDeploy server
  writeLines("Using default settings")
  databaseMode <- defaultDatabaseMode & defaultServer != ""
  if (databaseMode) {
    # connectionPool <- pool::dbPool(
    #  drv = DatabaseConnector::DatabaseConnectorDriver(),
    #  dbms = "postgresql",
    #  server = paste(defaultServer, defaultDatabase, sep = "/"),
    #  port = defaultPort,
    #  user = defaultUser,
    #  password = defaultPassword
    #)
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = "postgresql",
      server = paste(defaultServer,
                     defaultDatabase,
                     sep = "/"),
      port = defaultPort,
      user = defaultUser,
      password = defaultPassword)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    resultsDatabaseSchema <- defaultResultsSchema

    sql <- paste0("SET search_path TO ", resultsDatabaseSchema, ";")
    DatabaseConnector::executeSql(connection = connection, sql = sql)
  } else {
    dataFolder <- defaultDataFolder
  }
  headerText <- defaultHeaderText
  blind <- defaultBlind
  mainMask <- defaultMainMask
} else {
  writeLines("Using user-provided settings")
  databaseMode <- !is.null(shinySettings$connectionDetails)
  if (databaseMode) {
    connectionDetails <- shinySettings$connectionDetails
    # connectionPool <-
    #   pool::dbPool(
    #     drv = DatabaseConnector::DatabaseConnectorDriver(),
    #     dbms = "postgresql",
    #     server = connectionDetails$server(),
    #     port = connectionDetails$port(),
    #     user = connectionDetails$user(),
    #     password = connectionDetails$password(),
    #     connectionString = connectionDetails$connectionString()
    #   )
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    resultsDatabaseSchema <- shinySettings$resultsDatabaseSchema

    sql <- paste0("SET search_path TO ", resultsDatabaseSchema, ";")
    DatabaseConnector::executeSql(connection = connection, sql = sql)
  } else {
    dataFolder <- shinySettings$dataFolder
  }
  headerText <- shinySettings$headerText
  mainMask <- shinySettings$mainMask
  blind <- shinySettings$blind
}

# positiveControlOutcome <- NULL

if (databaseMode) {

  # onStop(function() {
  #   if (DBI::dbIsValid(connectionPool)) {
  #     writeLines("Closing database pool")
  #     pool::poolClose(connectionPool)
  #   }
  # })

  exposureOfInterest <- getExposures(connection)
  outcomeOfInterest <- getOutcomes(connection)
  database <- getDatabases(connection)
  metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]
  cohortMethodAnalysis <- getAnalyses(connection)

  sql <- "SELECT
    DISTINCT target_id, comparator_id, cohort_method_result.outcome_id
  FROM cohort_method_result
  INNER JOIN outcome_of_interest ON
    cohort_method_result.outcome_id = outcome_of_interest.outcome_id"

  tcos <- DatabaseConnector::querySql(connection = connection,
                                      sql = sql)

  colnames(tcos) <- SqlRender::snakeCaseToCamelCase(colnames(tcos))

  sql <- paste0(
    "SELECT * FROM cohort_method_result WHERE outcome_id IN (",
    paste(outcomeOfInterest$outcomeId, collapse = ","),
    ")")

  cohortMethodResult <- DatabaseConnector::querySql(connection, sql)
  colnames(cohortMethodResult) <- SqlRender::snakeCaseToCamelCase(colnames(cohortMethodResult))

  # loadResultsTable("attrition")
  # loadResultsTable("cm_follow_up_dist")
  # loadResultsTable("cohort_method_analysis")
  # loadResultsTable("cohort_method_result")
  # loadResultsTable("comparison_summary")
  # loadResultsTable("covariate")
  # loadResultsTable("covariate_analysis")
  # loadResultsTable("database")
  # loadResultsTable("exposure_of_interest")
  # loadResultsTable("exposure_summary")
  # loadResultsTable("likelihood_profile") # Not yet needed
  # loadResultsTable("negative_control_outcome")
  # loadResultsTable("outcome_of_interest")
  # loadResultsTable("propensity_model")
  # loadResultsTable("ps_auc_assessment")

} else { # Load from local folder

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
        newData$tau <- NULL
        newData$traditionalLogRr <- NULL
        newData$traditionalSeLogRr <- NULL
        if (!all(colnames(newData) %in% colnames(existingData))) {
          stop(sprintf("Columns names do not match in %s. \nObserved:\n %s, \nExpecting:\n %s",
                       file,
                       paste(colnames(newData), collapse = ", "),
                       paste(colnames(existingData), collapse = ", ")))

        }
        newData <- rbind(existingData, newData)
        newData <- unique(newData)
      }
      assign(camelCaseName, newData, envir = .GlobalEnv)
    }
    invisible(NULL)
  }
  # removePart <- removeParts[2]
  file <- files[grepl(removePart, files)][1]
  for (removePart in removeParts) {
    invisible(lapply(files[grepl(removePart, files)], loadFile, removePart))
  }

  tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
  tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
  metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]
}
