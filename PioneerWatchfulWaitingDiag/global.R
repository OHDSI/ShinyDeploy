library(magrittr)

source("R/StartUpScripts.R")
source("R/DisplayFunctions.R")
source("R/Tables.R")
source("R/Plots.R")
source("R/Results.R")

# Settings when running on server:
defaultLocalDataFolder <- "data"
defaultLocalDataFile <- "PreMerged.RData"
serverDataFolder <- Sys.getenv("OHDSI_SHINY_DATA_BUCKET")
serverDataFile <- "PioneerWatchfulWaitingDiag/PreMerged.RData"

connectionPool <- NULL
defaultServer <- Sys.getenv("shinydbServer")
defaultDatabase <- Sys.getenv("shinydbDatabase")
defaultPort <- 5432
defaultUser <- Sys.getenv("shinydbUser")
defaultPassword <- Sys.getenv("shinydbPw")
defaultResultsSchema <- 'thrombosisthrombocytopenia'
defaultVocabularySchema <- defaultResultsSchema
alternateVocabularySchema <- c('vocabulary')

defaultDatabaseMode <- FALSE # Use file system if FALSE

appInformationText <- "V 2.1"
appInformationText <- "Powered by OHDSI Cohort Diagnostics application - Version 2.1. This app is working in"
if (defaultDatabaseMode) {
  appInformationText <- paste0(appInformationText, " database")
} else {
  appInformationText <- paste0(appInformationText, " local file")
}
appInformationText <- paste0(appInformationText, 
                             " mode. Application was last initated on ", 
                             lubridate::now(tzone = "EST"),
                             " EST. Cohort Diagnostics website is at https://ohdsi.github.io/CohortDiagnostics/")

if (!exists("shinySettings")) {
  writeLines("Using default settings")
  databaseMode <- defaultDatabaseMode & defaultServer != ""
  if (databaseMode) {
    connectionPool <- pool::dbPool(
      drv = DatabaseConnector::DatabaseConnectorDriver(),
      dbms = "postgresql",
      server = paste(defaultServer, defaultDatabase, sep = "/"),
      port = defaultPort,
      user = defaultUser,
      password = defaultPassword
    )
    resultsDatabaseSchema <- defaultResultsSchema
  } else {
    dataFolder <- defaultLocalDataFolder
  }
  vocabularyDatabaseSchemas <-
    setdiff(x = c(defaultVocabularySchema, alternateVocabularySchema),
            y = defaultResultsSchema) %>%
    unique() %>%
    sort()
} else {
  writeLines("Using settings provided by user")
  databaseMode <- !is.null(shinySettings$connectionDetails)
  if (databaseMode) {
    connectionDetails <- shinySettings$connectionDetails
    if (is(connectionDetails$server, "function")) {
      connectionPool <-
        pool::dbPool(
          drv = DatabaseConnector::DatabaseConnectorDriver(),
          dbms = "postgresql",
          server = connectionDetails$server(),
          port = connectionDetails$port(),
          user = connectionDetails$user(),
          password = connectionDetails$password(),
          connectionString = connectionDetails$connectionString()
        )
    } else {
      # For backwards compatibility with older versions of DatabaseConnector:
      connectionPool <-
        pool::dbPool(
          drv = DatabaseConnector::DatabaseConnectorDriver(),
          dbms = "postgresql",
          server = connectionDetails$server,
          port = connectionDetails$port,
          user = connectionDetails$user,
          password = connectionDetails$password,
          connectionString = connectionDetails$connectionString
        )
    }
    resultsDatabaseSchema <- shinySettings$resultsDatabaseSchema
    vocabularyDatabaseSchemas <-
      shinySettings$vocabularyDatabaseSchemas
  } else {
    dataFolder <- shinySettings$dataFolder
  }
}

dataModelSpecifications <-
  read.csv("resultsDataModelSpecification.csv")
# Cleaning up any tables in memory:
suppressWarnings(rm(
  list = SqlRender::snakeCaseToCamelCase(dataModelSpecifications$tableName)
))

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

if (databaseMode) {
  onStop(function() {
    if (DBI::dbIsValid(connectionPool)) {
      writeLines("Closing database pool")
      pool::poolClose(connectionPool)
    }
  })
  
  resultsTablesOnServer <-
    tolower(DatabaseConnector::dbListTables(connectionPool, schema = resultsDatabaseSchema))
  
  # vocabularyTablesOnServer <- list()
  # vocabularyTablesInOmopCdm <- c('concept', 'concept_relationship', 'concept_ancestor',
  #                                'concept_class', 'concept_synonym',
  #                                'vocabulary', 'domain', 'relationship')
  
  # for (i in length(vocabularyDatabaseSchemas)) {
  #
  #     tolower(DatabaseConnector::dbListTables(connectionPool, schema = vocabularyDatabaseSchemas[[i]]))
  # vocabularyTablesOnServer[[i]] <- intersect(x = )
  # }
  loadResultsTable("database", required = TRUE)
  loadResultsTable("cohort", required = TRUE)
  loadResultsTable("temporal_time_ref")
  loadResultsTable("concept_sets")
  loadResultsTable("cohort_count", required = TRUE)
  
  for (table in c(dataModelSpecifications$tableName)) {
    #, "recommender_set"
    if (table %in% resultsTablesOnServer &&
        !exists(SqlRender::snakeCaseToCamelCase(table)) &&
        !isEmpty(table)) {
      #if table is empty, nothing is returned because type instability concerns.
      assign(SqlRender::snakeCaseToCamelCase(table),
             dplyr::tibble())
    }
  }
  
  dataSource <-
    createDatabaseDataSource(
      connection = connectionPool,
      resultsDatabaseSchema = resultsDatabaseSchema,
      vocabularyDatabaseSchema = resultsDatabaseSchema
    )
} else {
  localDataPath <- file.path(dataFolder, defaultLocalDataFile)
  if (file.exists(localDataPath)) {
    dataSource <-
      createFileDataSource(localDataPath, envir = .GlobalEnv)
  }
  else {
    if (is_installed("aws.s3") && is_installed("aws.ec2metadata")) {
      library("aws.ec2metadata") # not sure if this is necessary, copied it from the results app
      fileExists <- aws.s3::head_object(dataFile, bucket = dataFolder)
      if (fileExists) {
        writeLines("Using merged data detected in S3 Bucket")
        aws.s3::s3load(serverDataFile, bucket = serverDataFolder)
      } else {
        stop(paste0("Could not find ", dataFile, " in S3 Bucket"))
      }
    }
    else {
      stop(sprintf("Local data file %s does not exist and S3 packages are not installed.", localDataPath))
    }
  }
}

if (exists("database")) {
  if (nrow(database) > 0 &&
      "vocabularyVersion" %in% colnames(database)) {
    database <- database %>%
      dplyr::mutate(
        databaseIdWithVocabularyVersion = paste0(databaseId, " (", .data$vocabularyVersion, ")")
      )
  }
}

if (exists("cohort")) {
  cohort <- get("cohort")
  cohort <- cohort %>%
    dplyr::arrange(.data$cohortId) %>%
    dplyr::mutate(shortName = paste0("C", dplyr::row_number())) %>%
    dplyr::mutate(compoundName = paste0(.data$shortName, ": ", .data$cohortName,"(", .data$cohortId, ")"))
}

if (exists("temporalTimeRef")) {
  temporalCovariateChoices <- get("temporalTimeRef") %>%
    dplyr::mutate(choices = paste0("Start ", .data$startDay, " to end ", .data$endDay)) %>%
    dplyr::select(.data$timeId, .data$choices) %>%
    dplyr::arrange(.data$timeId)
}

if (exists("covariateRef")) {
  specifications <- readr::read_csv(
    file = "Table1Specs.csv",
    col_types = readr::cols(),
    guess_max = min(1e7)
  )
  prettyAnalysisIds <- specifications$analysisId
} else {
  prettyAnalysisIds <- c(0)
}
