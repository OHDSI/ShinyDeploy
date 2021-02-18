library(magrittr)
# library(promises)
# library(future)
# future::plan(strategy = multisession)
# library(reactlog)
# reactlog::reactlog_enable()
appVersion <- "2.1.0"


# userName <- Sys.getenv("phoebedbUser")
# password <- Sys.getenv("phoebedbPw")
# databaseServer <- Sys.getenv("phoebedbServer")
# databaseName <- Sys.getenv("phoebedb")
# resultsSchema <- Sys.getenv("phoebedbTargetSchema")
# vocabularySchema <- Sys.getenv("phoebedbVocabSchema")
  
userName <- Sys.getenv("ownerUser")
password <- Sys.getenv("ownerPassword")
databaseServer <- Sys.getenv("ohdsiShinyServer")
databaseName <- Sys.getenv("ohdsiShinyDatabase")
resultsSchema <- Sys.getenv("studySchemaMskai")
vocabularySchema <- Sys.getenv("ohdsiShinyDbVocabularySchema")


source("R/DisplayFunctions.R")
source("R/Tables.R")
source("R/Plots.R")
source("R/Results.R")
source("R/ConceptRecommender.R")
source("R/DataPulls.R")
source("R/Connections.R")
source("R/HelperFunctions.R")
source("R/ModifyDataSource.R")


# Settings when running on server:
assign(x = "defaultLocalDataFolder", value = "data", envir = .GlobalEnv)
assign(x = "defaultLocalDataFile", value = "PreMerged.RData", envir = .GlobalEnv)
assign(x = "isValidConnection", value = FALSE, envir = .GlobalEnv)

assign(x = "defaultDatabaseMode", value = TRUE, envir = .GlobalEnv) # Set to FALSE if using file system.
assign(x = "dbms", value = "postgresql", envir = .GlobalEnv)
assign(x = "port", value = 5432, envir = .GlobalEnv)

# default app titles and text
assign(x = "cohortDiagnosticModeDefaultTitle", value = "Cohort Diagnostics", envir = .GlobalEnv)
assign(x = "phenotypeLibraryModeDefaultTitle", value = "Phenotype Lib 2", envir = .GlobalEnv)
source("html/defaultAboutTextPhenotypeLibrary.txt")

# Cleaning up any tables in memory:
dataModelSpecifications <-
  readr::read_csv(
    file = "resultsDataModelSpecification.csv",
    col_types = readr::cols(),
    guess_max = min(1e7)
  )
suppressWarnings(rm(list = snakeCaseToCamelCase(dataModelSpecifications$tableName)))

# connection information
if (!exists("shinySettings")) {
  # shinySettings object is from CohortDiagnostics::launchDiagnosticsExplorer()
  writeLines("Using default settings -- attempting to connect to OHDSI phenotype library")
  assign(x = "usingUserProvidedSettings", value = FALSE, envir = .GlobalEnv)
  if (userName != '') {
    assign("username", userName, envir = .GlobalEnv)
  }
  if (password != '') {
    assign("password", password, envir = .GlobalEnv)
  }
  if (databaseServer != '') {
    assign("server", databaseServer, envir = .GlobalEnv)
  }
  if (databaseName != '') {
    assign("database", databaseName, envir = .GlobalEnv)
  }
  if (all((databaseServer != ''),
          (database != ''))) {
    assign("server", paste(
      databaseServer,
      database,
      sep = "/"
    ),
    envir = .GlobalEnv)
  }
  if (vocabularySchema != '') {
    assign("vocabularyDatabaseSchema",
           vocabularySchema,
           envir = .GlobalEnv)
  }
  if (resultsSchema != '') {
    assign("resultsDatabaseSchema",
           resultsSchema,
           envir = .GlobalEnv)
  }
  
  if (server != "" &&
      database != "" &&
      username != "" &&
      password != "" &&
      port != "") {
    # writeLines(text = "Checking Connection parameters.")
    connectionIsValid <- try(isConnectionValid(
      dbms = dbms,
      server = server,
      port = port,
      username = username,
      password = password
    ))
    if (connectionIsValid) {
      assign(x = "isValidConnection",
             value = TRUE,
             envir = .GlobalEnv)
      connectionDetails <-
        DatabaseConnector::createConnectionDetails(
          dbms = dbms,
          server = server,
          port = port,
          user = username,
          password = password
        )
      # connection <-
      #   DatabaseConnector::connect(connectionDetails = connectionDetails)
      # writeLines(text = "Database Connector Connection.")
      connectionPool <- NULL
      # writeLines(text = "Connecting to Pool.")
      connectionPool <- pool::dbPool(
        drv = DatabaseConnector::DatabaseConnectorDriver(),
        dbms = dbms,
        server = paste(server, database, sep = "/"),
        port = port,
        user = username,
        password = password
      )
      writeLines(text = "Connected.")
    }
  }
  if (!is.null(x = defaultAboutTextPhenotypeLibrary)) {
    aboutText <- defaultAboutTextPhenotypeLibrary
  }
  userNotification <-
    paste0("Cohort Diagnostics app (version ", appVersion, ")")
} else {
  assign(x = "usingUserProvidedSettings", value = TRUE, envir = .GlobalEnv)
  databaseMode <- !is.null(x = shinySettings$connectionDetails)
  if (!is.null(x = shinySettings$aboutText)) {
    aboutText <- shinySettings$aboutText
  } else {
    aboutText <- ''
  }
  if (databaseMode) {
    writeLines(text = "Using user provided settings - connecting to database in dbms mode.")
    userNotification <- paste0("Connected to database.")
    connectionDetails <- shinySettings$connectionDetails
    if (is(object = connectionDetails$server, class2 = "function")) {
      drv <- DatabaseConnector::DatabaseConnectorDriver()
      dbms <- connectionDetails$dbms()
      server <- connectionDetails$server()
      port <- connectionDetails$port()
      user <- connectionDetails$user()
      password <- connectionDetails$password()
      connectionString <- connectionDetails$connectionString()
    } else {
      # For backwards compatibility with older versions of DatabaseConnector:
      drv <- DatabaseConnector::DatabaseConnectorDriver()
      dbms <- connectionDetails$dbms
      server <- connectionDetails$server
      port <- connectionDetails$port
      user <- connectionDetails$user
      password <- connectionDetails$password
      connectionString <- connectionDetails$connectionString
    }
    connectionIsValid <- isConnectionValid(
      dbms = dbms,
      server = server,
      port = port,
      username = username,
      password = password
    )
    if (connectionIsValid) {
      assign(x = "isValidConnection",
             value = TRUE,
             envir = .GlobalEnv)
      connectionDetails <-
        DatabaseConnector::createConnectionDetails(
          dbms = dbms,
          server = server,
          port = port,
          user = username,
          password = password
        )
      # connection <-
      #   DatabaseConnector::connect(connectionDetails = connectionDetails)
      connectionPool <- NULL
      connectionPool <- pool::dbPool(
        drv = DatabaseConnector::DatabaseConnectorDriver(),
        dbms = database,
        server = paste(server, database, sep = "/"),
        port = port,
        user = user,
        password = password
      )
      writeLines(text = "Connected.")
      if (!is.null(x = shinySettings$resultsDatabaseSchema)) {
        writeLines(text = "No results database schema provided.")
      } else {
        resultsDatabaseSchema <- shinySettings$resultsDatabaseSchema
      }
      if (!is.null(x = shinySettings$vocabularyDatabaseSchema)) {
        writeLines(text = "No results database schema provided.")
      } else {
        vocabularyDatabaseSchema <- shinySettings$vocabularyDatabaseSchema
      }
    } else {
      writeLines(text = "User provided connection parameters are not valid.")
    }
  } else {
    writeLines(text = "Using user provided settings - running on local mode. Looking for premerged file.")
    userNotification <- paste0("Using premerged file.")
    if (!is.null(x = shinySettings$dataFolder)) {
      dataFolder <- shinySettings$dataFolder
    } else {
      writeLines(text = "No data folder provided.User provided settings are not valid.")
      dataFolder <- NULL
    }
    if (!is.null(x = shinySettings$dataFile)) {
      writeLines(text = "No data file provided. User provided settings are not valid.")
      dataFile <- shinySettings$dataFile
    } else {
      dataFile <- NULL
    }
  }
}

# Cleanup connection when the application stops
shiny::onStop(function() {
  if (isValidConnection) {
    writeLines(text = "Closing database connections")
    if (DBI::dbIsValid(dbObj = connectionPool)) {
      pool::poolClose(pool = connectionPool)
    }
    # if (DBI::dbIsValid(dbObj = connection)) {
    #   DatabaseConnector::disconnect(connection = connection)
    # }
  }
})


if (isValidConnection) {
  loadTimeStart <- Sys.time()
  resultsTablesOnServer <-
    tolower(x = DatabaseConnector::dbListTables(conn = connectionPool,
                                                schema = resultsDatabaseSchema))
  
  # the code section below instantiates set of tables in R memory.
  # some tables are 'dummy' tables.
  writeLines("Loading Database Table")
  loadRequiredTables(tableName = "database",
                     databaseSchema = resultsDatabaseSchema,
                     required = TRUE,
                     connection = connectionPool)
  writeLines("Loading Cohort Table")
  loadRequiredTables(tableName = "cohort",
                     databaseSchema = resultsDatabaseSchema,
                     required = TRUE,
                     connection = connectionPool)
  # writeLines("Loading Cohort Extra Table")
  #   loadRequiredTables(tableName = "cohort_extra", 
  #                      databaseSchema = resultsDatabaseSchema,
  #                      connection = connectionPool)
  # writeLines("Loading Concept set Table - future")
  # loadRequiredTables(tableName = "concept_sets", 
  #                    databaseSchema = resultsDatabaseSchema,
  #                    connection = connectionPool)
  
  # Downloading covariate_ref and temporal_covariate_ref in global R because
  # it is now a shared resource across multiple R sessions
  # temporariliy commenting this during app development - because it take a long time to load 
  writeLines("Loading Covariate reference Table")
  loadRequiredTables(tableName = "covariate_ref",
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines("Loading Temporal Covariate reference Table")
  loadRequiredTables(tableName = "temporal_covariate_ref",
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  
  writeLines("Loading Phenotype Description Table")
  loadRequiredTables(tableName = "phenotype_description", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines("Loading Temporal Time Reference Table")
  loadRequiredTables(tableName = "temporal_time_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines("Loading analysis ref Table")
  loadRequiredTables(tableName = "analysis_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines("Loading Temporal analysis reference Table")
  loadRequiredTables(tableName = "temporal_analysis_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines(paste("All Tables are loaded in", 
                    scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                           time2 = loadTimeStart, 
                                           units = "secs"))), 
                    "seconds."))
  
  for (table in c(dataModelSpecifications$tableName, "recommender_set")) {
    if (table %in% resultsTablesOnServer &&
        !exists(x = snakeCaseToCamelCase(string = table)) &&
        !isEmpty(
          connection = connectionPool,
          tableName = table,
          resultsDatabaseSchema = resultsDatabaseSchema
        )) {
      assign(
        x = snakeCaseToCamelCase(table),
        value = dplyr::tibble(),
        envir = .GlobalEnv
      )
    }
  }
  dataSource <-
    createDatabaseDataSource(
      connection = connectionPool,
      connectionDetails = connectionDetails,
      resultsDatabaseSchema = resultsDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
} else {
  localDataPath <- file.path(dataFolder, dataFile)
  if (!file.exists(localDataPath)) {
    stop(sprintf("Local data file %s does not exist.", localDataPath))
  }
  dataSource <-
    createFileDataSource(premergedDataFile = localDataPath, envir = .GlobalEnv)
}


# create memory variables based on
if (exists("temporalTimeRef")) {
  temporalTimeRef <- get("temporalTimeRef") %>%
    dplyr::mutate(temporalChoices = paste0("Start ", .data$startDay, " to end ", .data$endDay))
  assign(x = "temporalTimeRef", value = temporalTimeRef, envir = .GlobalEnv)
}
if (exists("temporalCovariateRef")) {
  temporalCovariateRef <- temporalCovariateRef %>% 
    dplyr::mutate(conceptName = stringr::str_to_sentence(
      stringr::str_replace_all(
        string = .data$covariateName,
        pattern = "^.*: ",
        replacement = ""
      )
    ))
}
if (exists("covariateRef")) {
  covariateRef <- covariateRef %>% 
    dplyr::mutate(conceptName = stringr::str_to_sentence(
      stringr::str_replace_all(
        string = .data$covariateName,
        pattern = "^.*: ",
        replacement = ""
      )
    ))
  specifications <- readr::read_csv(
    file = "Table1Specs.csv",
    col_types = readr::cols(),
    guess_max = min(1e7)
  )
  assign(x = "prettyAnalysisIds",
         value = specifications$analysisId,
         envir = .GlobalEnv)
}


referentConceptIds <- c(0)
# modify tables in memory - process cohort table.
writeLines("Post processing downloaded tables.")
postProcessingStartTime <- Sys.time()
if (exists("cohort")) {
  # this table is required for app to work.
  cohort <- get("cohort") %>%
    dplyr::arrange(.data$cohortId) %>%
    dplyr::mutate(cohortName = stringr::str_remove(.data$cohortName, "\\[.+?\\] "))
  
  fixCohortTableMetadataForBackwardCompatibility()
  
  if ('metadata' %in% colnames(cohort)) {
    cohortMetaData <- list()
    for (i in 1:nrow(cohort)) {
      x <- RJSONIO::fromJSON(cohort[i, ]$metadata)
      for (j in 1:length(x)) {
        if (!any(is.null(x[[j]]), is.na(x[[j]]), names(x[j]) == "sql")) {
          x[[j]] <- stringr::str_split(string = x[[j]], pattern = ";")[[1]]
        }
      }
      x <- dplyr::bind_rows(x)
      x$cohort_id <- cohort[i, ]$cohortId
      x$phenotype_id <- cohort[i, ]$phenotypeId
      cohortMetaData[[i]] <- x
    }
    cohortMetaData <- dplyr::bind_rows(cohortMetaData) %>%
      readr::type_convert(col_types = readr::cols())
    if ('referent_concept_id' %in% colnames(cohortMetaData)) {
      referentConceptIds <-
        c(referentConceptIds,
          cohortMetaData$referent_concept_id) %>% unique()
    }
    colnames(cohortMetaData) <-
      snakeCaseToCamelCase(colnames(cohortMetaData))
  }
} else {
  writeLines("Cohort table not found")
}
if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
  cohort <- cohort %>% 
    dplyr::left_join(y = phenotypeDescription %>% 
                       dplyr::select(.data$phenotypeId, .data$phenotypeName),
                     by = "phenotypeId")
} else {
  cohort$phenotyeName <- ""
}

 
if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) { 
  phenotypeDescription <- phenotypeDescription %>%
    dplyr::mutate(clinicalDescription = 
                    stringr::str_squish(.data$clinicalDescription)) %>%
    dplyr::mutate(overview = (
      stringr::str_match(.data$clinicalDescription,
                         "Overview:(.*?)Presentation:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(presentation = (
      stringr::str_match(.data$clinicalDescription,
                         "Presentation:(.*?)Assessment:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(assessment = (
      stringr::str_match(.data$clinicalDescription,
                         "Assessment:(.*?)Plan:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(plan = (
      stringr::str_match(.data$clinicalDescription,
                         pattern = "Plan:(.*?)Prognosis:")
    )[,2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(prognosis = (
      stringr::str_match(.data$clinicalDescription,
                         pattern = "^.+Prognosis:(.*)")
      )[,2] %>%
      stringr::str_trim()) %>%
    # dplyr::mutate(
    #   clinicalDescription = stringr::str_replace_all(
    #     string = .data$clinicalDescription,
    #     pattern = "Overview:",
    #     replacement = "<strong>Overview:</strong>"
    #   )
    # ) %>%
    # dplyr::mutate(
    #   clinicalDescription = stringr::str_replace_all(
    #     string = .data$clinicalDescription,
    #     pattern = "Assessment:",
    #     replacement = "<br/><br/> <strong>Assessment:</strong>"
    #   )
    # ) %>%
    # dplyr::mutate(
    #   clinicalDescription = stringr::str_replace_all(
    #     string = .data$clinicalDescription,
    #     pattern = "Presentation:",
    #     replacement = "<br/><br/> <strong>Presentation: </strong>"
    #   )
    # ) %>%
    # dplyr::mutate(
    #   clinicalDescription = stringr::str_replace_all(
    #     string = .data$clinicalDescription,
    #     pattern = "Plan:",
    #     replacement = "<br/><br/> <strong>Plan: </strong>"
    #   )
    # ) %>%
    # dplyr::mutate(
    #   clinicalDescription = stringr::str_replace_all(
    #     string = .data$clinicalDescription,
    #     pattern = "Prognosis:",
    #     replacement = "<br/><br/> <strong>Prognosis: </strong>"
    #   )
    # ) %>%
    dplyr::inner_join(
      cohort %>%
        dplyr::group_by(.data$phenotypeId) %>%
        dplyr::summarize(cohortDefinitions = dplyr::n()) %>%
        dplyr::ungroup(),
      by = "phenotypeId"
    ) %>% 
    dplyr::mutate(referentConceptId = .data$phenotypeId/1000) %>% 
    dplyr::select(.data$phenotypeId, .data$phenotypeName,
                  .data$clinicalDescription, .data$overview,
                  .data$presentation, .data$assessment, .data$plan,
                  .data$prognosis,
                  .data$cohortDefinitions, .data$referentConceptId)
  
  referentConceptIds <-
    c(referentConceptIds,
      phenotypeDescription$referentConceptId) %>% unique()
}
writeLines(paste("Post processing cohort table completed in", 
                 scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                                   time2 = postProcessingStartTime, 
                                                   units = "secs"))), 
                 "seconds."))

if (isValidConnection) {
  referentTimeStart <- Sys.time()
  referentConceptIdsDataFrame <-
    renderTranslateQuerySql(
      connection = dataSource$connection,
      vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
      concept_id_list = referentConceptIds,
      sql = SqlRender::readSql("sql/ConceptSynonymNamesForListOfConceptIds.sql"), 
      snakeCaseToCamelCase = TRUE
    ) %>%
    dplyr::arrange(.data$conceptId)
  writeLines(paste("Downloading details for referent concepts took", 
                   scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                                     time2 = referentTimeStart, 
                                                     units = "secs"))), 
                   "seconds."))
} else {
  referentConceptIdsDataFrame <-
    dplyr::tibble(conceptId = 0, conceptSynonymName = 'No matching concept')
}

referentConceptIdsSearchTerms <- referentConceptIdsDataFrame %>%
  dplyr::group_by(.data$conceptId) %>%
  dplyr::summarise(conceptNameSearchTerms = toString(.data$conceptSynonymName)) %>%
  dplyr::ungroup()

# pubmedQueryString <- tidyr::replace_na(data = cohortMetaData$pmid %>% unique(),
#                                        replace = 0) %>%
#   paste(collapse = '[UID] OR ')
# pubmedIds <- easyPubMed::get_pubmed_ids(pubmed_query_string = pubmedQueryString)
# pubmedXmlData <- easyPubMed::fetch_pubmed_data(pubmed_id_list = pubmedIds)


if (exists('cohortMetaData')) {
  if ('referentConceptId' %in% colnames(cohortMetaData)) {
    cohortReferentConceptSearchTerms <- cohortMetaData %>%
      dplyr::select(.data$cohortId, .data$referentConceptId) %>% 
      dplyr::distinct() %>% 
      dplyr::left_join(referentConceptIdsSearchTerms,
                       by = c("referentConceptId" = "conceptId")) %>%
      dplyr::group_by(.data$cohortId) %>% 
      dplyr::mutate(referentConceptId = paste0(as.character(.data$referentConceptId), collapse = ", "),
                    referentConceptIdsSearchTerms = paste0(.data$conceptNameSearchTerms, collapse = ", ")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(referentConceptIdsSearchTerms = paste(referentConceptId,referentConceptIdsSearchTerms)) %>%
      dplyr::select(-.data$referentConceptId, - .data$conceptNameSearchTerms) %>% 
      dplyr::distinct()
    
    cohort <- cohort %>% 
      dplyr::left_join(y = cohortReferentConceptSearchTerms, by = c('cohortId'))
    
    remove(cohortReferentConceptSearchTerms)
  }
  if ('cohortType' %in% colnames(cohortMetaData)) {
    cohortType <- cohortMetaData %>%
      dplyr::select(.data$cohortId, .data$cohortType) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(.data$cohortId) %>% 
      dplyr::mutate(cohortType = paste0(.data$cohortType, collapse = ", ")) %>%
      dplyr::ungroup() %>% 
      dplyr::distinct()
    
    cohort <- cohort %>% 
      dplyr::left_join(y = cohortType, by = c('cohortId'))
    
    remove(cohortType)
  }
}
remove(cohortMetaData)

if (exists('phenotypeDescription')) {
  if ('referentConceptId' %in% colnames(phenotypeDescription)) {
    phenotypeDescription <- phenotypeDescription %>%
      dplyr::left_join(referentConceptIdsSearchTerms,
                       by = c('referentConceptId' = 'conceptId')) %>%
      dplyr::select(-.data$referentConceptId) %>%
      dplyr::rename(
        phenotypeSynonyms = .data$conceptNameSearchTerms
      )
  }
}

assign(x = "combinationsOfPhenotypeDatabaseCohort",
       getCombinationsOfPhenotypeDatabaseCohort(dataSource = dataSource))


if (exists(x = "phenotypeDescription")) {
  appTitle <- phenotypeLibraryModeDefaultTitle
} else {
  appTitle <- cohortDiagnosticModeDefaultTitle
}