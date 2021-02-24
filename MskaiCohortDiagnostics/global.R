library(magrittr)
# library(promises)
# library(future)
# future::plan(strategy = multisession)
# library(reactlog)
# reactlog::reactlog_enable()
appVersion <- "Running Cohort Diagnostics 2.1.0"

# userName <- Sys.getenv("phoebedbUser")
# password <- Sys.getenv("phoebedbPw")
# databaseServer <- Sys.getenv("phoebedbServer")
# databaseName <- Sys.getenv("phoebedb")
# resultsSchema <- Sys.getenv("phoebedbTargetSchema")
# vocabularySchema <- Sys.getenv("phoebedbVocabSchema")

userName <- Sys.getenv("charybdisdbUser")
password <- Sys.getenv("charybdisdbPw")
databaseServer <- Sys.getenv("shinydbServer")
databaseName <- Sys.getenv("shinydbDatabase")
resultsSchema <- 'mskai'
vocabularySchema <- 'vocabulary'


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

assign(x = "databaseModeWhenPossible", value = TRUE, envir = .GlobalEnv) # Set to FALSE if using file system.
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
options(future.rng.onMisue = "ignore")



#########################################################################
# userNotification <-
#   paste0("Cohort Diagnostics app (version ", appVersion, ")")


############################################

if (exists("shinySettings")) {
  assign(x = "usingUserProvidedSettings", value = TRUE, envir = .GlobalEnv)
  writelines("User provided settings found.")
} else {
  assign(x = "usingUserProvidedSettings", value = FALSE, envir = .GlobalEnv)
}

# Database Connection information
if (databaseModeWhenPossible) {
  writeLines("Attempting to connect to Database.")
  if (exists("shinySettings") && !is.null(shinySettings$connectionDetails)) {
    writeLines(text = "Connection details has been provided by user.")
    connectionDetails <- shinySettings$connectionDetails
    writeLines(text = "User setting has database connection details. \n - attempting to connect to database in dbms mode.")
    
    if (is(object = connectionDetails$server, class2 = "function")) {
      assign("drv", DatabaseConnector::DatabaseConnectorDriver(), envir = .GlobalEnv)
      connectionString <- connectionDetails$connectionString()
      assign("username", connectionDetails$user(), envir = .GlobalEnv)
      assign("password", connectionDetails$password(), envir = .GlobalEnv)
      assign("server", connectionDetails$server(), envir = .GlobalEnv)
      # assign("dbms", connectionDetails$dbms(), envir = .GlobalEnv)
      # assign("port", connectionDetails$port(), envir = .GlobalEnv)
    } else {
      # For backwards compatibility with older versions of DatabaseConnector:
      assign("drv", DatabaseConnector::DatabaseConnectorDriver(), envir = .GlobalEnv)
      connectionString <- connectionDetails$connectionString
      assign("username", connectionDetails$user, envir = .GlobalEnv)
      assign("password", connectionDetails$password, envir = .GlobalEnv)
      assign("server", connectionDetails$server, envir = .GlobalEnv)
      # assign("dbms", connectionDetails$dbms, envir = .GlobalEnv)
      # assign("port", connectionDetails$port, envir = .GlobalEnv)
      drv <- DatabaseConnector::DatabaseConnectorDriver()
    }
    if (is.null(shinySettingssresultsDatabaseSchema)) {
      stop("Use provided connection settings is incomplete - resultsDatabaseSchema.")
    } else {
      resultsDatabaseSchema <- shinySettings$resultsDatabaseSchema
    }
    if (!is.null(x = shinySettings$vocabularyDatabaseSchema)) {
      stop("Use provided connection settings is incomplete - vocabularyDatabaseSchema.")
    } else {
      vocabularyDatabaseSchema <- shinySettings$vocabularyDatabaseSchema
    }
  } else {
    writeLines(text = "Connection details has not been provided by user, using default connection settings.")
    requiredFieldsToConnectToDatabase <- c('userName', 'password', 'databaseServer', 'databaseName', 'vocabularySchema')
    lengthGet <- function(x) {length(get(x))}
    if (all((all(unlist(lapply(X = requiredFieldsToConnectToDatabase, FUN = exists)))),
                 !0 %in% (unlist(lapply(X = requiredFieldsToConnectToDatabase, FUN = lengthGet))))) {
      assign("username", userName, envir = .GlobalEnv)
      assign("password", password, envir = .GlobalEnv)
      assign("dbms", dbms, envir = .GlobalEnv)
      assign("server", paste(databaseServer,
                             databaseName,
                             sep = "/"),
             envir = .GlobalEnv)
      assign("vocabularyDatabaseSchema",
             vocabularySchema,
             envir = .GlobalEnv)
      assign("resultsDatabaseSchema",
             resultsSchema,
             envir = .GlobalEnv)
    } else {
      stop("Default connection settings is incomplete.")
    }
  }
  connectionDetails <-
    DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      server = server,
      port = port,
      user = username,
      password = password
    )
  connectionIsValid <- try(isConnectionValid(
    dbms = dbms,
    server = server,
    port = port,
    username = username,
    password = password
  ))
  assign(x = "isValidConnection",
         value = connectionIsValid,
         envir = .GlobalEnv)
  if (isValidConnection) {
    # connection <-
    #   DatabaseConnector::connect(connectionDetails = connectionDetails)
    databaseMode <- TRUE
    connectionPool <- NULL
    writeLines(text = "Connecting to Pool.")
    connectionPool <- pool::dbPool(
      drv = DatabaseConnector::DatabaseConnectorDriver(),
      dbms = dbms,
      server = server,
      port = port,
      user = username,
      password = password
    )
    writeLines("Connected to database.")
  } else {
    writeLines("No valid connection to database. Database mode is not possible.")
    databaseMode <- FALSE
  }
}

##### looking for pre merged file #############  
if (!exists("shinySettings")) {
  locationOfPremergedFile = file.path(defaultLocalDataFolder, defaultLocalDataFile)
  if (file.exists(locationOfPremergedFile)) {
    foundPremergedFile <- TRUE
    writeLines(text = "Found premerged file.")
  } else {
    foundPremergedFile <- FALSE
    writeLines(text = "Did not find premerged file.")
  }
} else {
  if (!is.null(x = shinySettings$dataFolder) &&
      !is.null(x = shinySettings$dataFile)) {
    locationOfPremergedFile = file.path(shinySettings$dataFolder, shinySettings$dataFile)
    if (file.exists(locationOfPremergedFile)) {
      foundPremergedFile <- TRUE
      writeLines(text = "Found premerged file.")
    } else {
      writeLines(text = "Did not find premerged file.")
      foundPremergedFile <- FALSE
    }
  }
}
  
##### About #############   
aboutText <- defaultAboutTextPhenotypeLibrary
if (exists("shinySettings") && !is.null(shinySettings$aboutText)) {
  aboutText <- shinySettings$aboutText
}

if (databaseMode && !foundPremergedFile) {
  writeLines("App will run in pure database mode.")
}
if (databaseMode && foundPremergedFile) {
  writeLines("App will run in local mode. TO DO: hybrid mode using both premerged files and vocabulary tables in database.")
  dataSource <-
    createFileDataSource(premergedDataFile = locationOfPremergedFile, envir = .GlobalEnv)
}
if (!databaseMode && foundPremergedFile) {
  writeLines("App will run in local file mode.")
  dataSource <-
    createFileDataSource(premergedDataFile = locationOfPremergedFile, envir = .GlobalEnv)
}
if (!databaseMode && !foundPremergedFile) {
  stop("App cannot run.")
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


if (isValidConnection && databaseMode && !foundPremergedFile) {
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
  
  for (table in c(dataModelSpecifications$tableName)) {
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
    dplyr::arrange(.data$cohortId) 
  # %>%
  #   dplyr::mutate(cohortName = stringr::str_remove(.data$cohortName, "\\[.+?\\] "))
  
  if ('metadata' %in% colnames(cohort)) {
    cohortMetaData <- list()
    for (i in 1:nrow(cohort)) {
      x <- RJSONIO::fromJSON(cohort[i, ]$metadata)
      if (length(names(x)) > 0) {
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
  stop("Cohort table not found in data source")
}
if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
  cohort <- cohort %>% 
    dplyr::left_join(y = phenotypeDescription %>% 
                       dplyr::select(.data$phenotypeId, .data$phenotypeName),
                     by = "phenotypeId")
} else {
  cohort$phenotypeName <- ""
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
  } else {
    cohort$cohortType <- ""
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

