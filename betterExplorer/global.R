source("dataPulls.R")

connectionPool <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                               dbms = "postgresql",
                               server = paste(Sys.getenv("shinydbServer"),
                                              Sys.getenv("shinydbDatabase"),
                                              sep = "/"),
                               port = Sys.getenv("shinydbPort"),
                               user = Sys.getenv("eumaeusdbUser"),
                               password = Sys.getenv("eumaeusdbPw"))

# OLD: credentials used for local testing...
# connectionPool <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
#                                dbms = "postgresql",
#                                server = paste(keyring::key_get("eumaeusServer"),
#                                               keyring::key_get("eumaeusDatabase"),
#                                               sep = "/"),
#                                user = keyring::key_get("eumaeusUser"),
#                                password = keyring::key_get("eumaeusPassword"))


# onStop(function() {
#   if (DBI::dbIsValid(connectionPool)) {
#     writeLines("Closing connection pool")
#     pool::poolClose(connectionPool)
#   }
# })

schema <- "eumaeus"

analysis <- loadEntireTable(connectionPool, schema, "analysis")
database <- loadEntireTable(connectionPool, schema, "database")
exposure <- loadEntireTable(connectionPool, schema, "exposure")
negativeControlOutcome <- loadEntireTable(connectionPool, schema, "negative_control_outcome")
positiveControlOutcome <- loadEntireTable(connectionPool, schema, "positive_control_outcome")

trueRrs <- c("Any", 1, unique(positiveControlOutcome$effectSize))


# BETTER connections..... 
# connectionPoolBetter <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
#                                      dbms = "postgresql",
#                                server = paste(keyring::key_get("betterServer"),
#                                               keyring::key_get("betterDatabase"),
#                                               sep = "/"),
#                                user = keyring::key_get("betterUser"),
#                                password = keyring::key_get("betterPassword"))

## try to use Eumaeus credentials instead......
connectionPoolBetter = connectionPool


onStop(function() {
  if (DBI::dbIsValid(connectionPoolBetter)) {
    writeLines("Closing connection pool")
    pool::poolClose(connectionPoolBetter)
  }
})

schema <- "better_results"

mses = loadEntireTable(connectionPoolBetter, schema, "mses")
priors = loadEntireTable(connectionPoolBetter, schema, "priors")
#type1s = loadEntireTable(connectionPoolBetter, schema, "type1s")
#tts = loadEntireTable(connectionPoolBetter, schema, "time_to_signal")

## filter analysis to exclude "filtered" Historical Comparator results
analysis = analysis %>% filter(!(method == 'HistoricalComparator' & analysisId >= 13))
sensitivity_levels = c(.25, 0.5)

