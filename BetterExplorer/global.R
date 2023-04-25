source("dataPulls.R")

# set up BETTER connections..... 

# use the Eumaeus app user credentials
connectionPoolBetter <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                                     dbms = "postgresql",
                                     server = paste(Sys.getenv("shinydbServer"),
                                                    Sys.getenv("shinydbDatabase"),
                                                    sep = "/"),
                                     port = Sys.getenv("shinydbPort"),
                                     # user = Sys.getenv("shinydbUser"),
                                     # password = Sys.getenv("shinydbPw"))
                                     user = Sys.getenv("eumaeusdbUser"),
                                     password = Sys.getenv("eumaeusdbPw"))

## credentials for local testing
# connectionPoolBetter <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
#                                      dbms = "postgresql",
#                                server = paste(keyring::key_get("betterServer"),
#                                               keyring::key_get("betterDatabase"),
#                                               sep = "/"),
#                                user = keyring::key_get("betterUser"),
#                                password = keyring::key_get("betterPassword"))

schema <- "better_results"

analysis <- loadEntireTable(connectionPoolBetter, schema, "analysis")
database <- loadEntireTable(connectionPoolBetter, schema, "database")
exposure <- loadEntireTable(connectionPoolBetter, schema, "exposure")
positiveControlOutcome <- loadEntireTable(connectionPoolBetter, schema, "all_ipcs")

trueRrs <- c("Any", 1, unique(positiveControlOutcome$effectSize))

mses = loadEntireTable(connectionPoolBetter, schema, "mses")
#priors = loadEntireTable(connectionPoolBetter, schema, "priors")
#type1s = loadEntireTable(connectionPoolBetter, schema, "type1s")
#tts = loadEntireTable(connectionPoolBetter, schema, "time_to_signal")

## filter analysis to exclude "filtered" Historical Comparator results
analysis = analysis %>% filter(!(method == 'HistoricalComparator' & analysisId >= 13))
sensitivity_levels = c(.25, 0.5)

# close pool when session ends
onStop(function() {
  if (DBI::dbIsValid(connectionPoolBetter)) {
    writeLines("Closing connection pool")
    pool::poolClose(connectionPoolBetter)
    #pool::poolClose(connectionPool)
  }
})

