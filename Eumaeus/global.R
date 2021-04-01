source("dataPulls.R")

connectionPool <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                               dbms = "postgresql",
                               server = paste(Sys.getenv("shinydbServer"),
                                              Sys.getenv("shinydbDatabase"),
                                              sep = "/"),
                               port = Sys.getenv("shinydbPort"),
                               user = Sys.getenv("eumaeusdbUser"),
                               password = Sys.getenv("eumaeusdbPw"))

onStop(function() {
  if (DBI::dbIsValid(connectionPool)) {
    writeLines("Closing connection pool")
    pool::poolClose(connectionPool)
  }
})

schema <- Sys.getenv("eumaeusdbSchema")

analysis <- loadEntireTable(connectionPool, schema, "analysis")
database <- loadEntireTable(connectionPool, schema, "database")
exposure <- loadEntireTable(connectionPool, schema, "exposure")
negativeControlOutcome <- loadEntireTable(connectionPool, schema, "negative_control_outcome")
positiveControlOutcome <- loadEntireTable(connectionPool, schema, "positive_control_outcome")
timePeriod <- loadEntireTable(connectionPool, schema, "time_period")
databaseCharacterization <- loadEntireTable(connectionPool, schema, "database_characterization")
vaccinations <- getVaccinations(connectionPool, schema)

                                              

# subset <- getEstimates(connection = connectionPool,
#                        schema = schema,
#                        databaseId = "IBM_MDCR",
#                        exposureId = 21184,
#                        timeAtRisk = "1-28")

trueRrs <- c("Overall", 1, unique(positiveControlOutcome$effectSize))
timeAtRisks <- unique(analysis$timeAtRisk)

calibrationInfoHtml <- readChar("calibration.html", file.info("calibration.html")$size)
vaccineInfoHtml <- readChar("vaccine.html", file.info("vaccine.html")$size)
databaseInfoHtml <- readChar("databases.html", file.info("databases.html")$size)
timeAtRiskInfoHtml <- readChar("timeAtRisk.html", file.info("timeAtRisk.html")$size)
trueRrInfoHtml <- readChar("trueRr.html", file.info("trueRr.html")$size)
methodsInfoHtml <- readChar("methods.html", file.info("methods.html")$size)
periodInfoHtml <- readChar("period.html", file.info("period.html")$size)
minimumOutcomesInfoHtml <- readChar("minimumOutcomes.html", file.info("minimumOutcomes.html")$size)
metricInfoHtml <- readChar("metrics.html", file.info("metrics.html")$size)