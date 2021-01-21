library(shiny)
library(pool)
library(DatabaseConnector)
library(DBI)
source("DataPulls.R")

connPool <- NULL # Will be initialized if using a DB

connPool <- dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                   dbms = "postgresql",
                   server = paste(Sys.getenv("phoebedbServer"),
                                  Sys.getenv("phoebedb"),
                                  sep = "/"),
                   port = 5432,
                   user = Sys.getenv("phoebedbUser"),
                   password = Sys.getenv("phoebedbPw"))  

# Cleanup the database connPool  when the application stops
onStop(function() {
  if (DBI::dbIsValid(connPool)) {
    writeLines("Closing database pool")
    poolClose(connPool)
  }
})
