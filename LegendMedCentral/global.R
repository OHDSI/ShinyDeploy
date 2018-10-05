library(DatabaseConnector)
source("DataPulls.R")
source("PlotsAndTables.R")

# connectionDetails <- createConnectionDetails(dbms = 'postgresql', server = 'localhost/ohdsi', user
# = 'postgres', password = Sys.getenv('pwPostgres'), schema = 'legend')
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("shinydbServer"),
                                                            Sys.getenv("shinydbDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("shinydbPort"),
                                             user = Sys.getenv("shinydbUser"),
                                             password = Sys.getenv("shinydbPw"),
                                             schema = Sys.getenv("shinydbSchema"))
connection <- connect(connectionDetails)

exposures <- getExposures(connection)
exposures$exposureName <- sapply(exposures$exposureName, uncapitalize)

outcomes <- getOutcomes(connection)
databases <- getDatabases(connection)

writeLines("Closing connection")
disconnect(connection)

