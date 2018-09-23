library(DatabaseConnector)
source("DataPulls.R")
source("PlotsAndTables.R")

# connectionDetails <- createConnectionDetails(dbms = 'postgresql', server = 'localhost/ohdsi', user
# = 'postgres', password = Sys.getenv('pwPostgres'), schema = 'legend')

# connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                              server = paste(Sys.getenv("legendServer"),
#                                                             Sys.getenv("legendDatabase"),
#                                                             sep = "/"),
#                                              port = Sys.getenv("legendPort"),
#                                              user = Sys.getenv("legendUser"),
#                                              password = Sys.getenv("legendPw"),
#                                              schema = Sys.getenv("legendSchema"))

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("shinydbServer"),
                                                            Sys.getenv("shinydbDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("shinydbPort"),
                                             user = Sys.getenv("shinydbUser"),
                                             password = Sys.getenv("shinydbPw"),
                                             schema = Sys.getenv("shinydbSchema"))

connection <- connect(connectionDetails)

indications <- getIndications(connection)
exposures <- getExposures(connection)
exposures$exposureGroup[exposures$exposureGroup == "Drug" | exposures$exposureGroup == "Procedure"] <- "Drug or procedure"
exposureGroups <- unique(exposures[, c("indicationId", "exposureGroup")])
outcomes <- getOutcomes(connection)
databases <- getDatabases(connection)
analyses <- getAnalyses(connection)
subgroups <- getSubgroups(connection)

writeLines("Closing connection")
disconnect(connection)