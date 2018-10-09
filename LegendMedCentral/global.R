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
indications <- getIndications(connection)
exposures <- getExposures(connection)
exposures$exposureGroup[exposures$exposureGroup == "Drug" | exposures$exposureGroup == "Procedure"] <- "Drug or procedure"
exposureGroups <- unique(exposures[, c("indicationId", "exposureGroup")])
outcomes <- getOutcomes(connection)
databases <- getDatabases(connection)

# Sort for display:
indications <- indications[order(indications$indicationId), ]
exposures <- exposures[order(exposures$exposureName), ]
outcomes <- outcomes[order(outcomes$outcomeName), ]
databases <- databases[order(databases$isMetaAnalysis, databases$databaseId), ]

writeLines("Closing connection")
disconnect(connection)

