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
metaAnalysisDbIds <- databases$databaseId[databases$isMetaAnalysis == 1]
analyses <- getAnalyses(connection)
subgroups <- getSubgroups(connection)
# Sort for display:
indications <- indications[order(indications$indicationId), ]
exposures <- exposures[order(exposures$exposureName), ]
outcomes <- outcomes[order(outcomes$outcomeName), ]
databases <- databases[order(databases$isMetaAnalysis, databases$databaseId), ]
analyses <- analyses[order(analyses$analysisId), ]
subgroups <- subgroups[order(subgroups$subgroupId), ]


writeLines("Closing connection")
disconnect(connection)






# targetId = 1
# comparatorId = 2
# outcomeId = 62
# analysisIds = 1
# results <- getMainResults(connection = connection,
#                           targetIds = targetId,
#                           comparatorIds = comparatorId,
#                           outcomeIds = outcomeId,
#                           databaseIds = c(),
#                           analysisIds = analysisIds)

# balanceSummary <- getCovariateBalanceSummary(connection = connection,
#                                                                        targetId = 1,
#                                                                        comparatorId = 2)
# 
# 
# balance <- getCovariateBalance(connection = connection,
#                                              targetId = 1,
#                                              comparatorId = 2,
#                                databaseId = "CCAE",
#                                analysisId = 2)
