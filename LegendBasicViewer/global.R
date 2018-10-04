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

# sql <- "SELECT target_id,\n    comparator_id,\n    outcome_id,\n    cm_interaction_result.analysis_id,\n    cohort_method_analysis.description AS analysis_description,\n    cm_interaction_result.database_id,\n    interaction_covariate_id,\n    covariate_name AS interaction_covariate_name,\n    rrr,\n    ci_95_lb,\n    ci_95_ub,\n    p,\n    calibrated_p,\n    i_2,\n    log_rrr,\n    se_log_rrr,\n    target_subjects,\n    comparator_subjects,\n    target_days,\n    comparator_days,\n    target_outcomes,\n    comparator_outcomes\n  FROM cm_interaction_result\n  INNER JOIN covariate\n  ON cm_interaction_result.interaction_covariate_id = covariate.covariate_id\n  AND cm_interaction_result.database_id = covariate.database_id\n  INNER JOIN cohort_method_analysis\n  ON cm_interaction_result.analysis_id = cohort_method_analysis.analysis_id WHERE target_id IN (18) AND comparator_id IN (17) AND outcome_id IN (24) AND cm_interaction_result.database_id IN ('JMDC')"
# x <- querySql(connection, sql)
# nrow(x)


# head(x)
# 
# 
# sql <- "SELECT DISTINCT(outcome_id) FROM covariate_balance"
