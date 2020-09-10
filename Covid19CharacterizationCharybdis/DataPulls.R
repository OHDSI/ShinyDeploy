loadDataFromDB <- function(connPool, networkSchema = Sys.getenv("charybdisdbSchema")) {
  loadDataToGlobalEnv("cohort", queryTable(connPool, networkSchema, "cohort"))
  loadDataToGlobalEnv("cohortCount", queryTable(connPool, networkSchema, "cohort_count"))
  loadDataToGlobalEnv("database", queryTable(connPool, networkSchema, "database"))
  loadDataToGlobalEnv("covariate", queryTable(connPool, networkSchema, "covariate"))
  loadDataToGlobalEnv("covariateValue", queryTable(connPool, networkSchema, "covariate_value", limit = 1))
}

loadDataToGlobalEnv <- function(x, value) {
  assign(x, value, envir = .GlobalEnv)
}

getCovariateValue <- function(connPool, networkSchema = Sys.getenv("charybdisdbSchema"), cohortId, databaseList, comparatorCohortId = NULL) {
  if (cohortId > 0 && length(databaseList > 0)) {
    cohortSearchClause <- ifelse(is.null(comparatorCohortId), "cohort_id = @cohort_id", "cohort_id IN (@cohort_id, @comparator_cohort_id)")
    sql <- paste0("SELECT * FROM @network_schema.covariate_value WHERE ", cohortSearchClause, " AND database_id IN (@database_list)")
    sql <- SqlRender::render(sql, 
                             network_schema = networkSchema, 
                             cohort_id = cohortId,
                             comparator_cohort_id = comparatorCohortId,
                             database_list = paste0("'", databaseList, "'"), warnOnMissingParameters = FALSE)
    #print(sql)
    data <- queryDb(connPool, sql)
  } else {
    data <- queryTable(connPool, networkSchema, "covariate_value", limit = 0)
  }
  return(data)
}

queryTable <- function(connPool, networkSchema, tableName, limit = -1) {
  sql <- "SELECT * FROM @network_schema.@table_name"
  if (limit >= 0) {
    sql <- paste(sql, "LIMIT", limit)
  }
  sql <- SqlRender::render(sql, network_schema = networkSchema, table_name = tableName)
  data <- queryDb(connPool, sql)
  return(data)
}

queryDb <- function(connPool, sql) {
  data <- DatabaseConnector::dbGetQuery(connPool, sql)
  colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
  return(data)  
}
