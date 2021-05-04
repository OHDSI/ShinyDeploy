insertSqlData <- function(table, conn, primaryKey = NULL, ...){
  arguments <- list(...)
  values <- paste(arguments, collapse = "','")
  if(!is.null(primaryKey)){
  sql <- paste0("INSERT INTO ", table, " OUTPUT INSERTED.",primaryKey," VALUES ('", values,"\');")
  } else {
    sql <- paste0("INSERT INTO ", table, " VALUES ('", values,"\');")
  }
  res <- DBI::dbGetQuery(conn = conn, sql)
  return(res)
}