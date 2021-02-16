isConnectionValid <- function(dbms,
                              server,
                              port,
                              username,
                              password) {  
  DBI::dbCanConnect(drv = DatabaseConnector::DatabaseConnectorDriver(),
                    dbms = dbms,
                    server = server,
                    port = port,
                    user = username,
                    password = password) 
}

loadRequiredTables <- function(databaseSchema,
                               tableName, 
                               required = FALSE,
                               connection) {
  sql <- "SELECT * FROM @databaseSchema."
  if (required) {
    tryCatch(expr = {
      table <- DatabaseConnector::dbReadTable(conn = connection, 
                                              name = paste(databaseSchema, tableName, sep = "."))
    }, error = function(err) {
      stop("Error reading from ", paste(databaseSchema, tableName, sep = "."), ": ", err$message)
    })} else {
      table <- DatabaseConnector::dbReadTable(conn = connection, 
                                              name = paste(databaseSchema, tableName, sep = "."))
    }
  colnames(table) <- snakeCaseToCamelCase(colnames(table))
  table <- dplyr::tibble(table)
  if (nrow(table) > 0) {
    assign(snakeCaseToCamelCase(tableName), table, envir = .GlobalEnv)
  }
}
