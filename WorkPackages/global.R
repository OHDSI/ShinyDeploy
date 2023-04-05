defaultServer <- Sys.getenv("shinydbServer")
defaultDatabase <- Sys.getenv("shinydbDatabase")
defaultPort <- 5432
defaultUser <- Sys.getenv("shinydbUser")
defaultPassword <- Sys.getenv("shinydbPw")
defaultSchema <- "work_packages"


# shinySettings <- list(
#   connectionDetails = connectionDetails <- DatabaseConnector::createConnectionDetails(
#     dbms = "postgresql",
#     server = paste(keyring::key_get("workPackagesServer"),
#                    keyring::key_get("workPackagesDatabase"),
#                    sep = "/"),
#     user = keyring::key_get("workPackagesReadOnlyUser"),
#     password = keyring::key_get("workPackagesReadOnlyPassword")),
#   databaseSchema = paste0(keyring::key_get("workPackagesDatabase"), ".work_packages"),
#   dataFile = NULL
# )


# writeLines("Here")

if (!exists("shinySettings")) {
  writeLines("Using default settings")

    connectionPool <- pool::dbPool(
      drv = DatabaseConnector::DatabaseConnectorDriver(),
      dbms = "postgresql",
      server = paste(defaultServer, defaultDatabase, sep = "/"),
      port = defaultPort,
      user = defaultUser,
      password = defaultPassword
    )
    databaseSchema <- defaultSchema

} else {
  writeLines("Using settings provided by user")

    connectionDetails <- shinySettings$connectionDetails
      connectionPool <-
        pool::dbPool(
          drv = DatabaseConnector::DatabaseConnectorDriver(),
          dbms = "postgresql",
          server = connectionDetails$server(),
          port = connectionDetails$port(),
          user = connectionDetails$user(),
          password = connectionDetails$password(),
          connectionString = connectionDetails$connectionString()
        )
    databaseSchema <- shinySettings$databaseSchema
}

onStop(function() {
  if (DBI::dbIsValid(connectionPool)) {
    writeLines("Closing database pool")
    pool::poolClose(connectionPool)
  }
})

loadResultsTable <- function(tableName) {
  # if (required || tableName %in% resultsTablesOnServer) {
  tryCatch({
    table <- DatabaseConnector::dbReadTable(connectionPool,
                                            paste(databaseSchema, tableName, sep = "."))
  }, error = function(err) {
    stop(
      "Error reading from ",
      paste(resultsDatabaseSchema, tableName, sep = "."),
      ": ",
      err$message
    )
  })
  colnames(table) <-
    SqlRender::snakeCaseToCamelCase(colnames(table))
  if (nrow(table) > 0) {
    assign(
      SqlRender::snakeCaseToCamelCase(tableName),
      dplyr::as_tibble(table),
      envir = .GlobalEnv
    )
  }
  # }
}

loadResultsTable("package_descriptions")



