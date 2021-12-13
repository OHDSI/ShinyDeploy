defaultServer <- Sys.getenv("shinydbServer")
defaultDatabase <- Sys.getenv("shinydbDatabase")
defaultPort <- 5432
defaultUser <- Sys.getenv("shinydbUser")
defaultPassword <- Sys.getenv("shinydbPw")
defaultSchema <- "work_packages"

writeLines("Here")

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

      # connectionPool <-
      #   pool::dbPool(
      #     drv = DatabaseConnector::DatabaseConnectorDriver(),
      #     dbms = "postgresql",
      #     server = connectionDetails$server(),
      #     port = connectionDetails$port(),
      #     user = connectionDetails$user(),
      #     password = connectionDetails$password(),
      #     connectionString = connectionDetails$connectionString()
      #   )

    databaseSchema <- shinySettings$databaseSchema

}
