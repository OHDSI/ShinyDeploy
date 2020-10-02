library(shiny)
library(pool)
library(DatabaseConnector)
library(DBI)
source("DataPulls.R")

connPool <- NULL # Will be initialized if using a DB

#connection settings
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("shinydbServer"),
                                                            Sys.getenv("shinydbDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("shinydbPort"),
                                             user = Sys.getenv("shinydbUser"),
                                             password = Sys.getenv("shinydbPw"))


connPool <- connect(connectionDetails)

