library(magrittr)
library(purrr)

source("HelperFunctions.R")

# Used for Testing with old DB connection
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(
      Sys.getenv("shinyDbServer"),
      Sys.getenv("shinydbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUser"),
    password = Sys.getenv("shinyDbPassword"),
    port = Sys.getenv("shinydbPort")
  )

connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

defaultVocabularySchema <- "vocabulary"

vocabularyVersion <-
  ConceptSetDiagnostics::getVocabulary(connection = connection,
                                       vocabulary = defaultVocabularySchema) %>%
  dplyr::filter(.data$vocabularyId == 'None') %>%
  dplyr::pull(.data$vocabularyVersion)
