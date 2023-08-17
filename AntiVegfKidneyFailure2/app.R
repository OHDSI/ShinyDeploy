# create a config settings for the shiny that includes
# the tabs that are necessary for the analysis

#remotes::install_github('ohdsi/OhdsiShinyModules')

schema <- Sys.getenv("antivegfkf2dbSchema")

library(dplyr)
library(ShinyAppBuilder)
library(OhdsiShinyModules)
config <- initializeModuleConfig() %>%
  addModuleConfig(
    createDefaultAboutConfig()
  )  %>%
  # addModuleConfig(
  #   createDefaultDatasourcesConfig()
  # )  %>%
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) %>%
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig()
  ) %>%
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) %>%
  addModuleConfig(
    createDefaultPredictionConfig()
  ) %>%
  addModuleConfig(
    createDefaultCohortMethodConfig()
  ) %>%
  addModuleConfig(
    createDefaultSccsConfig()
  ) %>%
  addModuleConfig(
    createDefaultEvidenceSynthesisConfig()
  )

# Create connection details to results database ---------------
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'postgresql', 
  user = Sys.getenv('antivegfkfdbUser'), 
  password = Sys.getenv('antivegfkfdbPw'), 
  server = paste0(Sys.getenv('antivegfkfdbServer'),'/shinydb'),
  port = 5432 
)

# create a connection handler using the ResultModelManager package
connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)



# create result schema settings
resultDatabaseSettings <- createDefaultResultDatabaseSettings(
  schema = schema
)

# now create the shiny app based on the config file and view the results
# based on the connection 
##ShinyAppBuilder::createShinyApp(config = config, connection = connection)
ShinyAppBuilder::viewShiny(
  config = config, 
  connection = connection, 
  resultDatabaseSettings = resultDatabaseSettings
)
