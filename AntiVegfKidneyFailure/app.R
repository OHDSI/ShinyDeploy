#remotes::install_github('ohdsi/OhdsiShinyModules', ref = 'standardization')
schema <- Sys.getenv('antivegfkfdbSchema')

library(dplyr)
library(ShinyAppBuilder)
library(OhdsiShinyModules)
config <- initializeModuleConfig() %>%
  addModuleConfig(
    createDefaultAboutConfig(
      resultDatabaseDetails = list(
      ),
      useKeyring = F
    )
  )  %>%
  addModuleConfig(
    createDefaultCohortGeneratorConfig(
      resultDatabaseDetails = list(
        tablePrefix = 'cg_',
        schema = schema,
        databaseTable = 'DATABASE_META_DATA'
      ),
      useKeyring = F
    )
  ) %>%
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig(
      resultDatabaseDetails = list(
        tablePrefix = 'cd_',
        #cohortTablePrefix = 'cg_',
        schema = schema,
        vocabularyDatabaseSchema = schema,
        #databaseTable = 'DATABASE_META_DATA'
        databaseTable = 'cd_DATABASE'
      ),
      useKeyring = F
    )
  ) %>%
  addModuleConfig(
    createDefaultCharacterizationConfig(
      resultDatabaseDetails = list(
        tablePrefix = 'c_',
        cohortTablePrefix = 'cg_',
        databaseTablePrefix = '',
        schema = schema,
        databaseTable = 'DATABASE_META_DATA',
        incidenceTablePrefix = 'ci_'
      ),
      useKeyring = F
    )
  ) %>%
  addModuleConfig(
    createDefaultPredictionConfig(
      resultDatabaseDetails = list(
        tablePrefix = 'plp_',
        cohortTablePrefix = 'cg_',
        databaseTablePrefix = '',
        schema = schema,
        databaseTable = 'DATABASE_META_DATA'
      ),
      useKeyring = F
    )
  ) %>%
  addModuleConfig(
    createDefaultEstimationConfig(
      resultDatabaseDetails = list(
        tablePrefix = 'cm_',
        cohortTablePrefix = 'cg_',
        databaseTablePrefix = '',
        schema = schema,
        databaseTable = 'DATABASE_META_DATA'
      ),
      useKeyring = F
    )
  ) %>%
  addModuleConfig(
    ShinyAppBuilder::createModuleConfig( 
      moduleIcon = "people-arrows",
      moduleId = 'sscs', 
      tabName = 'SCCS',
      shinyModulePackage = "OhdsiShinyModules",
      moduleUiFunction = "sccsView",
      moduleServerFunction = "sccsServer",
      moduleDatabaseConnectionKeyUsername = 'sccs', 
      moduleInfoBoxFile = "sccsHelperFile()",
      resultDatabaseDetails = list(
        tablePrefix = 'sccs_',
        cohortTablePrefix = 'cg_',
        databaseTablePrefix = '',
        schema = schema,
        databaseTable = 'DATABASE_META_DATA'
      ),
      useKeyring = F
    )
  )# %>%
  #addModuleConfig(
  #  ShinyAppBuilder::createModuleConfig( 
  #    moduleIcon = "object-group",#"meta",
  #    moduleId = 'EvidenceSynthesis', 
  #    tabName = 'Meta',
  #    shinyModulePackage = "OhdsiShinyModules",
  #    moduleUiFunction = "evidenceSynthesisViewer",
  #    moduleServerFunction = "evidenceSynthesisServer",
  #    moduleDatabaseConnectionKeyUsername = 'es', 
  #    moduleInfoBoxFile = "evidenceSynthesisHelperFile()",
  #    resultDatabaseDetails = list(
  #      tablePrefix = 'es_',
  #      cmTablePrefix = 'cm_',
  #      cgTablePrefix = 'cg_',
  #      sccsTablePrefix = 'sccs_',
  #      schema = schema,
  #      databaseMetaData = 'DATABASE_META_DATA'
  #    ),
  #    useKeyring = F
  #  )
  #)

# specify the connection to the results database
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'postgresql', 
  user = Sys.getenv('antivegfkfdbUser'), 
  password = Sys.getenv('antivegfkfdbPw'), 
  server = paste0(Sys.getenv('antivegfkfdbServer'),'/shinydb'),
  port = 5432 
)

# create a connection handler using the ResultModelManager package
connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)

# now create the shiny app based on the config file and view the results
# based on the connection 
ShinyAppBuilder::createShinyApp(config = config, connection = connection)
##ShinyAppBuilder::viewShiny(config = config, connection = connection)
