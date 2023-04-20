schema <- Sys.getenv("dbDiagnosticsdbSchema")

library(dplyr)
library(ShinyAppBuilder)
config <- initializeModuleConfig() %>% 
  addModuleConfig(
    ShinyAppBuilder::createModuleConfig( 
      moduleIcon = "table",
      moduleId = 'EvidenceSynthesis', 
      tabName = 'DbDiagnostic',
      shinyModulePackage = "OhdsiShinyModules",
      moduleUiFunction = "dataDiagnosticViewer",
      moduleServerFunction = "dataDiagnosticServer",
      moduleDatabaseConnectionKeyUsername = 'dbd', 
      moduleInfoBoxFile = "dataDiagnosticHelperFile()",
      resultDatabaseDetails = list(
        tablePrefix = '',
        cmTablePrefix = 'cm_',
        sccsTablePrefix = 'sccs_',
        cgTablePrefix = 'cg_',
        schema = schema,
        databaseMetaData = 'DATABASE_META_DATA'
      ),
      useKeyring = F
    )
  )

# specify the connection to the results database
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'postgresql', 
  user = Sys.getenv("dbDiagnosticsdbUser"), 
  password = Sys.getenv("dbDiagnosticsdbPw"), 
  server = Sys.getenv("dbDiagnosticsdbServer")
)
# create a connection handler using the ResultModelManager package
connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)

# now create the shiny app based on the config file and view the results
# based on the connection 
#strong <- shiny::strong
ShinyAppBuilder::createShinyApp(config = config, connection = connection)