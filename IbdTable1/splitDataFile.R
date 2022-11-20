rm(list = ls())

setwd("~/temp/IbdCharacterization/")
renv::restore()

splitLargeFile <- function(dataFolder, dataFile) {
  load(file.path(dataFolder, dataFile))
  tableNames <- c("cohort", "covariate", "cohortCount", "featureProportions", "cohortStagingCount", "database") # + "covariateValue"
  
  batchSize <- 1e7
  
  save(list = tableNames, file = file.path(dataFolder, paste0("PreMerged_0.RData")), compress = TRUE)
  
  # split covariateValue
  for (i in (1:ceiling(nrow(covariateValue)/batchSize))) {
    subCovValue <- covariateValue[((i-1)*batchSize + 1) : (i*batchSize), ]
    save(subCovValue, file = file.path(dataFolder, paste0("PreMerged_", i, ".RData")), compress = TRUE)
  }
}

launchShinyApp <- function(outputFolder, 
                           shinySettings = list(storage = "filesystem", 
                                                dataFolder = outputFolder, 
                                                dataFile = "PreMerged.RData")) 
{
  appDir <- system.file("shiny/IbdCharacterizationResultsExplorer", package = getThisPackageName(), mustWork = TRUE)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir)
}


library(IbdCharacterization)

setwd("/Users/cyanover/OneDrive - KI/IBD/")
# dataFolder <- "results_v1/subset"
dataFolder <- "results_v1/"

# characteristics results
preMergeResultsFiles(dataFolder = dataFolder)
dataFile <- "PreMerged.RData"
splitLargeFile(dataFolder, dataFile)

rm(shinySettings, envir = .GlobalEnv)
# shiny::runApp("/Users/cyanover/GitHub/ShinyDeploy/IbdCharacterization/")

# diagnostics
diagnosticsFolder <- "results_v1/diagnostics/CD"
preMergeDiagnosticsFiles(file.path(getwd(), diagnosticsFolder))
# shiny::runApp("/Users/cyanover/GitHub/ShinyDeploy/CrohnsDiseaseCohortDiagnostics/")

diagnosticsFolder <- "results_v1/diagnostics/UC/"
preMergeDiagnosticsFiles(file.path(getwd(), diagnosticsFolder))
# shiny::runApp("/Users/cyanover/GitHub/ShinyDeploy/UlcerativeColitisCohortDiagnostics/")
