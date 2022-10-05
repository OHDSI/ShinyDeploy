rm(list = ls())
setwd("C:/Users/EndUser/Documents/GitHub/ShinyDeploy/IbdCharacterization/")

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


dataFolder <- "data"
dataFile <- "PreMerged.RData"
splitLargeFile(dataFolder, dataFile)