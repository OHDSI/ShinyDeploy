setwd("/Users/cyanover/OneDrive - KI/IBD/")

rm(list = ls())

dataFolder <- "results_v1/"
dataFile <- "PreMerged.RData"

load(file.path(dataFolder, dataFile))

timeCovToInclude <- c(covariate$covariateId[grep("cohort during day -365 through -1 days", covariate$covariateName)], # cohorts, 1Y before index 
                      covariate$covariateId[grep("cohort during day -30 through -1 days", covariate$covariateName)] # cohorts, 1M before index 
)
covariteIdToInclude <- c(covariate$covariateId[covariate$covariateAnalysisId == 1], # gender
                         covariate$covariateId[covariate$covariateAnalysisId == 3], # age group
                         timeCovToInclude
)

covariate <- covariate[covariate$covariateId %in% covariteIdToInclude, ]
covariate$covariateId <- covariate$covariateId - 1 *(covariate$covariateId %in% timeCovToInclude)

covariateValue <- covariateValue[covariateValue$covariateId %in% covariteIdToInclude, ]
covariateValue$covariateId <- covariateValue$covariateId - 1 * (covariateValue$covariateId %in% timeCovToInclude)

featureProportions <- featureProportions[featureProportions$covariateId %in% covariteIdToInclude, ]
featureProportions$covariateId <- featureProportions$covariateId - 1 * (featureProportions$covariateId %in% timeCovToInclude)

tableNames <- c("cohort", "covariate", "cohortCount", "featureProportions", "cohortStagingCount", "database", "covariateValue")
save(list = tableNames, file = file.path(dataFolder, paste0("PreMerged_table1.RData")), compress = TRUE)

# renv::deactivate()
# shiny::runApp("/Users/cyanover/GitHub/ShinyDeploy/IbdTable1/")

