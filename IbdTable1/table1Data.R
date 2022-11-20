setwd("/Users/cyanover/OneDrive - KI/IBD/")

rm(list = ls())

dataFolder <- "results_v1/"
dataFile <- "PreMerged.RData"

load(file.path(dataFolder, dataFile))

covariteIdToInclude <- c(covariate$covariateId[covariate$covariateAnalysisId == 1], # gender
                          covariate$covariateId[covariate$covariateAnalysisId == 3], # age group
                          covariate$covariateId[grep("cohort during day -365 through -1 days", covariate$covariateName)] # cohorts, 1Y before index 
)

covariate <- covariate[covariate$covariateId %in% covariteIdToInclude, ]
covariateValue <- covariateValue[covariateValue$covariateId %in% covariteIdToInclude, ]

tableNames <- c("cohort", "covariate", "cohortCount", "featureProportions", "cohortStagingCount", "database", "covariateValue")
save(list = tableNames, file = file.path(dataFolder, paste0("PreMerged_table1.RData")), compress = TRUE)

# renv::deactivate()
shiny::runApp("/Users/cyanover/GitHub/ShinyDeploy/IbdTable1/")

