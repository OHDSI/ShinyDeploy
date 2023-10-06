dataFolder <- "G:/motherinfantevaluation2/shinyData"

covarRef <- readRDS(file.path(dataFolder, "covariateRef.rds"))
covarRef <- covarRef[!duplicated(covarRef), ]

analysisRef <- readRDS(file.path(dataFolder, "analysisRef.rds"))
analysisRefInfant <- analysisRef[analysisRef$startDay == 0 | is.na(analysisRef$startDay), ]
analysisRefMother <- analysisRef[analysisRef$startDay != 0 | is.na(analysisRef$startDay), ]

infant <- readRDS(file.path(dataFolder, "infantLinkedNotLinked.rds"))
infant <- merge(infant, covarRef)
infant <- merge(infant, analysisRefInfant)

linkedInfantsPostObs <- dplyr::bind_rows(
  readr::read_csv("G:/motherinfantevaluation2/ccaeLinkedPostObsTimePatch.csv", show_col_types = FALSE),
  readr::read_csv("G:/motherinfantevaluation2/optumLinkedPostObsTimePatch.csv", show_col_types = FALSE)
)

# T3
# ccae
ccaeT3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "primary"]
ccaeT3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "primary"]
ccaeT3StdDiff <- -0.106

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeT3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeT3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeT3StdDiff

# optum
optumT3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "primary"]
optumT3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "primary"]
optumT3StdDiff <- -0.091

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumT3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumT3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "All pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumT3StdDiff


# E3
# ccae
ccaeE3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "first"]
ccaeE3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "first"]
ccaeE3StdDiff <- -0.067

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeE3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeE3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "truven_ccae"] <- ccaeE3StdDiff

# optum
optumE3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "first"]
optumE3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "first"]
optumE3StdDiff <- -0.009

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumE3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumE3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "First pregnancies - 60d" & infant$databaseId == "optum_extended_dod"] <- optumE3StdDiff


# F3
# ccae
ccaeF3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "90d"]
ccaeF3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "CCAE" & linkedInfantsPostObs$analysis == "90d"]
ccaeF3StdDiff <- -0.113

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "truven_ccae"] <- ccaeF3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "truven_ccae"] <- ccaeF3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "truven_ccae"] <- ccaeF3StdDiff

# optum
optumF3PostObsMean <- linkedInfantsPostObs$mean[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "90d"]
optumF3PostObsSd <- linkedInfantsPostObs$sd[linkedInfantsPostObs$db == "Optum" & linkedInfantsPostObs$analysis == "90d"]
optumF3StdDiff <- -0.07

infant$mean1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "optum_extended_dod"] <- optumF3PostObsMean
infant$sd1[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "optum_extended_dod"] <- optumF3PostObsSd
infant$stdDiff[infant$covariateId == 1009 & infant$analysisVariant == "All pregnanices - 90d" & infant$databaseId == "optum_extended_dod"] <- optumF3StdDiff


motherPregStart <- readRDS(file.path(dataFolder, "motherLinkedNotLinkedPregStart.rds"))
motherPregStart <- merge(motherPregStart, covarRef)
motherPregStart <- merge(motherPregStart, analysisRefMother)
motherPregStart <- motherPregStart[motherPregStart$covariateId != 8507001, ]
motherPregStart <- motherPregStart[motherPregStart$covariateId != 8532001, ]

motherPregEnd <- readRDS(file.path(dataFolder, "motherLinkedNotLinkedPregEnd.rds"))
motherPregEnd <- merge(motherPregEnd, covarRef)
motherPregEnd <- merge(motherPregEnd, analysisRefMother)
motherPregEnd <- motherPregEnd[motherPregEnd$covariateId != 8507001, ]
motherPregEnd <- motherPregEnd[motherPregEnd$covariateId != 8532001, ]

databaseRef <- unique(infant$databaseId)
domainRef <- unique(analysisRef$domainId)

counts <- readRDS(file.path(dataFolder, "cohortCounts.rds"))


rm(covarRef, analysisRef, analysisRefInfant, analysisRefMother)

dfs <- Filter(function(x) is.data.frame(get(x)) , ls())
save(list = dfs,
     file = file.path(dataFolder, "PreMergedShinyData.RData"),
     compress = TRUE,
     compression_level = 2)


