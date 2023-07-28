dataFolder <- "G:/motherinfantevaluation2/shinyData"

covarRef <- readRDS(file.path(dataFolder, "covariateRef.rds"))
covarRef <- covarRef[!duplicated(covarRef), ]

analysisRef <- readRDS(file.path(dataFolder, "analysisRef.rds"))
analysisRefInfant <- analysisRef[analysisRef$startDay == 0 | is.na(analysisRef$startDay), ]
analysisRefMother <- analysisRef[analysisRef$startDay != 0 | is.na(analysisRef$startDay), ]

infant <- readRDS(file.path(dataFolder, "infantLinkedNotLinked.rds"))
infant <- merge(infant, covarRef)
infant <- merge(infant, analysisRefInfant)

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


