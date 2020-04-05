outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

cohortMethodResult$i2 <- round(cohortMethodResult$i2, 2)

drops <- 
  (cohortMethodResult$databaseId == "PanTher" & cohortMethodResult$analysisId == 1) | # panther on-treatment
  (cohortMethodResult$databaseId %in% c("CCAE", "DAGermany", "JMDC", "MDCD", "MDCR", "PanTher", "OpenClaims", "AmbEMR") & cohortMethodResult$outcomeId %in% c(18, 19)) | # death
  (cohortMethodResult$databaseId %in% c("AmbEMR", "CPRD", "DAGermany", "IMRD", "SIDIAP") & cohortMethodResult$outcomeId %in% c(22, 13, 20, 21, 17, 8, 11)) # databases with no IP
cohortMethodResult <- cohortMethodResult[!drops, ]

database$order <- match(database$databaseId, c(database$databaseId[database$databaseId != "Meta-analysis"], "Meta-analysis"))
database <- database[order(database$order), ]
database$order <- NULL

blinds <-
  (cohortMethodResult$databaseId == "CPRD" & cohortMethodResult$targetId == 137) |
  (cohortMethodResult$databaseId == "JMDC" & cohortMethodResult$targetId == 2) |
  (cohortMethodResult$databaseId == "DAGermany" & cohortMethodResult$targetId == 137) |
  (cohortMethodResult$databaseId == "IMRD" & cohortMethodResult$targetId == 137) |
  (cohortMethodResult$databaseId == "SIDIAP" & cohortMethodResult$targetId %in% c(137, 2))
  (cohortMethodResult$databaseId == "ICPI" & cohortMethodResult$targetId %in% c(137, 2))

cohortMethodResult$rr[blinds] <- NA
cohortMethodResult$ci95Ub[blinds] <- NA
cohortMethodResult$ci95Lb[blinds] <- NA
cohortMethodResult$logRr[blinds] <- NA
cohortMethodResult$seLogRr[blinds] <- NA
cohortMethodResult$p[blinds] <- NA
cohortMethodResult$calibratedRr[blinds] <- NA
cohortMethodResult$calibratedCi95Ub[blinds] <- NA
cohortMethodResult$calibratedCi95Lb[blinds] <- NA
cohortMethodResult$calibratedLogRr[blinds] <- NA
cohortMethodResult$calibratedSeLogRr[blinds] <- NA
cohortMethodResult$calibratedP[blinds] <- NA

outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Persons with chest pain or angina"] <- "Chest pain or angina"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Venous thromboembolic (pulmonary embolism and deep vein thrombosis) events"] <- "Venous thromboembolism"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Acute renal failure events"] <- "Acute renal failure"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Persons with end stage renal disease"] <- "End stage renal disease"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Persons with hepatic failure"] <- "Hepatic failure"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Acute pancreatitis events"] <- "Acute pancreatitis"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Persons with heart failure"] <- "Heart failure"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Total cardiovascular disease events"] <- "Cardiovascular events"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Person with cardiac arrhythmia"] <- "Cardiac arrhythmia"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Persons with bradycardia"] <- "Bradycardia"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Gastrointestinal bleeding events"] <- "Gastrointestinal bleeding"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] All-cause mortality"] <- "All-cause mortality"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Cardiovascular-related mortality"] <- "Cardiovascular-related mortality"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Transient ischemic attack events"] <- "Transient ischemic attack"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Stroke (ischemic or hemorrhagic) events"] <- "Stroke"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "[LEGEND HTN] Acute myocardial infarction events"] <- "Myocardial infarction"
outcomeOfInterest$order <- match(outcomeOfInterest$outcomeName, c("All-cause mortality",
                                                                  "Cardiovascular-related mortality",
                                                                  "Myocardial infarction",
                                                                  "Chest pain or angina",
                                                                  "Heart failure",
                                                                  "Cardiovascular events",
                                                                  "Cardiac arrhythmia",
                                                                  "Bradycardia",
                                                                  "Transient ischemic attack",
                                                                  "Stroke",
                                                                  "Venous thromboembolism",
                                                                  "Gastrointestinal bleeding",
                                                                  "Acute renal failure",
                                                                  "End stage renal disease",
                                                                  "Hepatic failure",
                                                                  "Acute pancreatitis"))
outcomeOfInterest <- outcomeOfInterest[order(outcomeOfInterest$order), ]
outcomeOfInterest$order <- NULL

exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI-Covid19] Hydroxychloroquine + Azithromycin"] <- "Hydroxychloroquine + Azithromycin with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI Cov19] New users of Hydroxychloroquine with prior rheumatoid arthritis"] <- "Hydroxychloroquine with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI-Covid19] Hydroxychloroquine + Amoxicillin"] <- "Hydroxychloroquine + Amoxicillin with prior RA"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[OHDSI Cov19] New users of sulfasazine with prior rheumatoid arthritis"] <- "Sulfasalazine with prior RA"
exposureOfInterest <- exposureOfInterest[order(exposureOfInterest$exposureId), ]

cohortMethodAnalysis$description[cohortMethodAnalysis$description == "No prior outcome in last 30d, 5 PS strata, TAR on-treatment+14d"] <- "5 PS strata, on-treatment + 14 days follow-up"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "No prior outcome in last 30d, 5 PS strata, TAR 30d fixed"] <- "5 PS strata, 30 days follow-up"
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$analysisId, decreasing = TRUE), ]
