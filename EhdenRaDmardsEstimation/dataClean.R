outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

exposureOfInterest$definition <- NULL
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]

keeps <- ((cohortMethodResult$outcomeId %in% c(187, 193, 197, 203, 253) & cohortMethodResult$analysisId %in% c(7:10)) | # infections, leukopenia, pancytopenia
            (cohortMethodResult$outcomeId %in% c(182:185) & cohortMethodResult$analysisId %in% c(1, 2, 4, 5)) | # cardiovacular
            (cohortMethodResult$outcomeId %in% c(212, 223, 216, 218) & cohortMethodResult$analysisId %in% c(3, 6)) | # oncology except lung cancer
            cohortMethodResult$outcomeId %in% negativeControlOutcome$outcomeId) & # controls
  cohortMethodResult$comparatorId == 219 # keep only methotrexate as comparator
cohortMethodResult <- cohortMethodResult[keeps, ]

toBlind <- readRDS(file.path(dataFolder, "to_blind.rds"))
toBlind <- toBlind[, c("database_id", "analysis_id", "target_id", "comparator_id")] 
toBlind$to_blind <- 1
colnames(toBlind) <- SqlRender::snakeCaseToCamelCase(colnames(toBlind))

cohortMethodResult <- merge(cohortMethodResult, toBlind, all.x = TRUE)
cohortMethodResult$toBlind[is.na(cohortMethodResult$toBlind)] <- 0

dbBlinds <- (cohortMethodResult$databaseId %in% c("DABelgium", "DAGermany", "THIN", "PanTher", "IPCI") & cohortMethodResult$analysisId %in% c(1,4,7,9)) |
  (cohortMethodResult$databaseId %in% c("AmbEMR", "THIN") & cohortMethodResult$outcomeId == 203)

cohortMethodResult$rr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$ci95Ub[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$ci95Lb[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$logRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$seLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$p[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedCi95Ub[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedCi95Lb[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedSeLogRr[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA
cohortMethodResult$calibratedP[cohortMethodResult$toBlind == 1 | dbBlinds] <- NA

cohortMethodResult$targetDays[dbBlinds] <- NA
cohortMethodResult$comparatorDays[dbBlinds] <- NA
cohortMethodResult$targetOutcomes[dbBlinds] <- NA
cohortMethodResult$comparatorOutcomes[dbBlinds] <- NA

dbBlinds <- cmFollowUpDist$databaseId %in% c("DABelgium", "DAGermany", "THIN", "PanTher", "IPCI") & cmFollowUpDist$analysisId %in% c(1,4,7,9)
cmFollowUpDist$targetMinDays[dbBlinds] <- NA
cmFollowUpDist$targetP10Days[dbBlinds] <- NA
cmFollowUpDist$targetP25Days[dbBlinds] <- NA
cmFollowUpDist$targetMedianDays[dbBlinds] <- NA
cmFollowUpDist$targetP75Days[dbBlinds] <- NA
cmFollowUpDist$targetP90Days[dbBlinds] <- NA
cmFollowUpDist$targetMaxDays[dbBlinds] <- NA
cmFollowUpDist$comparatorMinDays[dbBlinds] <- NA
cmFollowUpDist$comparatorP10Days[dbBlinds] <- NA
cmFollowUpDist$comparatorP25Days[dbBlinds] <- NA
cmFollowUpDist$comparatorMedianDays[dbBlinds] <- NA
cmFollowUpDist$comparatorP75Days[dbBlinds] <- NA
cmFollowUpDist$comparatorP90Days[dbBlinds] <- NA
cmFollowUpDist$comparatorMaxDays[dbBlinds] <- NA

cohortMethodResult$i2 <- round(cohortMethodResult$i2, 2)

exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[EHDEN RA] New users of hydroxychloroquine monotherapy"] <- "Hydroxychloroquine"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[EHDEN RA] New users of leflunomide monotherapy"] <- "Leflunomide"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[EHDEN RA] New users of sulfasalazine monotherapy"] <- "Sulfasalazine"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "[EHDEN RA] New users of methoxtrexate monotherapy"] <- "Methoxtrexate"

outcomeOfInterest <- outcomeOfInterest[outcomeOfInterest$outcomeName != "[EHDEN RA] Persons with a Malignant neoplasm of breast 1 dx", ]
outcomeOfInterest$outcomeName <- gsub("\\[EHDEN RA\\] ", "", outcomeOfInterest$outcomeName)
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Acute myocardial infarction events (in any visit)"] <- "Acute myocardial infarction (any visit)"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Acute myocardial infarction events (in hospital/ER visit)"] <- "Acute myocardial infarction (IP/ER visit)"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Stroke (ischemic or hemorrhagic) events (any visit)"] <- "Ischemic or hemorrhagic stroke (any visit)"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Stroke (ischemic or hemorrhagic) events (in hospital/ER visit)"] <- "Ischemic or hemorrhagic stroke (IP/ER visit)"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Persons with a Malignant neoplasm of leukemia 1 dx"] <- "Leukemia"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Persons with a malignant neoplasm other than non-melanoma skin cancer 1 dx"] <- "Any cancer except non-melanoma skin cancer"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Persons with a Malignant neoplasm of lymphoma 1 dx"] <- "Lymphoma"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Persons with a Malignant neoplasm of colon and rectum 1 dx"] <- "Colorectal cancer"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Persons with a Malignant neoplasm of lung 1 dx"] <- "Lung cancer"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Opportunistic Infections"] <- "Opportunistic Infection"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Serious Infection  events"] <- "Serious Infection"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Serious Infection, opportunistic infections and other infections of interest event"] <- "Serious, opporunistic, or other infection"

outcomeOfInterest$order <- match(outcomeOfInterest$outcomeName, c("Leukopenia",
                                                                  "Pancytopenia",
                                                                  "Serious Infection",
                                                                  "Opportunistic Infection",
                                                                  "Serious, opporunistic, or other infection",
                                                                  "Acute myocardial infarction (any visit)",
                                                                  "Acute myocardial infarction (IP/ER visit)",
                                                                  "Ischemic or hemorrhagic stroke (any visit)",
                                                                  "Ischemic or hemorrhagic stroke (IP/ER visit)",
                                                                  "Any cancer except non-melanoma skin cancer",
                                                                  "Leukemia",
                                                                  "Lymphoma",
                                                                  "Colorectal cancer",
                                                                  "Lung cancer"))
outcomeOfInterest <- outcomeOfInterest[order(outcomeOfInterest$order), ]
outcomeOfInterest$order <- NULL

dbOrder <- c("AmbEMR", "CCAE", "ClinFormatics", "DABelgium", "DAGermany", "EstonianHIS", "ICPI", "JMDC", "LPDFrance", "MDCD", "MDCR", "OptumEHR", "SIDIAP", "THIN", "Meta-analysis")
database$order <- match(database$databaseId, dbOrder)
database <- database[order(database$order), ]
database$order <- NULL
outcomeOfInterest <- outcomeOfInterest[outcomeOfInterest$outcomeId != 201, ]

cohortMethodAnalysis$description[cohortMethodAnalysis$description == "01. No prior outcome in last 365d, on-treatment with 14d surveillance window, 5 PS strata, conditional Cox PH"] <- "No prior outcome (365d), On-treatment +14d, 5 PS strata"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "02. No prior outcome in last 365d, intent-to-treat to 1826d, 5 PS strata, conditional Cox PH"] <- "No prior outcome (365d), Intent-to-treat 5y, 5 PS strata" 
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "03. No prior outcome, delayed onset 365d to 1826d, 5 PS strata, conditional Cox PH"] <- "No prior outcome (ever), Delayed onset 1-5y, 5 PS strata"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "04. No prior outcome in last 365d, on-treatment with 14d surveillance window, 1:1 PS matching, unconditional Cox PH"] <- "No prior outcome (365d), On-treatment +14d, 1:1 PS match"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "05. No prior outcome in last 365d, intent-to-treat to 1826d, 1:1 PS matching, unconditional Cox PH"] <- "No prior outcome (365d), Intent-to-treat 5y, 1:1 PS match"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "06. No prior outcome, delayed onset 365d to 1826d, 1:1 PS matching, unconditional Cox PH"] <- "No prior outcome (ever), Delayed onset 1-5y, 1:1 PS match"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "07. No prior outcome in last 30d, on-treatment with 14d surveillance window, 5 PS strata, conditional Cox PH"] <- "No prior outcome ( 30d), On-treatment +14d, 5 PS strata"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "08. No prior outcome in last 30d, intent-to-treat to 1826d, 5 PS strata, conditional Cox PH"] <- "No prior outcome ( 30d), Intent-to-treat 5y, 5 PS strata"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "10. No prior outcome in last 30d, on-treatment with 14d surveillance window, 1:1 PS matching, unconditional Cox PH"] <- "No prior outcome ( 30d), On-treatment +14d, 1:1 PS match"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "11. No prior outcome in last 30d, intent-to-treat to 1826d surveillance window, 1:1 PS matching, unconditional Cox PH"] <- "No prior outcome ( 30d), Intent-to-treat 5y, 1:1 PS match"

cohortMethodAnalysis$order <- match(cohortMethodAnalysis$description, c("No prior outcome (365d), On-treatment +14d, 5 PS strata",
                                                                        "No prior outcome (365d), Intent-to-treat 5y, 5 PS strata",
                                                                        "No prior outcome (365d), On-treatment +14d, 1:1 PS match",
                                                                        "No prior outcome (365d), Intent-to-treat 5y, 1:1 PS match",
                                                                        "No prior outcome ( 30d), On-treatment +14d, 5 PS strata",
                                                                        "No prior outcome ( 30d), Intent-to-treat 5y, 5 PS strata",
                                                                        "No prior outcome ( 30d), On-treatment +14d, 1:1 PS match",
                                                                        "No prior outcome ( 30d), Intent-to-treat 5y, 1:1 PS match",
                                                                        "No prior outcome (ever), Delayed onset 1-5y, 5 PS strata",
                                                                        "No prior outcome (ever), Delayed onset 1-5y, 1:1 PS match"))
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$order), ]
cohortMethodAnalysis$order <- NULL
