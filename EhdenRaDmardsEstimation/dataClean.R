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

outcomeOfInterest$order <- match(outcomeOfInterest$outcomeName, c("Acute myocardial infarction (any visit)",
                                                                  "Acute myocardial infarction (IP/ER visit)",
                                                                  "Ischemic or hemorrhagic stroke (any visit)",
                                                                  "Ischemic or hemorrhagic stroke (IP/ER visit)",
                                                                  "Opportunistic Infection",
                                                                  "Serious Infection",
                                                                  "Serious, opporunistic, or other infection",
                                                                  "Leukopenia",
                                                                  "Pancytopenia",
                                                                  "Leukemia",
                                                                  "Any cancer other than non-melanoma skin cancer",
                                                                  "Lymphoma",
                                                                  "Colorectal cancer",
                                                                  "Lung cancer"))
outcomeOfInterest <- outcomeOfInterest[order(outcomeOfInterest$order), ]
outcomeOfInterest$order <- NULL

database$order <- match(database$databaseId, c(database$databaseId[database$databaseId != "Meta-analysis"], "Meta-analysis"))
database <- database[order(database$order), ]
database$order <- NULL

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
