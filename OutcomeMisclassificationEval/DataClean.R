

# outcomeOfInterest$definition <- NULL
# outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]
# outcomeOfInterest$outcomeName <- "Severe uterine bleed"

  
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "QBA, 183d"] <- "Simple QBA, 183d"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "PS matched, QBA, 183d"] <- "PS matched, simple QBA, 183d"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "QBA, 365d"] <- "Simple QBA, 365d"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "PS matched, QBA, 365d"] <- "PS matched, simple QBA, 365d"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "QBA, 730d"] <- "Simple QBA, 730d"
cohortMethodAnalysis$description[cohortMethodAnalysis$description == "PS matched, QBA, 730d"] <- "PS matched, simple QBA, 730d"

cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "QBA, 183d"] <- "Simple QBA, 183d"
cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "PS matched, QBA, 183d"] <- "PS matched, simple QBA, 183d"
cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "QBA, 365d"] <- "Simple QBA, 365d"
cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "PS matched, QBA, 365d"] <- "PS matched, simple QBA, 365d"
cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "QBA, 730d"] <- "Simple QBA, 730d"
cohortMethodResult$analysisDescription[cohortMethodResult$analysisDescription == "PS matched, QBA, 730d"] <- "PS matched, simple QBA, 730d"