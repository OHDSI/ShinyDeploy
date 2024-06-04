# rm(taBalance)
# rm(covariateBalanceTnaCna, kaplanMeierDistTnaCna, preferenceScoreDistTnaCna)

# View(cohortMethodResult[cohortMethodResult$databaseId == "optum_extended_dod" & cohortMethodResult$analysisId == 3, ])

#cohortMethodResult$i2 <- ifelse(cohortMethodResult$i2 < 0.01, "<0.01", round(cohortMethodResult$i2, 2))
exposureOfInterest <- exposureOfInterest[!duplicated(exposureOfInterest), ]

ncCohortIds <- unique(cohortMethodResult$outcomeId)
ncCohortIds <- ncCohortIds[!ncCohortIds == outcomeOfInterest$outcomeId]

keeps <- (
  (cohortMethodResult$databaseId == "iqvia_amb_emr"          & cohortMethodResult$analysisId == 1 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_amb_emr"          & cohortMethodResult$analysisId == 2 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_amb_emr"          & cohortMethodResult$analysisId == 4 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_amb_emr"          & cohortMethodResult$outcomeId %in% ncCohortIds) |

  (cohortMethodResult$databaseId == "iqvia_pharmetrics_plus" & cohortMethodResult$analysisId == 1 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_pharmetrics_plus" & cohortMethodResult$analysisId == 2 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_pharmetrics_plus" & cohortMethodResult$analysisId == 3 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_pharmetrics_plus" & cohortMethodResult$analysisId == 4 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "iqvia_pharmetrics_plus" & cohortMethodResult$outcomeId %in% ncCohortIds) |

  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 1 & cohortMethodResult$targetId == 93678449 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 1 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 2 & cohortMethodResult$targetId == 93678449 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 2 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 3 & cohortMethodResult$targetId == 93678449 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 3 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 4 & cohortMethodResult$targetId == 93678449 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$analysisId == 4 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "optum_ehr"              & cohortMethodResult$outcomeId %in% ncCohortIds) |

  (cohortMethodResult$databaseId == "truven_ccae"            & cohortMethodResult$analysisId == 1 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "truven_ccae"            & cohortMethodResult$analysisId == 2 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "truven_ccae"            & cohortMethodResult$analysisId == 3 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "truven_ccae"            & cohortMethodResult$analysisId == 4 & cohortMethodResult$targetId == 93699028 & cohortMethodResult$outcomeId == 8466) |
  (cohortMethodResult$databaseId == "truven_ccae"            & cohortMethodResult$outcomeId %in% ncCohortIds) |
    
  (cohortMethodResult$databaseId == "optum_extended_dod"     & cohortMethodResult$outcomeId %in% ncCohortIds) |

  (cohortMethodResult$databaseId == "Meta-analysis") 
)

cohortMethodResult$rr[!keeps] <- NA
cohortMethodResult$ci95Ub[!keeps] <- NA
cohortMethodResult$ci95Lb[!keeps] <- NA
cohortMethodResult$logRr[!keeps] <- NA
cohortMethodResult$seLogRr[!keeps] <- NA
cohortMethodResult$p[!keeps] <- NA
cohortMethodResult$calibratedRr[!keeps] <- NA
cohortMethodResult$calibratedCi95Ub[!keeps] <- NA
cohortMethodResult$calibratedCi95Lb[!keeps] <- NA
cohortMethodResult$calibratedLogRr[!keeps] <- NA
cohortMethodResult$calibratedSeLogRr[!keeps] <- NA
cohortMethodResult$calibratedP[!keeps] <- NA

# relabel long T, C, and O names
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "Remicade and methotrexate (RA) w Rheumatoid Arthritis"] <- "RA - Remicade®-methotrexate"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "Remicade (CD-UC) w Crohns disease or ulcerative colitis"] <- "IBD - Remicade®"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "Remicade (PP-PsA) w Psoriatic arthritis or plaque psoriasis"] <- "PsO/PsA - Remicade®"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "Remicade (AS) w Ankylosing Spondylitis"] <- "AS - Remicade®"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "certolizumab pegol, tocilizumab (RA) w Rheumatoid Arthritis"] <- "RA - comparator"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "golimumab, certolizumab pegol, ustekinumab, vedolizumab (CD-UC) w Crohns disease or ulcerative colitis"] <- "IBD - comparator"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "golim, certz, guselk, risankiz, tildrakiz, brodal, ixekiz, secukin, ustekin (PP-PsA) w Psoriatic arthritis or plaque psoriasis"] <- "PsO/PsA - comparator"
exposureOfInterest$exposureName[exposureOfInterest$exposureName == "certolizumab pegol, golimumab, ixekizumab, secukinumab (AS) w Ankylosing Spondylitis"] <- "AS - comparator"
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Non-infectious uveitis (primary)"] <- "Non-infectious uveitis"


