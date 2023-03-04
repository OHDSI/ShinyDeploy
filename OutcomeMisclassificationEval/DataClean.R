
analysisOrder <- c(
  "Unadjusted, 183d",
  "QBA, 183d",
  "diff QBA, 183d",
  "PS matched, 183d",
  "PS matched, QBA, 183d",
  "PS matched, diff QBA, 183d",
  "Unadjusted, 365d",
  "QBA, 365d",
  "diff QBA, 365d",
  "PS matched, 365d",
  "PS matched, QBA, 365d",
  "PS matched, diff QBA, 365d",
  "Unadjusted, 730d",
  "QBA, 730d",
  "diff QBA, 730d",
  "PS matched, 730d",
  "PS matched, QBA, 730d",
  "PS matched, diff QBA, 730d"
)

cohortMethodAnalysis$analysisOrder <- match(cohortMethodAnalysis$description, analysisOrder)
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$analysisOrder), ]
#cohortMethodAnalysis$analysisOrder <- NULL

cohortMethodResult$analysisOrder <- match(cohortMethodResult$analysisDescription, analysisOrder)
cohortMethodResult <- cohortMethodResult[order(cohortMethodResult$analysisOrder), ]

#cohortMethodResult$analysisOrder <- NULL