rm(likelihoodProfile, dummy) # save space


relabel <- function(df) {
  df[2][df[2] == "Unadjusted, 183d"] <- "183d, Unadjusted"
  df[2][df[2] == "QBA, 183d"] <- "183d, QBA"
  df[2][df[2] == "diff QBA, 183d"] <- "183d, diff QBA"
  df[2][df[2] == "PS matched, 183d"] <- "183d, PS matched"
  df[2][df[2] == "PS matched, QBA, 183d"] <- "183d, PS matched, QBA"
  df[2][df[2] == "PS matched, diff QBA, 183d"] <- "183d, PS matched, diff QBA"
  df[2][df[2] == "Unadjusted, 365d"] <- "365d, Unadjusted"
  df[2][df[2] == "QBA, 365d"] <- "365d, QBA"
  df[2][df[2] == "diff QBA, 365d"] <- "365d, diff QBA"
  df[2][df[2] == "PS matched, 365d"] <- "365d, PS matched"
  df[2][df[2] == "PS matched, QBA, 365d"] <- "365d, PS matched, QBA"
  df[2][df[2] == "PS matched, diff QBA, 365d"] <- "365d, PS matched, diff QBA"
  df[2][df[2] == "Unadjusted, 730d"] <- "730d, Unadjusted"
  df[2][df[2] == "QBA, 730d"] <- "730d, QBA"
  df[2][df[2] == "diff QBA, 730d"] <- "730d, diff QBA"
  df[2][df[2] == "PS matched, 730d"] <- "730d, PS matched"
  df[2][df[2] == "PS matched, QBA, 730d"] <- "730d, PS matched, QBA"
  df[2][df[2] == "PS matched, diff QBA, 730d"] <- "730d, PS matched, diff QBA"
  return(df)
}
cohortMethodAnalysis <- relabel(cohortMethodAnalysis)
cohortMethodResult <- relabel(cohortMethodResult)

analysisOrder <- c("183d, Unadjusted",
                   "183d, QBA",
                   "183d, diff QBA",
                   "183d, PS matched",
                   "183d, PS matched, QBA",
                   "183d, PS matched, diff QBA",
                   "365d, Unadjusted",
                   "365d, QBA",
                   "365d, diff QBA",
                   "365d, PS matched",
                   "365d, PS matched, QBA",
                   "365d, PS matched, diff QBA",
                   "730d, Unadjusted",
                   "730d, QBA",
                   "730d, diff QBA",
                   "730d, PS matched",
                   "730d, PS matched, QBA",
                   "730d, PS matched, diff QBA")

cohortMethodAnalysis$order <- match(cohortMethodAnalysis$description, analysisOrder)
cohortMethodAnalysis <- cohortMethodAnalysis[order(cohortMethodAnalysis$order), ]
cohortMethodAnalysis$order <- NULL

cohortMethodResult$order <- match(cohortMethodResult$analysisDescription, analysisOrder)
cohortMethodResult <- cohortMethodResult[order(cohortMethodResult$order), ]
cohortMethodResult$order <- NULL

exposureOfInterest <- exposureOfInterest %>% dplyr::filter(exposureId %in% c(1:3))
outcomeOfInterest <- outcomeOfInterest %>% dplyr::filter(outcomeId == 4008)
