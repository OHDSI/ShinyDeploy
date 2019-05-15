if (length(ls(pattern = "shinySettings")) == 0) {
  exportFolder <- "data"
} else {
  exportFolder <- shinySettings$exportFolder
}
# exportFolder <- "r:/MethodsLibraryPleEvaluation_ccae/export"
files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
estimates <- lapply(files, read.csv)
estimates <- do.call("rbind", estimates)

estimates$trueEffectSize[estimates$firstExposureOnly] <- estimates$trueEffectSizeFirstExposure[estimates$firstExposureOnly]
estimates$trueEffectSize[is.na(estimates$trueEffectSize)] <- estimates$targetEffectSize[is.na(estimates$trueEffectSize)]
z <- estimates$logRr/estimates$seLogRr
estimates$p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
idx <- is.na(estimates$logRr) | is.infinite(estimates$logRr) | is.na(estimates$seLogRr) | is.infinite(estimates$seLogRr)
estimates$logRr[idx] <- 0
estimates$seLogRr[idx] <- 999
estimates$ci95Lb[idx] <- 0
estimates$ci95Ub[idx] <- 999
estimates$p[idx] <- 1
idx <- is.na(estimates$calLogRr) | is.infinite(estimates$calLogRr) | is.na(estimates$calSeLogRr) | is.infinite(estimates$calSeLogRr)
estimates$calLogRr[idx] <- 0
estimates$calSeLogRr[idx] <- 999
estimates$calCi95Lb[idx] <- 0
estimates$calCi95Ub[idx] <- 999
estimates$calP[is.na(estimates$calP)] <- 1
dbs <- unique(estimates$database)
methods <- unique(estimates[, c("method", "comparative")])
strata <- as.character(unique(estimates$stratum))
strata <- strata[order(strata)]
strata <- c("All", strata)
strataSubset <- strata[strata != "All"]
strataSubset <- data.frame(stratum = strataSubset,
                           x = 1:length(strataSubset),
                           stringsAsFactors = FALSE)
strataSubset$stratum <- as.character(strataSubset$stratum)
strataSubset$stratum[strataSubset$stratum == "Inflammatory Bowel Disease"] <- "IBD"
strataSubset$stratum[strataSubset$stratum == "Acute pancreatitis"] <- "Acute\npancreatitis"

files <- list.files(exportFolder, "analysisRef.*csv", full.names = TRUE)
analysisRef <- lapply(files, read.csv)
analysisRef <- do.call("rbind", analysisRef)

trueRrs <- unique(estimates$targetEffectSize)
trueRrs <- trueRrs[order(trueRrs)]
trueRrs <- c("Overall", trueRrs)

