source("global.R")
source("staticOutput/staticPlotsAndTables.R")
staticOutputFolder <- "./staticOutput"

databaseIds <- database$databaseId
exposureOfInterest$shortName[exposureOfInterest$exposureId == 137] <- "AZM"
exposureOfInterest$shortName[exposureOfInterest$exposureId == 2] <- "HCQ"
exposureOfInterest$shortName[exposureOfInterest$exposureId == 143] <- "AMX"
exposureOfInterest$shortName[exposureOfInterest$exposureId == 28] <- "SSZ"
cohortMethodAnalysis$analysisShortName[cohortMethodAnalysis$analysisId == 1] <- "On-treatment"
cohortMethodAnalysis$analysisShortName[cohortMethodAnalysis$analysisId == 2] <- "30-day"
comparisonSummary <- comparisonSummary[order(comparisonSummary$targetId, comparisonSummary$databaseId), ]
outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeName == "Cardiovascular-related mortality"] <- "CV-related mortality"
tcs <- unique(tcos[, c("targetId", "comparatorId")])
tcs <- tcs[order(tcs$targetId), ]

headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
blank <- ""


# counts table -----------------------------------------------------------------------------------------------------

drops <- 
  (attrition$databaseId == "OptumEHR" & attrition$analysisId == 1) | # panther on-treatment
  (attrition$databaseId %in% c("CCAE", "DAGermany", "JMDC", "MDCD", "MDCR", "OptumEHR", "OpenClaims", "AmbEMR") & attrition$outcomeId %in% c(18, 19)) | # death
  (attrition$databaseId %in% c("AmbEMR", "CPRD", "DAGermany", "IMRD", "SIDIAP") & attrition$outcomeId %in% c(22, 13, 20, 21, 17, 8, 11)) # databases with no IP
patientCounts <- attrition[!drops, ]

getPatientCounts <- function(attritionLevel) {
  
  patientCounts <- patientCounts[patientCounts$description == attritionLevel & patientCounts$databaseId %in% databaseIds & patientCounts$analysisId ==2,
                                 c("databaseId", "exposureId", "subjects")]
  patientCounts <- aggregate(patientCounts, by = patientCounts[c("databaseId", "exposureId")], FUN = max)[3:5] # dunno why extra colummns being added
  exposureTotals <- aggregate(patientCounts["subjects"], by = patientCounts["exposureId"], FUN = sum)
  names(exposureTotals)[2] <- "exposureSubjects"
  databaseTotals <- aggregate(patientCounts["subjects"], by = patientCounts["databaseId"], FUN = sum)
  names(databaseTotals)[2] <- "databaseSubjects"
  patientCounts <- merge(patientCounts, exposureTotals)
  patientCounts$exposurePercent <- round(patientCounts$subjects / patientCounts$exposureSubjects * 100, 2)
  patientCounts <- merge(exposureOfInterest, patientCounts)
  patientCounts <- patientCounts[-c(1,3)]
  patientCounts <- merge(patientCounts, databaseTotals)
  patientCounts$databasePercent <- round(patientCounts$subjects / patientCounts$databaseSubjects * 100, 2)
  patientCounts$exposureOrder <- match(patientCounts$exposureName, exposureOfInterest$exposureName)
  patientCounts$databaseOrder <- match(patientCounts$databaseId, database$databaseId)
  patientCounts <- patientCounts[order(patientCounts$exposureOrder, patientCounts$databaseOrder), ]
  patientCounts[, c("exposureOrder", "databaseOrder")] <- NULL
  patientCounts <- patientCounts[, c("exposureName", "databaseId", "subjects", "exposureSubjects", "exposurePercent",  "databaseSubjects", "databasePercent")]
  return(patientCounts)
}

counts <- getPatientCounts(attritionLevel = "First cohort only & restrict to common period")
#counts <- getPatientCounts(attritionLevel = "Have at least 1 days at risk")
#counts <- getPatientCounts(attritionLevel = "Original cohorts")
write.csv(counts, file.path(staticOutputFolder, "COUNTS.csv"), row.names = FALSE) 


# baseline characteristics ------------------------------------------------------------------------------------------
section1 <- "Baseline characteristics before and after PS stratification"
section1 <- officer::fpar(officer::ftext(section1, prop = headingFormat))

table1s <- list()
table1Titles <- list()
for (i in c(6,20)) { # 1:nrow(comparisonSummary)) { # i=1
  
  targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$targetId[i]]
  comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$comparatorId[i]]
  balance <- getCovariateBalance(targetId = comparisonSummary$targetId[i],
                                 comparatorId = comparisonSummary$comparatorId[i],
                                 databaseId = comparisonSummary$databaseId[i],
                                 analysisId = 1, # follow-up doesnt affect covar balance
                                 outcomeId = 18) # all cause mortality
  table1 <- prepareTable1(balance,
                          targetLabel = targetLabel,
                          comparatorLabel = comparatorLabel)
  facs <- sapply(table1, is.factor)
  table1[facs] <- lapply(table1[facs], as.character)
  rm(facs)
  table1 <- rbind(c(blank, "Before stratification", blank, blank, "After stratification", blank, blank), table1)
  colnames(table1) <- letters[1:length(colnames(table1))]
  
  table1 <- flextable::qflextable(table1)
  table1 <- flextable::delete_part(table1, part = "header")
  table1 <- flextable::fontsize(table1, part = "all", size = 6)
  table1 <- flextable::align(table1, j = 1, align = "left", part = "all")
  table1 <- flextable::autofit(table1, add_w = 0, add_h = 0)
  table1s[[length(table1s) + 1]] <- table1
  title <- sprintf("%s vs. %s: %s", targetLabel, comparatorLabel, comparisonSummary$databaseId[i])
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))  
  table1Titles[[length(table1Titles) + 1]] <- title
}
table1Pairs <- list(table1Titles, table1s)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section1, style = "heading 1")
for(i in 1:length(table1s)) { #i=1
  doc <- doc  %>% 
    officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
    officer::body_add_break()
}
print(doc, target = file.path(staticOutputFolder, "table1s_ipci.docx"))

irTables <- list()
irTablesTitles <- list()
tcs <- unique(comparisonSummary[, c("targetId", "comparatorId")])
for (i in 1:nrow(tcs)) { # i=2
  targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[i]]
  comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[i]]
  for (j in 1:nrow(cohortMethodAnalysis)) { #j=1
    
    analysisName <- cohortMethodAnalysis$shortName[j]
    mainResults <- getMainResults(targetIds = tcs$targetId[i], 
                                  comparatorIds = tcs$comparatorId[i], 
                                  outcomeIds = outcomeOfInterest$outcomeId, 
                                  databaseIds = databaseIds,
                                  analysisIds = cohortMethodAnalysis$analysisId[j])
    irTable <- prepareStaticPowerTable(mainResults, cohortMethodAnalysis, outcomeOfInterest)
    irTable$analysisOrder <- match(irTable$shortName, cohortMethodAnalysis$analysisName)
    irTable$outcomeOrder <- match(irTable$outcomeName, outcomeOfInterest$outcomeName)
    irTable$databaseOrder <- match(irTable$databaseId, database$databaseId)
    irTable <- irTable[order(irTable$analysisOrder, irTable$outcomeOrder, irTable$databaseOrder), ]
    irTable[, c("analysisOrder", "outcomeOrder", "databaseOrder")] <- NULL
    irTable <- irTable[!is.na(irTable$rr), ]
    irTable[, c("shortName", "rr", "i2", "sources", "mdrr")] <- NULL
    irTable <- rbind(c("Outcome", "Database", "Patients", blank, "Follow-up", blank, "Events", blank, "IR", blank),
                     c(blank, blank, rep(c(targetLabel, comparatorLabel), 4)),
                     irTable)
    fileName <- sprintf("IRs %s vs %s %s.csv", targetLabel, comparatorLabel, analysisName)
    write.csv(irTable, file.path(staticOutputFolder, fileName), row.names = FALSE, col.names = FALSE)
  }
}

# HR tables --------------------------------------------------------------------------------------------------
section3 <- "Uncalibrated and calibrated hazard ratios"
section3 <- officer::fpar(officer::ftext(section3, prop = headingFormat))

hrTables <- list()
hrTablesTitles <- list()
tcs <- unique(comparisonSummary[, c("targetId", "comparatorId")])
for (i in 1:nrow(tcs)) { # i=1
  targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[i]]
  comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[i]]
  for (j in 1:nrow(cohortMethodAnalysis)) { #j=1
    analysisName <- cohortMethodAnalysis$shortName[j]
    mainResults <- getMainResults(targetIds = tcs$targetId[i], 
                                  comparatorIds = tcs$comparatorId[i], 
                                  outcomeIds = outcomeOfInterest$outcomeId, 
                                  databaseIds = databaseIds,
                                  analysisIds = cohortMethodAnalysis$analysisId[j])
    hrTable <- prepareStaticMainResultsTable(mainResults, cohortMethodAnalysis, outcomeOfInterest)
    hrTable$analysisOrder <- match(hrTable$Analysis, cohortMethodAnalysis$shortName)
    hrTable$outcomeOrder <- match(hrTable$Outcome, outcomeOfInterest$outcomeName)
    hrTable$databaseOrder <- match(hrTable$Database, database$databaseId)
    hrTable <- hrTable[order(hrTable$analysisOrder, hrTable$outcomeOrder, hrTable$databaseOrder), ]
    hrTable[, c("analysisOrder", "outcomeOrder", "databaseOrder")] <- NULL
    hrTable <- hrTable[hrTable$P != "NA", ]
    hrTable[, c("Analysis", "i2", "sources")] <- NULL
    fileName <- sprintf("HRs %s vs %s %s.csv", targetLabel, comparatorLabel, analysisName)
    write.csv(hrTable, file.path(staticOutputFolder, fileName), row.names = FALSE)
  }
}


# Diagnostics plots ------------------------------------------------------------------------------------------
section4 <- "Evidence evaluation diagnostics"
section4 <- officer::fpar(officer::ftext(section4, prop = headingFormat))

headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
diagTitles <- list()
diagFileNames <- list()
outcomeId <- 18 # all cause mortality
defaultAnalysisId <- 2

dbs <- database[database$databaseId != "Meta-analysis", ]
for (databaseId in dbs$databaseId) {  # databaseId = "JMDC"
  
  fileName <- file.path(staticOutputFolder, sprintf("DIAGNOSTICS %s.png", databaseId))
  dbComparisonSummary <- comparisonSummary[comparisonSummary$databaseId == databaseId, ]
  
  diagPlots <- list()
  for (i in 1:nrow(dbComparisonSummary)) { # i = 2
  
    targetId <- dbComparisonSummary$targetId[i]
    comparatorId <- dbComparisonSummary$comparatorId[i]
    targetName <- exposureOfInterest$shortName[exposureOfInterest$exposureId == targetId]
    comparatorName <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparatorId]
    
    title <- databaseId
    if (!file.exists(fileName)) {
      
      ps <- getPs(connection, targetId, comparatorId, defaultAnalysisId, databaseId)
      if (is.null(ps)) {
        psPlot <- ggplot2::ggplot() + ggplot2::theme_void()
      } else {
        psPlot <- plotPs(ps, targetName, comparatorName)  
      }
      diagPlots[[length(diagPlots) + 1]] <- psPlot
      bal <- getCovariateBalance(connection = connection,
                                 targetId = targetId,
                                 comparatorId = comparatorId,
                                 databaseId = databaseId,
                                 analysisId = defaultAnalysisId,
                                 outcomeId = outcomeId)
      balPlot <- plotCovariateBalanceScatterPlot(balance = bal)
      diagPlots[[length(diagPlots) + 1]] <- balPlot
      
      for (j in 1:nrow(cohortMethodAnalysis)) { # j = 1
        analysisId <- cohortMethodAnalysis$analysisId[j]
        analysisName <- cohortMethodAnalysis$analysisShortName[j]
        controlResults <- getControlResults(connection, targetId, comparatorId, analysisId, databaseId)
        nullPlot <-  plotLargeScatter(d = controlResults, xLabel = "Hazard ratio")
        diagPlots[[length(diagPlots) + 1]] <- nullPlot
      }
    }
  }
  if (databaseId == "JMDC") {
    blankPlot <- ggplot2::ggplot() + ggplot2::theme_void()
    diagPlots[[5]] <- blankPlot
    diagPlots[[6]] <- blankPlot
    diagPlots[[7]] <- blankPlot
    diagPlots[[8]] <- blankPlot
  }
  if (!file.exists(fileName)) {
    row0 <- grid::textGrob("")
    row1 <- grid::textGrob("HCQ vs SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
    row2 <- grid::textGrob("AZM vs AMX", rot = 90, gp = grid::gpar(fontsize = 18))
    col1 <- grid::textGrob("PS distribution", gp = grid::gpar(fontsize = 18))
    col2 <- grid::textGrob("Covariate balance", gp = grid::gpar(fontsize = 18))
    col3 <- grid::textGrob("30-day", gp = grid::gpar(fontsize = 18))
    col4 <- grid::textGrob("On-treatment", gp = grid::gpar(fontsize = 18))
    plotGrid <- gridExtra::grid.arrange(row0, col1, col2, col3, col4,
                                        row1, diagPlots[[1]], diagPlots[[2]], diagPlots[[3]], diagPlots[[4]],
                                        row2, diagPlots[[5]], diagPlots[[6]], diagPlots[[7]], diagPlots[[8]],
                                        nrow = 3,
                                        heights = c(1, 4, 4),
                                        widths = c(0.25, 3, 3, 3, 3))
    ggplot2::ggsave(fileName, plotGrid, width = 14, height = 7, dpi = 400)
  }
  titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  diagTitles[[length(diagTitles) + 1]] <- title
  diagFileNames[[length(diagFileNames) + 1]] <- fileName
}

# doc <- officer::read_docx() %>% 
#   officer::body_add_fpar(section4, style = "heading 1")
# for(i in 1:length(length(diagFileNames))) { #i=1
#   doc <- doc  %>% 
#     officer::body_add_fpar(diagTitles[[i]], style = "heading 2") %>%
#     officer::body_add_img(diagFileNames[[i]], width = 10, height = 5) %>%
#     officer::body_add_break()
# }
# print(doc, target = file.path(staticOutputFolder, "DIAGNOSTICS.docx"))
  
# all forest plots -----------------------------------------------------------------------
section5 <- "All forest plots"
section5 <- officer::fpar(officer::ftext(section5, prop = headingFormat))

hcqLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[1]]
sszLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[1]]

hcqAztLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[2]]
hcqAmxLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[2]]


headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
hrForestTitles <- list()
hrForestFileNames <- list()

for (i in 1:nrow(cohortMethodAnalysis)) { # i=2
  
  analysisId <- cohortMethodAnalysis$analysisId[i]
  analysisName <- cohortMethodAnalysis$analysisShortName[i]
  
  print(analysisName)
  
  for (j in 1:nrow(outcomeOfInterest)) {  # j=7
   
    outcomeId <- outcomeOfInterest$outcomeId[j]
    outcomeName <- outcomeOfInterest$outcomeName[j]
     
    print(outcomeName)
    
    fileName <- file.path(staticOutputFolder, sprintf("HRs %s %s.png", analysisName, outcomeName))
   
    
    mainResults <- getMainResults(targetIds = tcs$targetId, 
                                  comparatorIds = tcs$comparatorId, 
                                  outcomeIds = outcomeId, 
                                  databaseIds = databaseIds,
                                  analysisIds = analysisId)
    
    mainResults <- merge(mainResults, outcomeOfInterest)
    mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "targetId", by.y = "exposureId")
    names(mainResults)[names(mainResults) == "shortName"] <- "targetName"
    mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "comparatorId", by.y = "exposureId")
    names(mainResults)[names(mainResults) == "shortName"] <- "comparatorName"
    mainResults$comparison <- paste(mainResults$targetName, mainResults$comparatorName, sep = " vs ")
    
    mainResults$analysisOrder <- match(mainResults$analysisId, cohortMethodAnalysis$analysisId)
    mainResults$targetOrder <- match(mainResults$targetId, tcs$targetId)
    mainResults$outcomeOrder <- match(mainResults$outcomeId, outcomeOfInterest$outcomeId)
    mainResults$databaseOrder <- match(mainResults$databaseId, database$databaseId)
    mainResults <- mainResults[order(mainResults$analysisOrder, mainResults$targetOrder, mainResults$outcomeOrder, mainResults$databaseOrder), ]
    mainResults[, c("analysisOrder", "targetOrder", "outcomeOrder", "databaseOrder")] <- NULL
    mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
    
    hrPlot <- plotStaticForestPlotAppnx(mainResults,
                                        database,
                                        outcomeOfInterest,
                                        outcomesForPlot = outcomeName,
                                        estimateType = c("db", "ma"),
                                        targetLabel1 = hcqLabel,
                                        comparatorLabel1 = sszLabel,
                                        targetLabel2 = hcqAztLabel,
                                        comparatorLabel2 = hcqAmxLabel)
    ggplot2::ggsave(fileName, hrPlot, width = 14, height = 8, dpi = 400)
    
    
    hrForestFileNames[[length(hrForestFileNames) + 1]] <- fileName
    title <- sprintf("%s %s", outcomeName, analysisName)
    title <- officer::fpar(officer::ftext(title, prop = titleFormat))  
    hrForestTitles[[length(hrForestTitles) + 1]] <- title
  }
}

hrForest1Pairs <- list(hrForestTitles, hrForestFileNames)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section5, style = "heading 1")
for(i in 1:length(hrForestTitles)) { #i=1
  doc <- doc  %>% 
    officer::body_add_fpar(hrForest1Pairs[[1]][[i]], style = "heading 2") %>%
    officer::body_add_img(hrForest1Pairs[[2]][[i]], width = 10, height = 5) %>%
    officer::body_add_break()
}
print(doc, target = file.path(staticOutputFolder, "HRs forest all.docx"))





  
  




