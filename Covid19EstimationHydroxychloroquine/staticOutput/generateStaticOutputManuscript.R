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

# table 1 ----------------------------------------------------------------------------------------------------------
section1 <- "Baseline characteristics before and after PS stratification"
section1 <- officer::fpar(officer::ftext(section1, prop = headingFormat))

dbTable1s <- list()
dbTable1Titles <- list()

databaseSubset <- database[database$databaseId != "Meta-analysis" & database$databaseId != "JMDC", ]
for (i in 1:nrow(databaseSubset)) { # i = 3
  table1s <- list()
  for (j in 1:nrow(tcs)) { # j = 1
    targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[j]]
    comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[j]]
    balance <- getCovariateBalance(targetId = tcs$targetId[j],
                                   comparatorId = tcs$comparatorId[j],
                                   databaseId = databaseSubset$databaseId[i],
                                   analysisId = 1, # follow-up doesnt affect covar balance
                                   outcomeId = 18) # all cause mortality for missing gender
    table1 <- prepareTable1(balance,
                            targetLabel = targetLabel,
                            comparatorLabel = comparatorLabel)
    facs <- sapply(table1, is.factor)
    table1[facs] <- lapply(table1[facs], as.character)
    rm(facs)
    colnames(table1) <- letters[1:length(colnames(table1))]
    table1 <- table1[table1$a %in% table1CovarSubset, ]
    table1 <- table1[, c("a", "e", "f", "g")]
    table1s[[length(table1s) + 1]] <- table1
  }
  
  dbTable1 <- merge(table1s[[1]], table1s[[2]], by = "a", all = TRUE, suffixes = c("T", "C"))
  dbTable1$order <- match(dbTable1$a, table1CovarSubset)
  dbTable1 <- dbTable1[order(dbTable1$order), ]
  dbTable1$order <- NULL
  dbTable1 <- rbind(c(blank, "HCQ vs SSZ", blank, blank, "AZM vs AMX", blank, blank), dbTable1)
  
  write.csv(dbTable1, file.path(staticOutputFolder, sprintf("table1 %s.csv", databaseSubset$databaseId[i])), row.names = FALSE)
  
  dbTable1 <- flextable::qflextable(dbTable1)
  dbTable1 <- flextable::delete_part(dbTable1, part = "header")
  dbTable1 <- flextable::fontsize(dbTable1, part = "all", size = 6)
  dbTable1 <- flextable::align(dbTable1, j = 1, align = "left", part = "all")
  dbTable1 <- flextable::autofit(dbTable1, add_w = 0, add_h = 0)
  dbTable1s[[length(dbTable1s) + 1]] <- dbTable1

  dbTable1Title <- databaseSubset$databaseId[i]
  dbTable1Title <- officer::fpar(officer::ftext(dbTable1Title, prop = titleFormat))  
  dbTable1Titles[[length(dbTable1Titles) + 1]] <- dbTable1Title
}

table1Pairs <- list(dbTable1Titles, dbTable1s)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section1, style = "heading 1")
for(i in 1:length(dbTable1s)) { #i=1
  doc <- doc  %>% 
    officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
    officer::body_add_break()
}
print(doc, target = file.path(staticOutputFolder, "table1s.docx"))


# table 2 ---------------------------------------------------------------------------------------------------------
section2 <- "Exposure cohort counts, event counts, and incidence rates"
section2 <- officer::fpar(officer::ftext(section2, prop = headingFormat))

cvdOutcomes <- c("CV-related mortality", "Chest pain or angina", "Heart failure")
otherOutcomes <- outcomeOfInterest$outcomeName[!outcomeOfInterest$outcomeName %in% cvdOutcomes]

createIrTable <- function(outcomesForTable) {
  mainResults <- getMainResults(targetIds = tcs$targetId, 
                                comparatorIds = tcs$comparatorId, 
                                outcomeIds = outcomeOfInterest$outcomeId, 
                                databaseIds = databaseIds,
                                analysisIds = cohortMethodAnalysis$analysisId)
  mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "targetId", by.y = "exposureId")
  names(mainResults)[names(mainResults) == "shortName"] <- "targetName"
  mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "comparatorId", by.y = "exposureId")
  names(mainResults)[names(mainResults) == "shortName"] <- "comparatorName"
  mainResults$comparison <- paste(mainResults$targetName, mainResults$comparatorName, sep = " vs ")
  mainResults$targetOrder <- match(mainResults$targetId, tcs$targetId)
  mainResults$outcomeOrder <- match(mainResults$outcomeId, outcomeOfInterest$outcomeId)
  mainResults$databaseOrder <- match(mainResults$databaseId, database$databaseId)
  mainResults <- mainResults[order(mainResults$targetOrder, mainResults$outcomeOrder, mainResults$databaseOrder), ]
  mainResults[, c("targetOrder", "outcomeOrder", "databaseOrder")] <- NULL
  
  mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
  
  irTable <- prepareStaticPowerTable(mainResults, cohortMethodAnalysis, outcomeOfInterest, outcomesForTable)
  irTable <- irTable[!is.na(irTable$rr), ]
  
  irTable <- irTable[, c("analysisShortName", "comparison", "outcomeName", "databaseId", "targetSubjects", "comparatorSubjects", "targetOutcomes", "comparatorOutcomes", "targetIr", "comparatorIr")]
  irTableWide <- merge(x = irTable[irTable$analysisShortName == "30-day", ],
                       y = irTable[irTable$analysisShortName == "On-treatment", ],
                       by = c("comparison", "outcomeName", "databaseId"), all = TRUE, suffixes = c("1", "2"))
  irTableWide$comparisonOrder <- match(irTableWide$comparison, c("HCQ vs SSZ", "AZM vs AMX"))
  irTableWide$outcomeOrder <- match(irTableWide$outcomeName, outcomeOfInterest$outcomeName)
  irTableWide$databaseOrder <- match(irTableWide$databaseId, database$databaseId)
  irTableWide <- irTableWide[order(irTableWide$comparisonOrder, irTableWide$outcomeOrder, irTableWide$databaseOrder), ]
  irTableWide[, c("analysisShortName1", "analysisShortName2", "comparisonOrder", "outcomeOrder", "databaseOrder")] <- NULL
  
  header1 <- c(blank, blank, blank, "30-day follow-up", blank, blank, blank, blank, blank, "On-treatment follow-up", blank, blank, blank, blank, blank)
  header2 <- c("Comparison", "Outcome", "Database", "Patients", blank, "Events", blank, "IR", blank, "Patients", blank, "Events", blank, "IR", blank)
  header3 <- c(blank, blank, blank, rep(c("T", "C"), 6))
  
  irTableWide <- rbind(header1,
                       header2,
                       header3,
                       irTableWide)
  return(irTableWide)
}

irTableCvd <- createIrTable(cvdOutcomes)
write.csv(irTableCvd, file.path(staticOutputFolder, "IR CVD.csv"), row.names = FALSE)

irTableOther <- createIrTable(otherOutcomes)
write.csv(irTableOther, file.path(staticOutputFolder, "IR other.csv"), row.names = FALSE)

# irTableWide <- flextable::qflextable(irTableWide)
# irTableWide <- flextable::delete_part(irTableWide, part = "header")
# irTableWide <- flextable::fontsize(irTableWide, part = "all", size = 6)
# irTableWide <- flextable::align(irTableWide, j = 1, align = "left", part = "all")
# irTableWide <- flextable::autofit(irTableWide, add_w = 0, add_h = 0)
# irTableWide <- flextable::merge_v(irTableWide, j = c(1,2,3), part = "body")
# 
# doc <- officer::read_docx() %>%
#   officer::body_add_fpar(section2, style = "heading 1") %>%  # pancytopenia table1s
#   flextable::body_add_flextable(irTableWide) %>%
#   print(target = file.path(staticOutputFolder, "IR CVD.docx"))

# forest plots -----------------------------------------------------------------------------------------------------
hcqLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[1]]
sszLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[1]]

hcqAztLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$targetId[2]]
hcqAmxLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcs$comparatorId[2]]

cvdOutcomes <- c("CV-related mortality", "Chest pain or angina", "Heart failure")
otherOutcomes <- outcomeOfInterest$outcomeName[!outcomeOfInterest$outcomeName %in% cvdOutcomes]

# figures 1, 2
for (j in 1:nrow(cohortMethodAnalysis)) { # j=1
  analysisName <- cohortMethodAnalysis$analysisShortName[j]
  mainResults <- getMainResults(targetIds = tcs$targetId, 
                                comparatorIds = tcs$comparatorId, 
                                outcomeIds = outcomeOfInterest$outcomeId, 
                                databaseIds = databaseIds,
                                analysisIds = cohortMethodAnalysis$analysisId[j])
  
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
  
  hrPlot <- plotStaticForestPlot(mainResults,
                                 database,
                                 outcomeOfInterest,
                                 outcomesForPlot = cvdOutcomes,
                                 estimateType = c("db", "ma"),
                                 targetLabel1 = hcqLabel,
                                 comparatorLabel1 = sszLabel,
                                 targetLabel2 = hcqAztLabel,
                                 comparatorLabel2 = hcqAmxLabel)
  
  fileName1 <- file.path(staticOutputFolder, sprintf("Figure %s HRs CVD %s.eps", j+1, analysisName))
  ggplot2::ggsave(fileName1, hrPlot, width = 14, height = 8, dpi = 1200)
  
  fileName2 <- file.path(staticOutputFolder, sprintf("Figure %s HRs CVD %s.pdf", j+1, analysisName))
  ggplot2::ggsave(fileName2, hrPlot, width = 14, height = 8, dpi = 1200)
}

#figure 3
mainResults <- getMainResults(targetIds = tcs$targetId, 
                              comparatorIds = tcs$comparatorId, 
                              outcomeIds = outcomeOfInterest$outcomeId, 
                              databaseIds = databaseIds,
                              analysisIds = cohortMethodAnalysis$analysisId)

mainResults <- merge(mainResults, outcomeOfInterest)
mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "targetId", by.y = "exposureId")
names(mainResults)[names(mainResults) == "shortName"] <- "targetName"
mainResults <- merge(mainResults, exposureOfInterest[, c("exposureId", "shortName")], by.x = "comparatorId", by.y = "exposureId")
names(mainResults)[names(mainResults) == "shortName"] <- "comparatorName"
mainResults$comparison <- paste(mainResults$targetName, mainResults$comparatorName, sep = " vs ")
mainResults <- merge(mainResults, cohortMethodAnalysis[, c("analysisId", "analysisShortName")])

mainResults$analysisOrder <- match(mainResults$analysisId, cohortMethodAnalysis$analysisId)
mainResults$targetOrder <- match(mainResults$targetId, tcs$targetId)
mainResults$outcomeOrder <- match(mainResults$outcomeId, outcomeOfInterest$outcomeId)
mainResults$databaseOrder <- match(mainResults$databaseId, database$databaseId)
mainResults <- mainResults[order(mainResults$analysisOrder, mainResults$targetOrder, mainResults$outcomeOrder, mainResults$databaseOrder), ]
mainResults[, c("analysisOrder", "targetOrder", "outcomeOrder", "databaseOrder")] <- NULL
mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
  
hrPlot <- plotStaticForestPlotMeta(mainResults,
                                   database,
                                   outcomeOfInterest,
                                   outcomesForPlot = otherOutcomes,
                                   targetLabel1 = hcqLabel,
                                   comparatorLabel1 = sszLabel,
                                   targetLabel2 = hcqAztLabel,
                                   comparatorLabel2 = hcqAmxLabel)

fileName1 <- file.path(staticOutputFolder, "Figure 1 HRs others 30d on-treatment.eps")
ggplot2::ggsave(fileName1, hrPlot, width = 14, height = 8, dpi = 1200)

fileName2 <- file.path(staticOutputFolder, "Figure 1 HRs others 30d on-treatment.pdf")
ggplot2::ggsave(fileName2, hrPlot, width = 14, height = 8, dpi = 1200)



  
