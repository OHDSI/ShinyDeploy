# ccae table 1 fix 

# #hcq vs ssz
# balance <- readRDS("G:/StudyResults/Covid19EstimationHydroxychloroquine_2/CCAE/balance/bal_t2_c28_o18_a1.rds")
# table1Test <- CohortMethod::createCmTable1(balance)
# 
# 
# # hcq+azm vs hcq+amx
# balance <- readRDS("G:/StudyResults/Covid19EstimationHydroxychloroquine_2/CCAE/balance/bal_t137_c143_o18_a1.rds")
# table1Test <- CohortMethod::createCmTable1(balance)


table1CovarSubset <- c(
  "",
  "",
  "Characteristic",
  "    15-19",
  "    20-24",
  "    25-29",                                        
  "    30-34",
  "    35-39",
  "    40-44",                                        
  "    45-49",
  "    50-54",
  "    55-59",                                        
  "    60-64",
  "    65-69",
  "Gender: female",                                       
  "Medical history: General",
  "    Acute respiratory disease",
  "    Chronic obstructive lung disease",
  "    Depressive disorder",
  "    Diabetes mellitus",
  "    Hyperlipidemia",                               
  "    Pneumonia",
  "    Renal impairment",
  "    Urinary tract infectious disease",
  "Medical history: Cardiovascular disease",
  "    Atrial fibrillation",
  "    Cerebrovascular disease",                      
  "    Coronary arteriosclerosis",
  "    Heart disease",
  "    Heart failure",                             
  "    Ischemic heart disease",
  "Medication use",
  "    Agents acting on the renin-angiotensin system",
  "    Antidepressants",
  "    Drugs for obstructive airway diseases",
  "    Immunosuppressants",                           
  "    Opioids",
  "    Psycholeptics"
)

prepareStaticPowerTable <- function(mainResults, analyses, outcomes, cvdOutcomes) {
  table <- merge(mainResults, analyses[, c("analysisId", "analysisShortName")])
  table <- merge(table, outcomes)
  table <- table[table$outcomeName %in% cvdOutcomes, ]
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- qnorm(1 - alpha/2)
  zBeta <- -qnorm(1 - power)
  pA <- table$targetSubjects/(table$targetSubjects + table$comparatorSubjects)
  pB <- 1 - pA
  totalEvents <- abs(table$targetOutcomes) + (table$comparatorOutcomes)
  table$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  table$targetYears <- table$targetDays/365.25
  table$comparatorYears <- table$comparatorDays/365.25
  table$targetIr <- 1000 * table$targetOutcomes/table$targetYears
  table$comparatorIr <- 1000 * table$comparatorOutcomes/table$comparatorYears
  
  table <- table[, c("analysisShortName",
                     "comparison",
                     "outcomeName",
                     "databaseId",
                     "targetSubjects",
                     "comparatorSubjects",
                     "targetYears",
                     "comparatorYears",
                     "targetOutcomes",
                     "comparatorOutcomes",
                     "targetIr",
                     "comparatorIr",
                     "mdrr",
                     "rr",
                     "i2",
                     "sources")]
  
  table$targetSubjects <- formatC(table$targetSubjects, big.mark = ",", format = "d")
  table$comparatorSubjects <- formatC(table$comparatorSubjects, big.mark = ",", format = "d")
  table$targetYears <- formatC(table$targetYears, big.mark = ",", format = "d")
  table$comparatorYears <- formatC(table$comparatorYears, big.mark = ",", format = "d")
  table$targetOutcomes <- formatC(table$targetOutcomes, big.mark = ",", format = "d")
  table$comparatorOutcomes <- formatC(table$comparatorOutcomes, big.mark = ",", format = "d")
  table$targetIr <- sprintf("%.2f", table$targetIr)
  table$comparatorIr <- sprintf("%.2f", table$comparatorIr)
  table$mdrr <- sprintf("%.2f", table$mdrr)
  table$targetSubjects <- gsub("^-", "<", table$targetSubjects)
  table$comparatorSubjects <- gsub("^-", "<", table$comparatorSubjects)
  table$targetOutcomes <- gsub("^-", "<", table$targetOutcomes)
  table$comparatorOutcomes <- gsub("^-", "<", table$comparatorOutcomes)
  table$targetIr <- gsub("^-", "<", table$targetIr)
  table$comparatorIr <- gsub("^-", "<", table$comparatorIr)
  idx <- (table$targetOutcomes < 0 | table$comparatorOutcomes < 0)
  table$mdrr[idx] <- paste0(">", table$mdrr[idx])
  return(table)
}

prepareStaticMainResultsTable <- function(mainResults, analyses, outcomes) {
  table <- mainResults
  table$hr <- sprintf("%.2f (%.2f - %.2f)", mainResults$rr, mainResults$ci95Lb, mainResults$ci95Ub)
  table$p <- sprintf("%.2f", table$p)
  table$calHr <- sprintf("%.2f (%.2f - %.2f)",
                         mainResults$calibratedRr,
                         mainResults$calibratedCi95Lb,
                         mainResults$calibratedCi95Ub)
  table$calibratedP <- sprintf("%.2f", table$calibratedP)
  table <- merge(table, analyses[, c("analysisId", "shortName")])
  table <- merge(table, outcomes)
  table <- table[, c("shortName", "outcomeName", "databaseId", "hr", "p", "calHr", "calibratedP", "i2", "sources")]
  colnames(table) <- c("Analysis", "Outcome", "Database", "HR (95% CI)", "P", "Cal. HR (95% CI)", "Cal. p", "i2", "sources")
  return(table)
}

plotStaticForestPlot <- function(mainResults,
                                 database,
                                 outcomeOfInterest,
                                 outcomesForPlot,
                                 estimateType,
                                 targetLabel1,
                                 comparatorLabel1,
                                 targetLabel2,
                                 comparatorLabel2) {
  
  meta <- mainResults$databaseId == "Meta-analysis"
  
  comparisons <- mainResults$comparison[!meta]
  logRr <- log(mainResults$calibratedRr[!meta])
  logLb95Ci <- log(mainResults$calibratedCi95Lb[!meta])
  logUb95Ci <- log(mainResults$calibratedCi95Ub[!meta])
  labels <- mainResults$databaseId[!meta]
  i2 <- mainResults$i2[!meta]
  outcomes <- mainResults$outcomeName[!meta]
  
  mComparisons <- mainResults$comparison[meta]
  mLogRr <- log(mainResults$calibratedRr[meta])
  mLogLb95Ci <- log(mainResults$calibratedCi95Lb[meta])
  mLogUb95Ci <- log(mainResults$calibratedCi95Ub[meta])
  mLabels <- mainResults$databaseId[meta]
  mI2 <- mainResults$i2[meta]
  mOutcomes <- mainResults$outcomeName[meta]
  i2s <- mainResults$i2[meta]
  
  # create plot data
  d1 <- data.frame(logRr = logRr,
                   logLb95Ci = logLb95Ci,
                   logUb95Ci = logUb95Ci,
                   name = labels,
                   i2 = i2,
                   outcome = outcomes,
                   type = "db",
                   comparison = comparisons,
                   stringsAsFactors = FALSE)
  d2 <- data.frame(logRr = mLogRr,
                   logLb95Ci = mLogLb95Ci,
                   logUb95Ci = mLogUb95Ci,
                   name = mLabels,
                   i2 = mI2,
                   outcome = mOutcomes,
                   type = "ma",
                   comparison = mComparisons,
                   stringsAsFactors = FALSE)
  d <- rbind(d1, d2)
  d <- merge(x = d[d$comparison == "HCQ vs SSZ", ],
             y = d[d$comparison == "AZM vs AMX", ],
             by = c("name", "outcome", "type"),
             all = TRUE,
             suffixes = c("1", "2"))
  nums <- c("logRr1", "logLb95Ci1", "logUb95Ci1", "logRr2", "logLb95Ci2", "logUb95Ci2")
  d[nums][is.na(d[nums])] <- -100
  d$comparison1[is.na(d$comparison1)] <- "HCQ vs SSZ"
  d$comparison2[is.na(d$comparison2)] <- "AZM vs AMX"
  
  dHeader <- data.frame(name = "Database",
                        outcome = "Outcome",
                        type = "header",
                        logRr1 = -100,
                        logLb95Ci1 = -100,
                        logUb95Ci1 = -100,
                        i21 = NA,
                        comparison1 = "header",
                        logRr2 = -100,
                        logLb95Ci2 = -100,
                        logUb95Ci2 = -100,
                        i22 = NA,
                        comparison2 = "header",
                        stringsAsFactors = FALSE)
  d <- rbind(dHeader, d)
  d$dbOrder <- match(d$name, database$databaseId)
  d$dbOrder[d$name == "Database"] <- 0
  d$outcomeOrder <- match(d$outcome, outcomeOfInterest$outcomeName)
  d$outcomeOrder[d$name == "Database"] <- 0
  d <- d[order(d$outcomeOrder, d$dbOrder), ]
  
  # subset outcomes
  d <- d[d$outcome == "Outcome" | d$outcome %in% outcomesForPlot, ]
  d <- d[d$type == "header" | d$type %in% estimateType, ]
  
  d$row <- rev(1:nrow(d))
  
  breaks <- c(0.175, 0.25, 0.5, 1, 2, 4, 6)
  plotLabels1 <- c(0.175, 0.25, paste("0.5\nFavors", targetLabel1), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel1), 4, 6)
  plotLabels2 <- c(0.175, 0.25, paste("0.5\nFavors", targetLabel2), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel2), 4, 6)
  
  # labels grob
  labels0 <- data.frame(y = rep(d$row, 2),
                        x = rep(c(1, 2), each = nrow(d)),
                        label = c(d$outcome, as.character(d$name)),
                        stringsAsFactors = FALSE)
  labels0$label[labels0$x == 1 & duplicated(labels0$label)] <- ""
  
  plot0 <- ggplot2::ggplot(labels0, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels0) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::coord_cartesian(xlim = c(1, 2.5), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +
    ggplot2::xlab(blank) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(color = "white"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "white"),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  # mono grob
  labels1 <- paste0(formatC(exp(d$logRr1),  digits = 2, format = "f"), " (",
                    formatC(exp(d$logLb95Ci1), digits = 2, format = "f"), "-",
                    formatC(exp(d$logUb95Ci1), digits = 2, format = "f"), ")")
  labels1[d$name == "Meta-analysis"] <- paste(labels1[d$name == "Meta-analysis"], paste0("[", ifelse(d$i21[d$name == "Meta-analysis"] < 0.01, "<0.01", d$i21[d$name == "Meta-analysis"]), "]"))
  labels1[grep("NA", labels1)] <- ""
  labels1 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels1,
                        stringsAsFactors = FALSE)
  labels1$label[labels1$x == -5.6 & duplicated(labels1$label)] <- ""
  labels1$label[labels1$label == "0.00 (0.00-0.00)"][1] <- "CalHR (95% CI) [I2]"
  labels1$label[labels1$label == "0.00 (0.00-0.00)"] <- ""
  
  d$lcl1 <- ifelse(d$logLb95Ci1 < log(0.175), log(0.175), d$logLb95Ci1)
  d$ucl1 <- ifelse(d$logUb95Ci1 > log(6), log(6), d$logUb95Ci1)
  d$lcl1[d$type == "header"] <- -100
  d$ucl1[d$type == "header"] <- -100
  
  if (length(d$row[d$logLb95Ci1 < d$lcl1 & d$logLb95Ci1 != -100]) > 0) {
    lclData1 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci1 < d$lcl1],
                           yend = d$row[d$logLb95Ci1 < d$lcl1])
  } else {
    lclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci1 > d$ucl1 & d$logUb95Ci1 != -100]) > 0) {
    uclData1 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci1 > d$ucl1],
                           yend = d$row[d$logUb95Ci1 > d$ucl1])
  } else {
    uclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot1 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr1, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl1, xmax = ucl1)) +
    ggplot2::geom_segment(data = lclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 1, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels1) +
    ggplot2::coord_cartesian(xlim = c(-3, log(5)), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +    #c(min(d$row)+1.25, max(d$row)-coordOffset)) # horizontal locations params
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  #combo grob
  d$lcl2 <- ifelse(d$logLb95Ci2 < log(0.175), log(0.175), d$logLb95Ci2)
  d$lcl2 <- ifelse(d$logLb95Ci2 == -100, -100, d$lcl2)
  d$ucl2 <- ifelse(d$logUb95Ci2 > log(6), log(6), d$logUb95Ci2)
  d$lcl2[d$type == "header"] <- -100
  d$ucl2[d$type == "header"] <- -100
  
  labels2 <- paste0(formatC(exp(d$logRr2),  digits = 2, format = "f"),
                    " (",
                    formatC(exp(d$logLb95Ci2), digits = 2, format = "f"),
                    "-",
                    formatC(exp(d$logUb95Ci2), digits = 2, format = "f"),
                    ")")
  labels2[d$name == "Meta-analysis"] <- paste(labels2[d$name == "Meta-analysis"], paste0("[", ifelse(d$i22[d$name == "Meta-analysis"] < 0.01, "<0.01", d$i22[d$name == "Meta-analysis"]), "]"))
  labels2[grep("NA", labels2)] <- ""
  labels2 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels2,
                        stringsAsFactors = FALSE)
  
  labels2$label[labels2$x == -5.6 & duplicated(labels2$label)] <- ""
  labels2$label[labels2$label == "0.00 (0.00-0.00)"][1] <- "CalHR (95% CI) [I2]"
  labels2$label[labels2$label == "0.00 (0.00-0.00)"] <- ""
  
  if (length(d$row[d$logLb95Ci2 < d$lcl2 & d$logLb95Ci2 != -100])) {
    lclData2 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci2 < d$lcl2],
                           yend = d$row[d$logLb95Ci2 < d$lcl2])
  } else {
    lclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci2 > d$ucl2 & d$logUb95Ci2 != -100]) > 0) {
    uclData2 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci2 > d$ucl2],
                           yend = d$row[d$logUb95Ci2 > d$ucl2])
  } else {
    uclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot2 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr2, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl2, xmax = ucl2)) +
    ggplot2::geom_segment(data = lclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 1, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels2) +
    ggplot2::coord_cartesian(xlim = c(-3, log(5)), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +    #c(min(d$row)+1.25, max(d$row)-coordOffset)) # horizontal locations params
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  col0 <- grid::textGrob("")
  col1 <- grid::textGrob("HCQ vs SSZ", gp = grid::gpar(fontsize = 12), hjust = 1)
  col2 <- grid::textGrob("AZM vs AMX", gp = grid::gpar(fontsize = 12))
  plotGrid <- gridExtra::grid.arrange(col0, col1, col2,
                                      plot0, plot1, plot2,
                                      nrow = 2,
                                      heights = c(0.25, 4),
                                      widths = c(0.5, 1, 1))
  return(plotGrid)
}


plotStaticForestPlotMeta <- function(mainResults,
                                     database,
                                     outcomeOfInterest,
                                     outcomesForPlot,
                                     targetLabel1,
                                     comparatorLabel1,
                                     targetLabel2,
                                     comparatorLabel2) {
  
  meta <- mainResults$databaseId == "Meta-analysis"
  mComparisons <- mainResults$comparison[meta]
  mLogRr <- log(mainResults$calibratedRr[meta])
  mLogLb95Ci <- log(mainResults$calibratedCi95Lb[meta])
  mLogUb95Ci <- log(mainResults$calibratedCi95Ub[meta])
  mAnalyis <- mainResults$analysisShortName[meta]
  mI2 <- mainResults$i2[meta]
  mOutcomes <- mainResults$outcomeName[meta]
  i2s <- mainResults$i2[meta]
  
  # create plot data
  d2 <- data.frame(logRr = mLogRr,
                   logLb95Ci = mLogLb95Ci,
                   logUb95Ci = mLogUb95Ci,
                   analysis = mAnalyis,
                   i2 = mI2,
                   outcome = mOutcomes,
                   type = "ma",
                   comparison = mComparisons,
                   stringsAsFactors = FALSE)
  d <- merge(x = d2[d2$comparison == "HCQ vs SSZ", ],
             y = d2[d2$comparison == "AZM vs AMX", ],
             by = c("analysis", "outcome", "type"),
             all = TRUE,
             suffixes = c("1", "2"))
  nums <- c("logRr1", "logLb95Ci1", "logUb95Ci1", "logRr2", "logLb95Ci2", "logUb95Ci2")
  d[nums][is.na(d[nums])] <- -100
  d$comparison1[is.na(d$comparison1)] <- "HCQ vs SSZ"
  d$comparison2[is.na(d$comparison2)] <- "AZM vs AMX"
  
  dHeader <- data.frame(analysis = "Follow-up",
                        outcome = "Outcome",
                        type = "header",
                        logRr1 = -100,
                        logLb95Ci1 = -100,
                        logUb95Ci1 = -100,
                        i21 = NA,
                        comparison1 = "header",
                        logRr2 = -100,
                        logLb95Ci2 = -100,
                        logUb95Ci2 = -100,
                        i22 = NA,
                        comparison2 = "header",
                        stringsAsFactors = FALSE)
  d <- rbind(dHeader, d)
  
  d$analysisOrder <- match(d$analysis, cohortMethodAnalysis$analysisShortName)
  d$analysisOrder[d$analysis == "Follow-up"] <- 0
  d$outcomeOrder <- match(d$outcome, outcomeOfInterest$outcomeName)
  d$outcomeOrder[d$outcome == "Outcome"] <- 0
  d <- d[order(d$analysisOrder, d$outcomeOrder), ]
  
  # subset outcomes
  d <- d[d$outcome == "Outcome" | d$outcome %in% outcomesForPlot, ]
  d <- d[d$type %in% c("header", "ma"), ]
  
  d$row <- rev(1:nrow(d))
  
  breaks <- c(0.25, 0.5, 1, 2, 4)
  plotLabels1 <- c(0.25, paste("0.5\nFavors", targetLabel1), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel1), 4)
  plotLabels2 <- c(0.25, paste("0.5\nFavors", targetLabel2), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel2), 4)
  
  # labels grob
  labels0 <- data.frame(y = rep(d$row, 2),
                        x = rep(c(1, 1.1), each = nrow(d)),   # horizontal locations params
                        label = c(d$analysis, as.character(d$outcome)),
                        stringsAsFactors = FALSE)
  labels0$label[labels0$x == 1 & duplicated(labels0$label)] <- ""
  
  plot0 <- ggplot2::ggplot(labels0, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels0) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::coord_cartesian(xlim = c(1, 1.25), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +
    ggplot2::xlab(blank) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(color = "white"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "white"),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  # mono grob
  labels1 <- paste0(formatC(exp(d$logRr1),  digits = 2, format = "f"), " (",
                    formatC(exp(d$logLb95Ci1), digits = 2, format = "f"), "-",
                    formatC(exp(d$logUb95Ci1), digits = 2, format = "f"), ")")
  labels1 <- paste(labels1, paste0("[", ifelse(d$i21 < 0.01, "<0.01", d$i21), "]"))
  labels1[grep("NA", labels1)] <- ""
  labels1 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels1,
                        stringsAsFactors = FALSE)
  labels1$label[labels1$x == -5.6 & duplicated(labels1$label)] <- ""
  labels1$label[labels1$label == ""][1] <- "CalHR (95% CI) [I2]"

  d$lcl1 <- ifelse(d$logLb95Ci1 < log(0.175), log(0.175), d$logLb95Ci1)
  d$lcl1 <- ifelse(d$logLb95Ci1 == -100, -100, d$lcl1)
  d$ucl1 <- ifelse(d$logUb95Ci1 > log(6), log(6), d$logUb95Ci1)
  d$lcl1[d$type == "header"] <- -100
  d$ucl1[d$type == "header"] <- -100

  if (length(d$row[d$logLb95Ci1 < d$lcl1 & d$logLb95Ci1 != -100]) > 0) {
    lclData1 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci1 < d$lcl1],
                           yend = d$row[d$logLb95Ci1 < d$lcl1])
  } else {
    lclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci1 > d$ucl1 & d$logUb95Ci1 != -100]) > 0) {
    uclData1 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci1 > d$ucl1],
                           yend = d$row[d$logUb95Ci1 > d$ucl1])
  } else {
    uclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot1 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr1, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl1, xmax = ucl1)) +
    ggplot2::geom_segment(data = lclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, shape = 23, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels1) +
    ggplot2::coord_cartesian(xlim = c(-3, log(5)), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +    #c(min(d$row)+1.25, max(d$row)-coordOffset)) # horizontal locations params
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  #combo grob
  d$lcl2 <- ifelse(d$logLb95Ci2 < log(0.175), log(0.175), d$logLb95Ci2)
  d$lcl2 <- ifelse(d$logLb95Ci2 == -100, -100, d$lcl2)
  d$ucl2 <- ifelse(d$logUb95Ci2 > log(6), log(6), d$logUb95Ci2)
  d$lcl2[d$type == "header"] <- -100
  d$ucl2[d$type == "header"] <- -100
  
  labels2 <- paste0(formatC(exp(d$logRr2),  digits = 2, format = "f"),
                    " (",
                    formatC(exp(d$logLb95Ci2), digits = 2, format = "f"),
                    "-",
                    formatC(exp(d$logUb95Ci2), digits = 2, format = "f"),
                    ")")
  labels2 <- paste(labels2, paste0("[", ifelse(d$i22 < 0.01, "<0.01", d$i22), "]"))
  labels2[grep("NA", labels2)] <- ""
  labels2 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels2,
                        stringsAsFactors = FALSE)
  
  labels2$label[labels2$x == -5.6 & duplicated(labels2$label)] <- ""
  labels2$label[labels2$label == "0.00 (0.00-0.00)"][1] <- "CalHR (95% CI) [I2]"
  labels2$label[labels2$label == "0.00 (0.00-0.00)"] <- ""
  
  if (length(d$row[d$logLb95Ci2 < d$lcl2 & d$logLb95Ci2 != -100])) {
    lclData2 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci2 < d$lcl2],
                           yend = d$row[d$logLb95Ci2 < d$lcl2])
  } else {
    lclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci2 > d$ucl2 & d$logUb95Ci2 != -100]) > 0) {
    uclData2 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci2 > d$ucl2],
                           yend = d$row[d$logUb95Ci2 > d$ucl2])
  } else {
    uclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot2 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr2, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl2, xmax = ucl2)) +
    ggplot2::geom_segment(data = lclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, shape = 23, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels2) +
    ggplot2::coord_cartesian(xlim = c(-3, log(5)), ylim = c(min(d$row)+0.5, max(d$row) - 0.0224*max(d$row))) +    #c(min(d$row)+1.25, max(d$row)-coordOffset)) # horizontal locations params
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  col0 <- grid::textGrob("")
  col1 <- grid::textGrob("HCQ vs SSZ", gp = grid::gpar(fontsize = 12), hjust = 1)
  col2 <- grid::textGrob("AZM vs AMX", gp = grid::gpar(fontsize = 12))
  plotGrid <- gridExtra::grid.arrange(col0, col1, col2,
                                      plot0, plot1, plot2,
                                      nrow = 2,
                                      heights = c(0.125, 4),
                                      widths = c(1, 1, 1))
  return(plotGrid)
}















plotStaticForestPlotAppnx <- function(mainResults,
                                      database,
                                      outcomeOfInterest,
                                      outcomesForPlot,
                                      estimateType,
                                      targetLabel1,
                                      comparatorLabel1,
                                      targetLabel2,
                                      comparatorLabel2) {
  
  meta <- mainResults$databaseId == "Meta-analysis"
  
  comparisons <- mainResults$comparison[!meta]
  logRr <- log(mainResults$calibratedRr[!meta])
  logLb95Ci <- log(mainResults$calibratedCi95Lb[!meta])
  logUb95Ci <- log(mainResults$calibratedCi95Ub[!meta])
  labels <- mainResults$databaseId[!meta]
  i2 <- mainResults$i2[!meta]
  outcomes <- mainResults$outcomeName[!meta]
  
  mComparisons <- mainResults$comparison[meta]
  mLogRr <- log(mainResults$calibratedRr[meta])
  mLogLb95Ci <- log(mainResults$calibratedCi95Lb[meta])
  mLogUb95Ci <- log(mainResults$calibratedCi95Ub[meta])
  mLabels <- mainResults$databaseId[meta]
  mI2 <- mainResults$i2[meta]
  mOutcomes <- mainResults$outcomeName[meta]
  i2s <- mainResults$i2[meta]
  
  # create plot data
  d1 <- data.frame(logRr = logRr,
                   logLb95Ci = logLb95Ci,
                   logUb95Ci = logUb95Ci,
                   name = labels,
                   i2 = i2,
                   outcome = outcomes,
                   type = "db",
                   comparison = comparisons,
                   stringsAsFactors = FALSE)
  
  if (length(mLogRr) > 0) {
    d2 <- data.frame(logRr = mLogRr,
                     logLb95Ci = mLogLb95Ci,
                     logUb95Ci = mLogUb95Ci,
                     name = mLabels,
                     i2 = mI2,
                     outcome = mOutcomes,
                     type = "ma",
                     comparison = mComparisons,
                     stringsAsFactors = FALSE)
    d <- rbind(d1, d2)
  } else {
    d <- d1
  }
  d <- merge(x = d[d$comparison == "HCQ vs SSZ", ],
             y = d[d$comparison == "AZM vs AMX", ],
             by = c("name", "outcome", "type"),
             all = TRUE,
             suffixes = c("1", "2"))
  nums <- c("logRr1", "logLb95Ci1", "logUb95Ci1", "logRr2", "logLb95Ci2", "logUb95Ci2")
  d[nums][is.na(d[nums])] <- -100
  d$comparison1[is.na(d$comparison1)] <- "HCQ vs SSZ"
  d$comparison2[is.na(d$comparison2)] <- "AZM vs AMX"
  
  dHeader <- data.frame(name = "Database",
                        outcome = "Outcome",
                        type = "header",
                        logRr1 = -100,
                        logLb95Ci1 = -100,
                        logUb95Ci1 = -100,
                        i21 = NA,
                        comparison1 = "header",
                        logRr2 = -100,
                        logLb95Ci2 = -100,
                        logUb95Ci2 = -100,
                        i22 = NA,
                        comparison2 = "header",
                        stringsAsFactors = FALSE)
  d <- rbind(dHeader, d)
  d$dbOrder <- match(d$name, database$databaseId)
  d$dbOrder[d$name == "Database"] <- 0
  d$outcomeOrder <- match(d$outcome, outcomeOfInterest$outcomeName)
  d$outcomeOrder[d$name == "Database"] <- 0
  d <- d[order(d$outcomeOrder, d$dbOrder), ]
  
  # subset outcomes
  d <- d[d$outcome == "Outcome" | d$outcome %in% outcomesForPlot, ]
  d <- d[d$type == "header" | d$type %in% estimateType, ]
  
  d$row <- rev(1:nrow(d))
  
  breaks <- c(0.175, 0.25, 0.5, 1, 2, 4, 6)
  plotLabels1 <- c(0.175, 0.25, paste("0.5\nFavors", targetLabel1), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel1), 4, 6)
  plotLabels2 <- c(0.175, 0.25, paste("0.5\nFavors", targetLabel2), paste("1\nCalHR"), paste("2\nFavors", comparatorLabel2), 4, 6)
  
  # labels grob
  labels0 <- data.frame(y = rep(d$row, 2),
                        x = rep(c(1, 2), each = nrow(d)),
                        label = c(d$outcome, as.character(d$name)),
                        stringsAsFactors = FALSE)
  labels0$label[labels0$x == 1 & duplicated(labels0$label)] <- ""
  
  plot0 <- ggplot2::ggplot(labels0, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels0) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::coord_cartesian(xlim = c(1, 2.5),  ylim = c(min(d$row), max(d$row))) +
    ggplot2::xlab(blank) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(color = "white"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "white"),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  # mono grob
  labels1 <- paste0(formatC(exp(d$logRr1),  digits = 2, format = "f"), " (",
                    formatC(exp(d$logLb95Ci1), digits = 2, format = "f"), "-",
                    formatC(exp(d$logUb95Ci1), digits = 2, format = "f"), ")")
  labels1[d$name == "Meta-analysis"] <- paste(labels1[d$name == "Meta-analysis"], paste0("[", ifelse(d$i21[d$name == "Meta-analysis"] < 0.01, "<0.01", d$i21[d$name == "Meta-analysis"]), "]"))
  labels1[grep("NA", labels1)] <- ""
  labels1 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels1,
                        stringsAsFactors = FALSE)
  labels1$label[labels1$x == -5.6 & duplicated(labels1$label)] <- ""
  labels1$label[labels1$label == "0.00 (0.00-0.00)"][1] <- "CalHR (95% CI) [I2]"
  labels1$label[labels1$label == "0.00 (0.00-0.00)"] <- ""
  
  d$lcl1 <- ifelse(d$logLb95Ci1 < log(0.175), log(0.175), d$logLb95Ci1)
  d$ucl1 <- ifelse(d$logUb95Ci1 > log(6), log(6), d$logUb95Ci1)
  d$lcl1 <- ifelse(d$logLb95Ci1 == -100, -100, d$lcl1)
  d$lcl1[d$type == "header"] <- -100
  d$ucl1[d$type == "header"] <- -100
  
  if (length(d$row[d$logLb95Ci1 < d$lcl1 & d$logLb95Ci1 != -100]) > 0) {
    lclData1 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci1 < d$lcl1],
                           yend = d$row[d$logLb95Ci1 < d$lcl1])
  } else {
    lclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci1 > d$ucl1 & d$logUb95Ci1 != -100]) > 0) {
    uclData1 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci1 > d$ucl1],
                           yend = d$row[d$logUb95Ci1 > d$ucl1])
  } else {
    uclData1 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot1 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr1, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl1, xmax = ucl1)) +
    ggplot2::geom_segment(data = lclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData1,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 1, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels1) +
    ggplot2::coord_cartesian(xlim =  c(-3, log(5)),  ylim = c(min(d$row), max(d$row))) +
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  #combo grob
  d$lcl2 <- ifelse(d$logLb95Ci2 < log(0.175), log(0.175), d$logLb95Ci2)
  d$lcl2 <- ifelse(d$logLb95Ci2 == -100, -100, d$lcl2)
  d$ucl2 <- ifelse(d$logUb95Ci2 > log(6), log(6), d$logUb95Ci2)
  d$lcl2[d$type == "header"] <- -100
  d$ucl2[d$type == "header"] <- -100
  
  labels2 <- paste0(formatC(exp(d$logRr2),  digits = 2, format = "f"),
                    " (",
                    formatC(exp(d$logLb95Ci2), digits = 2, format = "f"),
                    "-",
                    formatC(exp(d$logUb95Ci2), digits = 2, format = "f"),
                    ")")
  labels2[d$name == "Meta-analysis"] <- paste(labels2[d$name == "Meta-analysis"], paste0("[", ifelse(d$i22[d$name == "Meta-analysis"] < 0.01, "<0.01", d$i22[d$name == "Meta-analysis"]), "]"))
  labels2[grep("NA", labels2)] <- ""
  labels2 <- data.frame(y = d$row,
                        x = rep(-3, nrow(d)),
                        label = labels2,
                        stringsAsFactors = FALSE)
  
  labels2$label[labels2$x == -5.6 & duplicated(labels2$label)] <- ""
  labels2$label[labels2$label == "0.00 (0.00-0.00)"][1] <- "CalHR (95% CI) [I2]"
  labels2$label[labels2$label == "0.00 (0.00-0.00)"] <- ""
  
  if (length(d$row[d$logLb95Ci2 < d$lcl2 & d$logLb95Ci2 != -100])) {
    lclData2 <- data.frame(x = log(0.175),
                           xend = log(0.175),
                           y = d$row[d$logLb95Ci2 < d$lcl2],
                           yend = d$row[d$logLb95Ci2 < d$lcl2])
  } else {
    lclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  if (length(d$row[d$logUb95Ci2 > d$ucl2 & d$logUb95Ci2 != -100]) > 0) {
    uclData2 <- data.frame(x = log(6),
                           xend = log(6),
                           y = d$row[d$logUb95Ci2 > d$ucl2],
                           yend = d$row[d$logUb95Ci2 > d$ucl2])
  } else {
    uclData2 <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot2 <- ggplot2::ggplot(d, ggplot2::aes(x = logRr2, y = row)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(size = 0.5, height = 0, ggplot2::aes(xmin = lcl2, xmax = ucl2)) +
    ggplot2::geom_segment(data = lclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_segment(data = uclData2,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.075, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 1, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels2) +
    
    ggplot2::coord_cartesian(xlim =  c(-3, log(5)),  ylim = c(min(d$row), max(d$row))) +
    
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  col0 <- grid::textGrob("")
  col1 <- grid::textGrob("HCQ vs SSZ", gp = grid::gpar(fontsize = 12), hjust = 1)
  col2 <- grid::textGrob("AZM vs AMX", gp = grid::gpar(fontsize = 12))
  plotGrid <- gridExtra::grid.arrange(col0, col1, col2,
                                      plot0, plot1, plot2,
                                      nrow = 2,
                                      heights = c(0.25, 4),
                                      widths = c(0.5, 1, 1))
  return(plotGrid)
}


