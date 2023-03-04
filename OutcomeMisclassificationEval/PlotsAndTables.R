prepareFollowUpDistTable <- function(followUpDist) {
  targetRow <- data.frame(Database = followUpDist$databaseId,
                          Cohort = "Target",
                          Min = followUpDist$targetMinDays,
                          P10 = followUpDist$targetP10Days,
                          P25 = followUpDist$targetP25Days,
                          Median = followUpDist$targetMedianDays,
                          P75 = followUpDist$targetP75Days,
                          P90 = followUpDist$targetP90Days,
                          Max = followUpDist$targetMaxDays)
  comparatorRow <- data.frame(Database = followUpDist$databaseId,
                              Cohort = "Comparator",
                              Min = followUpDist$comparatorMinDays,
                              P10 = followUpDist$comparatorP10Days,
                              P25 = followUpDist$comparatorP25Days,
                              Median = followUpDist$comparatorMedianDays,
                              P75 = followUpDist$comparatorP75Days,
                              P90 = followUpDist$comparatorP90Days,
                              Max = followUpDist$comparatorMaxDays)
  table <- rbind(targetRow, comparatorRow)
  table$Min <- formatC(table$Min, big.mark = ",", format = "d")
  table$P10 <- formatC(table$P10, big.mark = ",", format = "d")
  table$P25 <- formatC(table$P25, big.mark = ",", format = "d")
  table$Median <- formatC(table$Median, big.mark = ",", format = "d")
  table$P75 <- formatC(table$P75, big.mark = ",", format = "d")
  table$P90 <- formatC(table$P90, big.mark = ",", format = "d")
  table$Max <- formatC(table$Max, big.mark = ",", format = "d")
  if (length(unique(followUpDist$databaseId)) == 1)
    table$Database <- NULL
  return(table)
}

prepareMainResultsTable <- function(mainResults, analyses) {
  table <- mainResults
  table$hr <- sprintf("%.2f (%.2f - %.2f)", mainResults$rr, mainResults$ci95Lb, mainResults$ci95Ub)
  table$p <- sprintf("%.2f", table$p)
  table$calHr <- sprintf("%.2f (%.2f - %.2f)",
                         mainResults$calibratedRr,
                         mainResults$calibratedCi95Lb,
                         mainResults$calibratedCi95Ub)
  table$calibratedP <- sprintf("%.2f", table$calibratedP)
  table <- merge(table, analyses)
  table <- table[, c("description", "hr", "p", "calHr", "calibratedP")]
  colnames(table) <- c("Analysis", "HR (95% CI)", "P", "Cal. HR (95% CI)", "Cal. p")
  return(table)
}

prepareValidationTable <- function(validationTable) {
  validationTable$TP <- formatC(validationTable$TP, big.mark = ",", format = "d")
  validationTable$FP <- formatC(validationTable$FP, big.mark = ",", format = "d")
  validationTable$FN <- formatC(validationTable$FN, big.mark = ",", format = "d")
  validationTable$TN <- formatC(validationTable$TN, big.mark = ",", format = "d")
  return(validationTable)
}

prepareCountsTable <- function(mainResults,
                               analyses,
                               alpha = 0.05) {
  table <- merge(mainResults, analyses)
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- qnorm(1 - alpha/2)
  zBeta <- -qnorm(1 - power)
  pA <- table$target/(table$target + table$comparator)
  pB <- 1 - pA
  totalEvents <- abs(table$eventsTarget) + abs(table$eventsComparator)
  table$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  table$targetYears <- table$targetDays/365.25
  table$comparatorYears <- table$comparatorDays/365.25
  table$targetOdds <- table$eventsTarget/(table$target - table$eventsTarget)
  table$comparatorOdds <- table$eventsComparator/(table$comparator - table$eventsComparator)

  a <- table$eventsTarget
  b <- table$eventsComparator
  c <- table$target - table$eventsTarget
  d <- table$comparator - table$eventsComparator
  ab <- a + b
  cd <- c + d
  ac <- a + c
  bd <- b + d
  abcd <- a + b + c + d
  countsTable <- data.frame(target = c(a, c, ac),
                            comparator = c(b, d, bd),
                            total = c(ab, cd, abcd),
                            row.names = c(sprintf("%s [+]", table$outcomeName), sprintf("%s [-]", table$outcomeName), "Total"))
  countsTable$target <- formatC(round(countsTable$target), big.mark = ",", format = "d")
  countsTable$comparator <- formatC(round(countsTable$comparator), big.mark = ",", format = "d")
  countsTable$total <- formatC(round(countsTable$total), big.mark = ",", format = "d")

  powerTable <- table[, c("targetOdds", "comparatorOdds", "mdrr")]
  powerTable$targetOdds <- sprintf("%.4f", powerTable$targetOdds)
  powerTable$comparatorOdds <- sprintf("%.4f", powerTable$comparatorOdds)
  powerTable$mdrr <- sprintf("%.2f", powerTable$mdrr)

  obs.or <- (a/b) / (c/d)
  se.log.obs.or <- sqrt(1/a + 1/b + 1/c + 1/d)

  lci.obs.or <- exp(log(obs.or) - qnorm(1 - alpha/2) * se.log.obs.or)
  uci.obs.or <- exp(log(obs.or) + qnorm(1 - alpha/2) * se.log.obs.or)

  orTable <- table[, c("targetOdds", "comparatorOdds")]
  orTable$oddsRatio <- obs.or
  orTable$ci95lb <- lci.obs.or
  orTable$ci95ub <- uci.obs.or
  orTable$targetOdds <- sprintf("%.4f", orTable$targetOdds)
  orTable$comparatorOdds <- sprintf("%.4f", orTable$comparatorOdds)

  return(list(countsTable, powerTable, orTable))
}

prepareMdCountsTable <- function(mainResults,
                                 analyses,
                                 sens,
                                 spec,
                                 alpha = 0.05) {

  table <- merge(mainResults, analyses)

  a <- table$eventsTarget
  b <- table$eventsComparator
  c <- table$target - table$eventsTarget
  d <- table$comparator - table$eventsComparator
  ac <- a + c
  bd <- b + d

  A <- (a - (1 - spec) * ac) / (sens - (1 - spec)) # cast as doubles?
  B <- (b - (1 - spec) * bd) / (sens - (1 - spec))
  C <- ac - A
  D <- bd - B

  AB <- A + B
  CD <- C + D
  AC <- A + C
  BD <- B + D
  ABCD <- A + B + C + D

  mdCountsTable <- data.frame(target = c(A, C, AC),
                              comparator = c(B, D, BD),
                              total = c(AB, CD, ABCD),
                              row.names = c(sprintf("%s [+]", table$outcomeName), sprintf("%s [-]", table$outcomeName), "Total"))
  mdCountsTable$target <- formatC(round(mdCountsTable$target), big.mark = ",", format = "d")
  mdCountsTable$comparator <- formatC(round(mdCountsTable$comparator), big.mark = ",", format = "d")
  mdCountsTable$total <- formatC(round(mdCountsTable$total), big.mark = ",", format = "d")

  if(A < 1 | B < 1 | C < 1 | D < 1) {
    orTable <- data.frame(targetOdds = NA,
                          comparatorOdds = NA,
                          oddsRatio = NA,
                          ci95lb = NA,
                          ci95ub = NA)
  } else {
    targetOdds <- (A/C)
    comparatorOdds <- (B/D)
    corr.or <- targetOdds / comparatorOdds
   
    num1 <- ac * a * c * (sens + spec - 1)^2
    den1 <- (ac * sens - a)^2 * (ac * spec - c)^2
    num2 <- bd * b * d * (sens + spec - 1)^2
    den2 <- (bd * sens - b)^2 * (bd * spec - d)^2
    se.log.corr.or <- sqrt((num1 / den1) + (num2 / den2))
    lci.corr.or <- exp(log(corr.or) - qnorm(1 - alpha/2) * se.log.corr.or)
    uci.corr.or <- exp(log(corr.or) + qnorm(1 - alpha/2) * se.log.corr.or)

    orTable <- table
    orTable$targetOdds <- targetOdds
    orTable$comparatorOdds <- comparatorOdds
    orTable <- orTable[, c("targetOdds", "comparatorOdds")]
    orTable$oddsRatio <- corr.or
    orTable$ci95lb <- lci.corr.or
    orTable$ci95ub <- uci.corr.or
    orTable$targetOdds <- sprintf("%.4f", orTable$targetOdds)
    orTable$comparatorOdds <- sprintf("%.4f", orTable$comparatorOdds)
  }
  return(list(mdCountsTable, orTable))
}



prepareTable1 <- function(balance,
                          beforeLabel = "Before stratification",
                          afterLabel = "After stratification",
                          targetLabel = "Target",
                          comparatorLabel = "Comparator",
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = "Table1Specs.csv") {
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }
  specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)

  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }

  formatPercent <- function(x) {
    result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("^-", "<", result)
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }

  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }

  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = NA,
                                                           beforeMatchingMeanComparator = NA,
                                                           beforeMatchingStdDiff = NA,
                                                           afterMatchingMeanTreated = NA,
                                                           afterMatchingMeanComparator = NA,
                                                           afterMatchingStdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                   space,
                                                                                   space,
                                                                                   space,
                                                                                   balanceSubset$covariateName),
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$beforeMatchingMeanTreated <- formatPercent(resultsTable$beforeMatchingMeanTreated)
  resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator)
  resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff)
  resultsTable$afterMatchingMeanTreated <- formatPercent(resultsTable$afterMatchingMeanTreated)
  resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator)
  resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff)

  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$beforeMatchingMeanTreated <- targetLabel
  headerRow$beforeMatchingMeanComparator <- comparatorLabel
  headerRow$afterMatchingMeanTreated <- targetLabel
  headerRow$afterMatchingMeanComparator <- comparatorLabel

  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  subHeaderRow$beforeMatchingMeanTreated <- "%"
  subHeaderRow$beforeMatchingMeanComparator <- "%"
  subHeaderRow$beforeMatchingStdDiff <- "Std. diff"
  subHeaderRow$afterMatchingMeanTreated <- "%"
  subHeaderRow$afterMatchingMeanComparator <- "%"
  subHeaderRow$afterMatchingStdDiff <- "Std. diff"

  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)

  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}

prepareRawTable1 <- function(balance,
                             percentDigits = 1,
                             stdDiffDigits = 2,
                             output = "latex") {
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }

  formatPercent <- function(x) {
    result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("^-", "<", result)
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }

  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }

  resultsTable <- balance[, c("covariateName", "analysisId",
                              "beforeMatchingMeanTreated", "beforeMatchingMeanComparator", "beforeMatchingStdDiff",
                              "afterMatchingMeanTreated", "afterMatchingMeanComparator", "afterMatchingStdDiff")]
  resultsTable$covariateName <- stringi::stri_trans_general(resultsTable$covariateName, "Latin-ASCII")
  resultsTable <- resultsTable[order(abs(resultsTable$afterMatchingStdDiff), decreasing = TRUE), ]

  resultsTable$beforeMatchingMeanTreated[resultsTable$analysisId < 800] <- formatPercent(resultsTable$beforeMatchingMeanTreated[resultsTable$analysisId < 800])
  resultsTable$beforeMatchingMeanComparator[resultsTable$analysisId < 800] <- formatPercent(resultsTable$beforeMatchingMeanComparator[resultsTable$analysisId < 800])
  resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff)

  resultsTable$afterMatchingMeanTreated[resultsTable$analysisId < 800] <- formatPercent(resultsTable$afterMatchingMeanTreated[resultsTable$analysisId < 800])
  resultsTable$afterMatchingMeanComparator[resultsTable$analysisId < 800] <- formatPercent(resultsTable$afterMatchingMeanComparator[resultsTable$analysisId < 800])
  resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff)
  resultsTable <- resultsTable[-2]

  resultsTable <- rbind(c("", "Target", "", "", "Comparator", "", ""),
                        c("Characteristic", "%", "%", "Std. diff.", "%", "%", "Std. diff"),
                        resultsTable)
  return(resultsTable)
}

plotPs <- function(ps, targetName, comparatorName) {
  if (is.null(ps$databaseId)) {
    ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
                data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))

  } else {
    ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, databaseId = ps$databaseId, group = targetName),
                data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, databaseId = ps$databaseId, group = comparatorName))
  }
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  if (!is.null(ps$databaseId)) {
    plot <- plot + ggplot2::facet_grid(databaseId~., switch = "both") +
      ggplot2::theme(legend.position = "right")
  }
  return(plot)
}

plotAllPs <- function(ps) {
  ps <- rbind(data.frame(targetName = ps$targetName,
                         comparatorName = ps$comparatorName,
                         x = ps$preferenceScore,
                         y = ps$targetDensity,
                         group = "Target"),
              data.frame(targetName = ps$targetName,
                         comparatorName = ps$comparatorName,
                         x = ps$preferenceScore,
                         y = ps$comparatorDensity,
                         group = "Comparator"))
  ps$group <- factor(ps$group, levels = c("Target", "Comparator"))
  plot <- ggplot2::ggplot(ps, ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::facet_grid(targetName ~ comparatorName) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0),
                   strip.text.y = ggplot2::element_text(size = 12, angle = 0, hjust = 0),
                   panel.spacing = ggplot2::unit(0.1, "lines"),
                   legend.position = "none")
  return(plot)
}


plotCovariateBalanceScatterPlot <- function(balance, beforeLabel, afterLabel) {
  limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE),
              max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE))
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = theme)
  return(plot)
}

drawAttritionDiagram <- function(attrition,
                                 targetLabel = "Target",
                                 comparatorLabel = "Comparator") {
  addStep <- function(data, attrition, row) {
    label <- paste(strwrap(as.character(attrition$description[row]), width = 30), collapse = "\n")
    data$leftBoxText[length(data$leftBoxText) + 1] <- label
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel,
                                                              ": n = ",
                                                              data$currentTarget - attrition$targetPersons[row],
                                                              "\n",
                                                              comparatorLabel,
                                                              ": n = ",
                                                              data$currentComparator - attrition$comparatorPersons[row],
                                                              sep = "")
    data$currentTarget <- attrition$targetPersons[row]
    data$currentComparator <- attrition$comparatorPersons[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("Exposed:\n",
                                     targetLabel,
                                     ": n = ",
                                     attrition$targetPersons[1],
                                     "\n",
                                     comparatorLabel,
                                     ": n = ",
                                     attrition$comparatorPersons[1],
                                     sep = "")), rightBoxText = c(""), currentTarget = attrition$targetPersons[1], currentComparator = attrition$comparatorPersons[1])
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }

  data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study population:\n",
                                                          targetLabel,
                                                          ": n = ",
                                                          data$currentTarget,
                                                          "\n",
                                                          comparatorLabel,
                                                          ": n = ",
                                                          data$currentComparator,
                                                          sep = "")
  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)

  boxHeight <- (1/nSteps) - 0.03
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }

  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 + arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2) + shadowOffset,
                                                    ymin = y - (boxHeight/2) - shadowOffset,
                                                    xmax = x + (boxWidth/2) + shadowOffset,
                                                    ymax = y + (boxHeight/2) - shadowOffset), fill = rgb(0,
                                                                                                         0,
                                                                                                         0,
                                                                                                         alpha = 0.2))
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2),
                                                    ymin = y - (boxHeight/2),
                                                    xmax = x + (boxWidth/2),
                                                    ymax = y + (boxHeight/2)), fill = rgb(0.94,
                                                                                          0.94,
                                                                                          0.94), color = "black")
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, y = y, label = paste("\"", text, "\"",
                                                                                sep = "")),
                                hjust = hjust,
                                size = 3.7)
    return(p)
  }

  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
    p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
  }
  for (i in 2:(nSteps - 1)) {
    p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, y(i))
    p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
  }
  for (i in 2:(nSteps - 1)) {
    p <- box(p, x(2), y(i))
  }
  for (i in 1:nSteps) {
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps - 1)) {
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())

  return(p)
}

prettyHr <- function(x) {
  result <- sprintf("%.2f", x)
  result[is.na(x) | x > 100] <- "NA"
  return(result)
}

preparePropensityModelTable <- function(model) {
  rnd <- function(x) {
    ifelse(x > 10, sprintf("%.1f", x), sprintf("%.2f", x))
  }
  table <- model[order(-abs(model$coefficient)), c("coefficient", "covariateName")]
  table$coefficient <- sprintf("%.2f", table$coefficient)
  colnames(table) <- c("Beta", "Covariate")
  return(table)
}

plotForest <- function(results) {
  results <- results[!is.na(results$seLogRr), ]
  targetName <- "target"
  comparatorName <- "comparator"
  breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)
  labels <- c(0.125, paste("0.25\nFavors", targetName), 0.5, 1, 2, paste("4\nFavors", comparatorName), 8)

  data <- data.frame(logRr = results$logRr,
                     logLb = results$logRr + qnorm(0.025) * results$seLogRr,
                     logUb = results$logRr + qnorm(0.975) * results$seLogRr,
                     databaseId = as.factor(results$databaseId),
                     analysisDescription = as.factor(results$analysisDescription))
  limits <- rev(unique(data$analysisDescription))

  plot <- ggplot2::ggplot(data,
                          ggplot2::aes(x = exp(logRr),
                                       y = analysisDescription,
                                       xmin = exp(logLb),
                                       xmax = exp(logUb))) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.1) +
    ggplot2::geom_vline(xintercept = 1, colour = "#000000", lty = 1, size = 1) +
    ggplot2::geom_errorbarh(height = 0, size = 1.25, alpha = 0.85) +
    ggplot2::geom_point(shape = 18, size = 4, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c(rgb(0.8, 0, 0), rgb(0, 0, 0))) +
    ggplot2::scale_y_discrete(limits = limits) +
    ggplot2::scale_x_continuous("Odds ratio", trans = "log10", breaks = breaks, labels = labels) +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA",colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#EEEEEE"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   legend.position = "none") +
    ggplot2::facet_wrap(~ databaseId, nrow = 1)
  return(plot)
}

plotMdForest <- function(results) {
  targetName <- "target"
  comparatorName <- "comparator"
  breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)
  labels <- c(0.125, paste("0.25\nFavors", targetName), 0.5, 1, 2, paste("4\nFavors", comparatorName), 8)

  data <- data.frame(logRr = log(results$OR),
                     logLb = log(results$LB),
                     logUb = log(results$UB),
                     analysisDescription = as.factor(c("No QBA", "Simple QBA")))
  limits <- rev(as.factor(c("No QBA", "Simple QBA")))

  plot <- ggplot2::ggplot(data,
                          ggplot2::aes(x = exp(logRr),
                                       y = analysisDescription,
                                       xmin = exp(logLb),
                                       xmax = exp(logUb))) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.1) +
    ggplot2::geom_vline(xintercept = 1, colour = "#000000", lty = 1, size = 1) +
    ggplot2::geom_errorbarh(height = 0, size = 1.25, alpha = 0.85) +
    ggplot2::geom_point(shape = 18, size = 4, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c(rgb(0.8, 0, 0), rgb(0, 0, 0))) +
    ggplot2::scale_y_discrete(limits = limits) +
    ggplot2::scale_x_continuous("Odds ratio", trans = "log10", breaks = breaks, labels = labels) +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA",colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#EEEEEE"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12),
                   legend.position = "none")
}
