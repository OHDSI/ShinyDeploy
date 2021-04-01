library(ggplot2)
library(dplyr)

plotScatter <- function(d) {
  d$Significant <- d$ci95Lb > d$effectSize | d$ci95Ub < d$effectSize
  
  temp1 <- aggregate(Significant ~ Group, data = d, length)
  temp2 <- aggregate(Significant ~ Group, data = d, mean)
  
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs includes ",
                            substr(as.character(temp2$Group), start = 20, stop = nchar(as.character(temp2$Group))))
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(substr(as.character(dd$Group), start = 20, stop = nchar(as.character(dd$Group))))
  
  breaks <- c(0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  alpha <- 1 - min(0.95*(nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot(d, aes(x = logRr, y= seLogRr)) +
    geom_abline(aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)), color = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    geom_abline(aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)), color = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    geom_hline(yintercept = 0) +
    geom_label(x = log(0.26), y = 0.95, alpha = 1, hjust = "left", aes(label = nLabel), size = 5, data = dd) +
    geom_label(x = log(0.26), y = 0.8, alpha = 1, hjust = "left", aes(label = meanLabel), size = 5, data = dd) +
    scale_x_continuous("Estimated effect size", limits = log(c(0.25, 10)), breaks = log(breaks), labels = breaks) +
    scale_y_continuous("Standard Error", limits = c(0, 1)) +
    facet_grid(. ~ Group) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = themeRA,
          axis.text.x = theme,
          axis.title = theme,
          legend.key = element_blank(),
          strip.text.x = theme,
          strip.text.y = theme,
          strip.background = element_blank(),
          legend.position = "none")
  return(plot)
}

plotRocsInjectedSignals <- function(logRr, trueLogRr, showAucs, fileName = NULL) {
  trueLogRrLevels <- unique(trueLogRr)
  trueLogRrLevels <- trueLogRrLevels[order(trueLogRrLevels)]
  
  
  idx <- is.na(logRr) | is.infinite(logRr) 
  logRr[idx] <- rep(0, sum(idx))
  
  allData <- data.frame()
  aucs <- c()
  labels <- c()
  overall <- c()
  for (trueLogRrLevel in trueLogRrLevels) {
    if (trueLogRrLevel != 0 ) {
      data <- data.frame(logRr = logRr[trueLogRr == 0 | trueLogRr == trueLogRrLevel], 
                         trueLogRr = trueLogRr[trueLogRr == 0 | trueLogRr == trueLogRrLevel])
      data$truth <- data$trueLogRr != 0
      label <- paste("True effect size =", exp(trueLogRrLevel))
      roc <- pROC::roc(data$truth, data$logRr, algorithm = 3, quiet = TRUE)
      if (showAucs) {
        aucs <- c(aucs, pROC::auc(roc))
        labels <- c(labels, label)
        overall <- c(overall, FALSE)
      }
      data <- data.frame(sens = roc$sensitivities, 
                         fpRate = 1 - roc$specificities, 
                         label = label,
                         overall = FALSE,
                         stringsAsFactors = FALSE)
      data <- data[order(data$sens, data$fpRate), ]
      allData <- rbind(allData, data)
    }
  }
  # Overall ROC
  data <- data.frame(logRr = logRr, 
                     trueLogRr = trueLogRr)
  data$truth <- data$trueLogRr != 0
  roc <- pROC::roc(data$truth, data$logRr, algorithm = 3, quiet = TRUE)
  if (showAucs) {
    aucs <- c(aucs, pROC::auc(roc))
    labels <- c(labels, "Overall")
    overall <- c(overall, TRUE)
  }
  data <- data.frame(sens = roc$sensitivities, 
                     fpRate = 1 - roc$specificities, 
                     label = "Overall", 
                     overall = TRUE,
                     stringsAsFactors = FALSE)
  data <- data[order(data$sens, data$fpRate), ]
  allData <- rbind(allData, data)
  
  allData$label <- factor(allData$label, levels = c(paste("True effect size =", exp(trueLogRrLevels)), "Overall"))
  breaks <- seq(0, 1, by = 0.2)
  theme <- element_text(colour = "#000000", size = 15)
  themeRA <- element_text(colour = "#000000", size = 15, hjust = 1)
  plot <- ggplot(allData, aes(x = fpRate, y = sens, group = label, color = label, fill = label)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_line(aes(linetype = overall), alpha = 0.5, size = 1) +
    scale_x_continuous("1 - specificity", breaks = breaks, labels = breaks) +
    scale_y_continuous("Sensitivity", breaks = breaks, labels = breaks) +
    # scale_color_manual(values = c("#6a7fd2", "#0ca82e", "#74168e", "#000000")) +
    scale_color_manual(values = c("#781C86", "#83BA70", "#547BD3", "#000000")) +
    labs(color = "True effect size", linetype = "Overall") +
    theme(panel.grid.minor = element_blank(),
          axis.text.y = themeRA,
          axis.text.x = theme,
          axis.title = theme,
          legend.key = element_blank(),
          strip.text.x = theme,
          strip.text.y = theme,
          strip.background = element_blank(),
          legend.position = "right",
          legend.text = theme,
          legend.title = theme)
  
  
  if (showAucs) {
    aucs <- data.frame(auc = aucs, label = labels) 
    aucs <- aucs[order(aucs$label), ]
    for (i in 1:nrow(aucs)) {
      label <- paste0(aucs$label[i], ": AUC = ", format(round(aucs$auc[i], 2), nsmall = 2))
      plot <- plot + geom_text(label = label, x = 1, y = 0.4 - (i*0.1), hjust = 1, color = "#000000", size = 5)
    }
  }
  if (!is.null(fileName))
    ggsave(fileName, plot, width = 5.5, height = 4.5, dpi = 400)
  return(plot)
}

plotOverview <- function(metrics, metric, strataSubset, calibrated) {
  yLabel <- paste0(metric, if (calibrated == "Calibrated") " after empirical calibration" else "")
  point <- scales::format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)
  fiveColors <- c(
    "#781C86",
    "#83BA70",
    "#D3AE4E",
    "#547BD3",
    "#DF4327"
  )
  plot <- ggplot2::ggplot(metrics, ggplot2::aes(x = x, y = metric, color = tidyMethod)) +
    ggplot2::geom_vline(xintercept = 0.5 + 0:nrow(strataSubset), linetype = "dashed") +
    ggplot2::geom_point(size = 4.5, alpha = 0.5, shape = 16) +
    ggplot2::scale_x_continuous("Stratum", breaks = strataSubset$x, labels = strataSubset$stratum) +
    ggplot2::scale_colour_manual(values = fiveColors) +
    ggplot2::facet_grid(database~., scales = "free_y") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#F0F0F0F0", colour = NA),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "#CCCCCC"),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 20))
  if (metric %in% c("Mean precision (1/SE^2)", "Mean squared error (MSE)")) {
    plot <- plot + ggplot2::scale_y_log10(yLabel, labels = point)
  } else {
    plot <- plot + ggplot2::scale_y_continuous(yLabel, labels = point)
  }
  return(plot)
}


addTrueEffectSize <- function(estimates, negativeControlOutcome, positiveControlOutcome) {
  allControls <- bind_rows(negativeControlOutcome %>%
                             mutate(effectSize = 1) %>%
                             select(.data$outcomeId, .data$outcomeName, .data$effectSize),
                           positiveControlOutcome %>%
                             select(.data$outcomeId, .data$outcomeName, .data$effectSize))
  estimates <- estimates %>%
    inner_join(allControls, by = "outcomeId")
  return(estimates)
}

# estimates <- subset[subset$method == "SCCS" & subset$analysisId == 2 & subset$periodId == 9, ]
computeEffectEstimateMetrics <- function(estimates, trueRr = "Overall") {
  if (!"effectSize" %in% colnames(estimates))
    stop("Must add column 'effectSize' to estimates (e.g. using addTrueEffectSize())")
  
  if (trueRr == "Overall") {
    forEval <- estimates
  } else {
    forEval <- estimates[estimates$effectSize == as.numeric(trueRr), ]
  }
  idx <- is.na(forEval$logRr) | is.infinite(forEval$logRr)
  forEval$logRr[idx] <- rep(0, sum(idx))
  idx <- idx |  (is.na(forEval$seLogRr) | is.infinite(forEval$seLogRr))
  forEval$seLogRr[idx] <- rep(999, sum(idx))
  forEval$ci95Lb[idx] <- rep(0, sum(idx))
  forEval$ci95Ub[idx] <- rep(999, sum(idx))
  forEval$p[idx] <- rep(1, sum(idx))
  
  idx <- forEval$seLogRr == 0
  forEval$seLogRr[idx] <- rep(0.001, sum(idx))
  
  nonEstimable <- round(mean(forEval$seLogRr >= 99), 2)
  mse <- round(mean((forEval$logRr - log(forEval$effectSize))^2), 2)
  coverage <- round(mean(forEval$ci95Lb < forEval$effectSize & forEval$ci95Ub > forEval$effectSize), 2)
  meanP <- round(-1 + exp(mean(log(1 + (1/(forEval$seLogRr^2))))), 2)
  if (trueRr == "Overall") {
    roc <- pROC::roc(forEval$effectSize > 1, forEval$logRr, algorithm = 3, quiet = TRUE)
    auc <- round(pROC::auc(roc), 2)
    type1 <- round(mean(forEval$p[forEval$effectSize == 1] < 0.05), 2)
    type2 <- round(mean(forEval$p[forEval$effectSize > 1] >= 0.05), 2)
  } else if (trueRr == "1") {
    roc <- NA
    auc <- NA
    type1 <- round(mean(forEval$p < 0.05), 2)  
    type2 <- NA
  } else {
    if (trueRr == ">1") {
      negAndPos <- estimates[estimates$effectSize > 1 | estimates$effectSize == 1, ]
    } else {
      negAndPos <- estimates[estimates$effectSize == as.numeric(trueRr) | estimates$effectSize == 1, ]
    }
    roc <- pROC::roc(negAndPos$effectSize > 1, negAndPos$logRr, algorithm = 3)
    auc <- round(pROC::auc(roc), 2)
    type1 <- NA
    type2 <- round(mean(forEval$p[forEval$effectSize > 1] >= 0.05), 2)  
  }
  
  return(tibble(method = estimates$method[1],
                analysisId = estimates$analysisId[1],
                auc = auc, 
                coverage = coverage, 
                meanP = meanP, 
                mse = mse, 
                type1 = type1, 
                type2 = type2, 
                nonEstimable = nonEstimable))
}

computeMaxSprtMetricsPerPeriod <- function(estimates) {
  timeToSignal <- estimates %>%
    mutate(signal = .data$llr > .data$criticalValue) %>%
    filter(.data$signal) %>%
    group_by(.data$outcomeId) %>%
    summarize(firstSignalPeriodId = min(.data$periodId))
  
  timeToSignal <- estimates %>%
    distinct(.data$outcomeId, .data$effectSize) %>%
    left_join(timeToSignal, by = "outcomeId") %>%
    mutate(firstSignalPeriodId = if_else(is.na(.data$firstSignalPeriodId), Inf, as.numeric(.data$firstSignalPeriodId)))
  
  # periodId <- 9
  computeSensSpec <- function(periodId) {
    timeToSignal$signalInPeriod <- timeToSignal$firstSignalPeriodId <= periodId
    sensitivity <- mean(timeToSignal$signalInPeriod[timeToSignal$effectSize > 1])
    specificity <- 1 - mean(timeToSignal$signalInPeriod[timeToSignal$effectSize == 1])
    
    # sensitivity <-  timeToSignal %>%
    #   filter(.data$effectSize > 1) %>%
    #   summarise(sensitivity = mean(.data$firstSignalPeriodId <= periodId)) %>%
    #   pull()
    # specificity <-  timeToSignal %>%
    #   filter(.data$effectSize == 1) %>%
    #   summarise(specificity = 1 - mean(.data$firstSignalPeriodId <= periodId)) %>%
    #   pull()
    # return(tibble(periodId = periodId,
    #               sensitivity = sensitivity,
    #               specificity = specificity))
    return(c(sensitivity = sensitivity,
             specificity = specificity))
  }
  periodIds <- 1:max(estimates$periodId)
  sensSpec <- as_tibble(t(sapply(periodIds, computeSensSpec)))
  sensSpec$periodId <- periodIds
  # sensSpec <- lapply(1:max(estimates$periodId), computeSensSpec)
  # sensSpec <- bind_rows(sensSpec)
  return(sensSpec)
}

# estimates = split(subset, paste(subset$method, subset$analysisId))[[1]]
# estimates <- subset[subset$method == "HistoricalComparator" & subset$analysisId == 1, ]
# estimates = addTrueEffectSize(estimates, negativeControlOutcome, positiveControlOutcome)
computeMaxSprtMetrics <- function(estimates, trueRr = "Overall") {
  if (!"effectSize" %in% colnames(estimates))
    stop("Must add column 'effectSize' to estimates (e.g. using addTrueEffectSize())")
  
  if (trueRr == "Overall") {
    forEval <- estimates
  } else {
    forEval <- estimates[estimates$effectSize == 1 | estimates$effectSize == as.numeric(trueRr), ]
  }
  
  metricsPerPeriod <- computeMaxSprtMetricsPerPeriod(forEval)
  firstPeriod80Sens <- metricsPerPeriod %>%
    filter(.data$sensitivity >= 0.80) %>%
    summarize(periodId = min(.data$periodId)) %>%
    pull()
  if (is.infinite(firstPeriod80Sens)) {
    result <- tibble(mehod = estimates$method[1],
                     analysisId = estimates$analysisId[1],
                     firstPeriod80Sens = NA,
                     sensitivity = NA,
                     specificity = NA)
  } else {
    result <- metricsPerPeriod %>%
      filter(.data$periodId == firstPeriod80Sens) %>%
      transmute(mehod = estimates$method[1],
                analysisId = estimates$analysisId[1],
                firstPeriod80Sens = !!firstPeriod80Sens,
                sensitivity = round(.data$sensitivity, 2),
                specificity = round(.data$specificity, 2))
  }
  return(result)
}

plotMaxSprtSensSpecAcrossMethods <- function(estimates, trueRr = "Overall") {
  if (!"effectSize" %in% colnames(estimates))
    stop("Must add column 'effectSize' to estimates (e.g. using addTrueEffectSize())")
  
  if (trueRr != "Overall") {
    estimates <- estimates[estimates$effectSize == 1 | estimates$effectSize == as.numeric(trueRr), ]
  }
  computeSensSpec <- function(subset) {
    result <- computeMaxSprtMetricsPerPeriod(subset) %>%
      mutate(method = subset$method[1],
             analysisId = subset$analysisId[1])
    return(result)
  }
  
  effectSizes <- unique(estimates$effectSize)
  effectSizes <- effectSizes[effectSizes > 1]
  effectSizes <- effectSizes[order(effectSizes)]
  data <- tibble()
  for (effectSize in effectSizes) {
    subset <- estimates[estimates$effectSize == 1 | estimates$effectSize == effectSize, ]
    data <- lapply(split(subset, paste(subset$method, subset$analysisId)), computeSensSpec) %>%
      bind_rows() %>%
      mutate(group = sprintf("Sensitivity when\ntrue effect = %s", effectSize)) %>%
      bind_rows(data)
  }
  data <- data %>%
    filter(.data$group == data$group[1]) %>%
    mutate(value = .data$specificity,
           group = "Specificity") %>%
    select(-.data$sensitivity, -.data$specificity) %>%
    bind_rows(data %>%
                mutate(value = .data$sensitivity) %>%
                select(-.data$sensitivity, -.data$specificity))  
  
  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  yBreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  f <- function(x) {
    -log(1.1 - x)
  }
  cutoff <- expand.grid(group = sprintf("Sensitivity when\ntrue effect = %s", effectSizes), method = unique(data$method)) %>%
    mutate(value = 0.8,
           analysisId = 1)
  
  
  data$analysisId <- as.factor(data$analysisId)
  data$group <- factor(data$group, levels = rev(c("Specificity", sprintf("Sensitivity when\ntrue effect = %s", effectSizes))))
  plot <- ggplot(data, aes(x = .data$periodId, y = f(.data$value), group = .data$analysisId, color = .data$analysisId)) +
    geom_hline(aes(yintercept = f(.data$value)), size = 1, color = rgb(0, 0, 0), linetype = "dashed", data = cutoff) + 
    geom_line(size = 1, alpha = 0.5) +
    geom_point(size = 2, alpha = 0.7) +
    scale_x_continuous("Time (Months)", breaks = 1:max(data$periodId), limits = c(1, max(data$periodId))) +
    scale_y_continuous("Sensitivity / Specificity", limits = f(c(0, 1)), breaks = f(yBreaks), labels = yBreaks) +
    labs(color = "Analysis ID") +
    facet_grid(group ~ method) +
    theme(axis.text.y = theme,
          axis.text.x = theme,
          axis.title.x = theme,
          axis.title.y = element_blank(),
          strip.text.x = theme,
          strip.text.y = theme,
          strip.background = element_blank(),
          legend.text = themeLA,
          legend.title = themeLA,
          legend.position = "right")
  # plot
  return(plot)
}

# estimates <- addTrueEffectSize(estimates, negativeControlOutcome, positiveControlOutcome)
plotSensSpecAcrossMethods <- function(estimates, trueRr = "Overall") {
  if (!"effectSize" %in% colnames(estimates))
    stop("Must add column 'effectSize' to estimates (e.g. using addTrueEffectSize())")
  
  if (trueRr != "Overall") {
    estimates <- estimates[estimates$effectSize == 1 | estimates$effectSize == as.numeric(trueRr), ]
  }
  computeSensSpec <- function(subset) {
    if (subset$effectSize[1] == 1) {
      subset %>%
        group_by(.data$periodId) %>%
        summarize(value = 1 - mean(!is.na(.data$p) & .data$p < 0.05)) %>%
        mutate(group = "Specificity",
               method = subset$method[1],
               analysisId = subset$analysisId[1]) %>%
        return()
    } else {
      subset %>%
        group_by(.data$periodId) %>%
        summarize(value = mean(!is.na(.data$p) & .data$p < 0.05)) %>%
        mutate(group = sprintf("Sensitivity when\ntrue effect = %s", subset$effectSize[1]),
               method = subset$method[1],
               analysisId = subset$analysisId[1]) %>%
        return()
    }
  }
  
  effectSizes <- unique(estimates$effectSize)
  effectSizes <- effectSizes[order(effectSizes)]
  data <- tibble()
  for (effectSize in effectSizes) {
    subset <- estimates %>%
      filter(.data$effectSize == !!effectSize)
    data <- lapply(split(subset, paste(subset$method, subset$analysisId)), computeSensSpec) %>%
      bind_rows() %>%
      bind_rows(data)
  }

  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  yBreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  f <- function(x) {
    -log(1.1 - x)
  }
  pcEffectSizes <- effectSizes[effectSizes > 1]
  cutoff <- expand.grid(group = sprintf("Sensitivity when\ntrue effect = %s", pcEffectSizes), method = unique(data$method)) %>%
    mutate(value = 0.8,
           analysisId = 1)
  
  
  data$analysisId <- as.factor(data$analysisId)
  data$group <- factor(data$group, levels = rev(c("Specificity", sprintf("Sensitivity when\ntrue effect = %s", pcEffectSizes))))
  plot <- ggplot(data, aes(x = .data$periodId, y = f(.data$value), group = .data$analysisId, color = .data$analysisId)) +
    geom_hline(aes(yintercept = f(.data$value)), size = 1, color = rgb(0, 0, 0), linetype = "dashed", data = cutoff) + 
    geom_line(size = 1, alpha = 0.5) +
    geom_point(size = 2, alpha = 0.7) +
    scale_x_continuous("Time (Months)", breaks = 1:max(data$periodId), limits = c(1, max(data$periodId))) +
    scale_y_continuous("Sensitivity / Specificity", limits = f(c(0, 1)), breaks = f(yBreaks), labels = yBreaks) +
    labs(color = "Analysis ID") +
    facet_grid(group ~ method) +
    theme(axis.text.y = theme,
          axis.text.x = theme,
          axis.title.x = theme,
          axis.title.y = element_blank(),
          strip.text.x = theme,
          strip.text.y = theme,
          strip.background = element_blank(),
          legend.text = themeLA,
          legend.title = themeLA,
          legend.position = "right")
  plot
  return(plot)
}

computeTrueRr <- function(estimates, negativeControlOutcome, positiveControlOutcome) {
  estimates <- addTrueEffectSize(estimates, negativeControlOutcome, positiveControlOutcome)
  
  pcs <- estimates %>%
    inner_join(select(positiveControlOutcome, .data$negativeControlId, .data$outcomeId), by = "outcomeId") %>%
    inner_join(select(estimates, periodId = .data$periodId, negativeControlId = .data$outcomeId, negativeControlOutcomes = .data$exposureOutcomes ), by = c("negativeControlId", "periodId")) %>%
    mutate(trueEffectSize = .data$exposureOutcomes / .data$negativeControlOutcomes) %>%
    select(-.data$negativeControlId)
  
  
  # pcs[pcs$periodId == 2, c("effectSize", "trueEffectSize", "exposureOutcomes", "negativeControlOutcomes")]
  
}

# d <- subset[subset$method == "HistoricalComparator" & subset$analysisId == 1, ]
# d <- addTrueEffectSize(d, negativeControlOutcome, positiveControlOutcome)
# d$Group <- as.factor(paste("True effect size =", d$effectSize))
# d <- d[d$exposureOutcomes >= 3, ]
# vaccinationsSubset <- vaccinations
plotLlrs <- function(d, vaccinationsSubset, trueRr = "Overall") {
  d$Signal <- !is.na(d$llr) & !is.na(d$criticalValue) & d$llr >= d$criticalValue
  
  if (trueRr != "Overall") {
    d <- d[d$effectSize == 1 | d$effectSize == as.numeric(trueRr), ]
  }
  
  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  yBreaks <- c(0, 1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100)

  f <- function(y) {
    log10(y + 1) 
  }
  
  multiplier <- f(max(yBreaks)) / max(vaccinationsSubset$vaccinations)
  
  f2 <- function(y) {
    y * multiplier
  }
  
  vaccinationsSubset <- tibble(periodId = 1:max(d$periodId)) %>%
    left_join(vaccinationsSubset, by = "periodId") %>% 
    inner_join(distinct(d, .data$Group), by = character())
  
  maxVaccinations <- max(vaccinationsSubset$vaccinations)
  if (maxVaccinations > 1e6) {
    yBreaks2 <- seq(0, max(vaccinationsSubset$vaccinations), by = 100000)
  } else if (maxVaccinations > 100000) {
    yBreaks2 <- seq(0, max(vaccinationsSubset$vaccinations), by = 20000)
  } else if (maxVaccinations > 20000) {
    yBreaks2 <- seq(0, max(vaccinationsSubset$vaccinations), by = 10000)
  } else {
    yBreaks2 <- seq(0, max(vaccinationsSubset$vaccinations), by = 1000)
  }
  
  yLabels2 <- format(yBreaks2, scientific = FALSE, big.mark = ",")
  
  plot <- ggplot(d, aes(x = periodId, y = f(llr))) +
    geom_area(aes(y = f2(vaccinations)), size = 1, color = rgb(0.83, 0.68, 0.31), fill = rgb(0.83, 0.68, 0.31), alpha = 0.1, data = vaccinationsSubset) +
    geom_line(aes(group = outcomeId), size = 1, color = rgb(0, 0, 0), alpha = 0.5) +
    geom_point(aes(shape = .data$Signal), size = 3, color = rgb(0, 0, 0), fill = rgb(1, 1, 1), alpha = 0.7) +
    scale_shape_manual(name = "Above critical value", values = c(21,16)) +
    scale_x_continuous("Time (Months)", breaks = 1:max(d$periodId), limits = c(1, max(d$periodId))) +
    scale_y_continuous(name = "Log Likelihood ratio", 
                       breaks = f(yBreaks), 
                       labels = yBreaks, 
                       sec.axis = sec_axis(trans = ~ .,
                                           name = "Vaccinations", 
                                           breaks = f2(yBreaks2),
                                           labels = yLabels2)) +
    coord_cartesian(ylim = f(c(min(yBreaks), max(yBreaks)))) +
    facet_grid(. ~ Group) +
    ggplot2::theme(axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   panel.grid.minor = element_blank(),
                   strip.text = theme,
                   strip.background = element_blank(),
                   legend.text = themeLA,
                   legend.title = themeLA,
                   legend.position = "top")
  return(plot)
}

plotSensSpec <- function(estimates) {
  if (!"effectSize" %in% colnames(estimates))
    stop("Must add column 'effectSize' to estimates (e.g. using addTrueEffectSize())")
  
  effectSizes <- unique(estimates$effectSize)
  effectSizes <- effectSizes[effectSizes > 1]
  data <- computeMaxSprtMetricsPerPeriod(estimates) %>%
    mutate(group = "Overall",
           overall = TRUE)
  for (effectSize in effectSizes) {
    subset <- estimates[estimates$effectSize == 1 | estimates$effectSize == effectSize, ]
    data <- computeMaxSprtMetricsPerPeriod(subset) %>%
      mutate(group = sprintf("True effect size = %s", effectSize),
             overall = FALSE) %>%
      bind_rows(data)
  }
  d <- tibble(periodId = rep(data$periodId, 2),
              group = factor(rep(data$group, 2), levels = c(sprintf("True effect size = %s", effectSizes), "Overall")),
              value = c(data$sensitivity, data$specificity),
              metric = c(rep("Sensitivity", nrow(data)), rep("Specificity", nrow(data))),
              overall = rep(data$overall, 2),)
  d <- d %>%
    filter(.data$overall | .data$metric == "Sensitivity")
  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  yBreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  f <- function(x) {
    -log(1.1 - x)
  }
  
  plot <- ggplot(d, aes(x = .data$periodId, y = f(.data$value), group = .data$group, color = .data$group)) +
    geom_line(aes(linetype = .data$overall), size = 1, alpha = 0.5) +
    geom_point(size = 2, alpha = 0.7) +
    scale_x_continuous("Time (Months)", breaks = 1:max(d$periodId), limits = c(1, max(d$periodId))) +
    scale_y_continuous("Sensitivity / Specificity", limits = f(c(0, 1)), breaks = f(yBreaks), labels = yBreaks) +
    # scale_color_manual(values = c("#6a7fd2", "#0ca82e", "#74168e", "#000000")) +
    scale_color_manual(values = c("#781C86", "#83BA70", "#547BD3", "#000000")) +
    facet_grid(metric ~ .) +
    labs(color = "True effect size", linetype = "Overall") +
    theme(axis.text.y = theme,
          axis.text.x = theme,
          axis.title.x = theme,
          axis.title.y = element_blank(),
          strip.text.x = theme,
          strip.text.y = theme,
          strip.background = element_blank(),
          legend.text = themeLA,
          legend.title = themeLA,
          legend.position = "right")
  
  return(plot)
}

plotDbCharacteristics <- function(databaseCharacterization) {
  
  populationCount <- databaseCharacterization %>%
    filter(.data$stratification == "All") 
  observationDuration <- databaseCharacterization %>%
    filter(.data$stratification == "Observation years") %>%
    mutate(stratum = as.numeric(.data$stratum))
  gender <- databaseCharacterization %>%
    filter(.data$stratification == "Gender")
  visitType <- databaseCharacterization %>%
    filter(.data$stratification == "Visit type")
  ageObserved <- databaseCharacterization %>%
    filter(.data$stratification == "Age")
  calendarYearObserved <- databaseCharacterization %>%
    filter(.data$stratification == "Calendar year") %>%
    mutate(stratum = as.numeric(.data$stratum))
  
  barChartTheme <- ggplot2::theme(text = ggplot2::element_text(size = 14),
                                  legend.text = ggplot2::element_text(size = 14),
                                  strip.text = ggplot2::element_text(size = 14),
                                  axis.text.x = ggplot2::element_text(size = 14),
                                  panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(),
                                  panel.grid.major.y = ggplot2::element_blank(),
                                  panel.grid.major.x = ggplot2::element_line(colour = "#AAAAAA"),
                                  axis.ticks = ggplot2::element_blank(),
                                  axis.text.y = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_blank(),
                                  strip.background = ggplot2::element_blank())
  
  pieChartTheme <- ggplot2::theme(text = ggplot2::element_text(size = 14),
                                  legend.text = ggplot2::element_text(size = 14),
                                  strip.text = ggplot2::element_text(size = 14),
                                  panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(),
                                  axis.ticks = ggplot2::element_blank(),
                                  axis.text = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_blank(),
                                  strip.background = ggplot2::element_blank(),
                                  legend.title = ggplot2::element_blank())
  
  firstUppercase <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    return(paste0(" ", x))
  }

  plot2 <- ggplot2::ggplot(observationDuration, ggplot2::aes(x = stratum, y = subjectCount)) +
    ggplot2::geom_bar(stat = "identity", color = rgb(0,0,0, alpha = 0), fill = "#547BD3", alpha = 0.8, width = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_grid(databaseId~., scales = "free_y") +
    ggplot2::xlab("Observation duration (years)") +
    ggplot2::ylab("Persons") +
    barChartTheme
  
  gender <- gender %>%
    inner_join(select(populationCount, .data$databaseId, total = .data$subjectCount), by = "databaseId") %>%
    mutate(fraction = .data$subjectCount / .data$total,
           gender = firstUppercase(.data$stratum))
  plot3 <- ggplot2::ggplot(gender, ggplot2::aes(x = "", y = fraction, color = gender, fill = gender)) +
    ggplot2::geom_bar(stat = "identity", color = rgb(0,0,0, alpha = 0), alpha = 0.7) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_manual(values = c("#547BD3", "#DF4327", "#D3AE4E")) +
    ggplot2::facet_grid(databaseId~.) +
    ggplot2::ylab("Gender") +
    pieChartTheme
  
  visitType <- visitType %>%
    group_by(.data$databaseId) %>%
    summarise(total = sum(.data$subjectCount)) %>%
    inner_join(visitType, by = "databaseId") %>%
    mutate(fraction = .data$subjectCount / .data$total,
           visitType = firstUppercase(.data$stratum)) %>%
    filter(.data$fraction > 0.01)
  
  plot4 <- ggplot2::ggplot(visitType, ggplot2::aes(x = "", y = fraction, color = visitType, fill = visitType)) +
    ggplot2::geom_bar(stat = "identity", color = rgb(0,0,0, alpha = 0), alpha = 0.7) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_manual(values = c("#547BD3", "#DF4327", "#D3AE4E")) +
    ggplot2::facet_grid(databaseId~.) +
    ggplot2::ylab("Visit type") +
    pieChartTheme
  
  ageObserved <- ageObserved %>%
    mutate(startAge = as.numeric(gsub("-.*$", "", .data$stratum)),
           endAge = as.numeric(gsub("^.*-", "", .data$stratum)))
  plot5 <- ggplot2::ggplot(ageObserved, ggplot2::aes(xmin = startAge,  xmax = .data$endAge + 1, ymax = subjectCount, ymin = 0)) +
    ggplot2::geom_rect(stat = "identity", color = rgb(0,0,0, alpha = 0), fill = "#547BD3", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_grid(databaseId~., scales = "free_y") +
    ggplot2::xlab("Observed age (years)") +
    ggplot2::ylab("Persons") +
    ggplot2::xlim(c(0,100)) +
    barChartTheme
  
  populationCount <- populationCount %>%
    mutate(label = sprintf("%.1fM persons", .data$subjectCount / 1e6))
  
  calendarYearObserved <- calendarYearObserved %>%
    inner_join(select(populationCount, .data$databaseId, .data$label), by = "databaseId") %>%
    mutate(databaseId = paste(.data$databaseId, .data$label, sep = "\n"))
  plot6 <- ggplot2::ggplot(calendarYearObserved, ggplot2::aes(x = stratum, y = subjectCount)) +
    ggplot2::geom_bar(stat = "identity", color = rgb(0,0,0, alpha = 0), fill = "#547BD3", alpha = 0.8, width = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_grid(databaseId~., scales = "free_y") +
    ggplot2::xlab("Observed calendar year") +
    ggplot2::ylab("Persons") +
    barChartTheme
  
  grabLegend <- function(a.gplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  legend3 <- grabLegend(plot3)
  legend4 <- grabLegend(plot4)
  
  noStrip <- ggplot2::theme(strip.text = ggplot2::element_blank(),
                            legend.position = "none")
  plot <- gridExtra::grid.arrange(grid::textGrob(""), legend3, grid::textGrob(""), legend4, grid::textGrob(""),
                                  plot2 + noStrip, plot3  + noStrip, plot5  + noStrip, plot4  + noStrip, plot6,
                                  ncol = 5, nrow = 2, widths = c(200, 120, 200, 120, 200),
                                  heights = c(80, 400),
                                  respect = FALSE)
  return(plot)
}


# d <- subset[subset$method == "HistoricalComparator" & subset$analysisId == 1, ]
# d <- addTrueEffectSize(d, negativeControlOutcome, positiveControlOutcome)
# d$Group <- as.factor(paste("True effect size =", d$effectSize))
# d <- d[d$exposureOutcomes >= 3, ]
plotEstimatesAcrossPeriods <- function(d, trueRr = "Overall") {
  d$Signal <- !is.na(d$ci95Lb)  & d$ci95Lb >= 1
  
  if (trueRr != "Overall") {
    d <- d[d$effectSize == 1 | d$effectSize == as.numeric(trueRr), ]
  }
  
  perFacet <- d %>%
    group_by(.data$Group) %>%
    summarize(trueEffectSize = max(.data$effectSize))
  
  theme <- element_text(colour = "#000000", size = 14)
  themeRA <- element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- element_text(colour = "#000000", size = 14, hjust = 0)
  yBreaks <- c(0.25, 0.5, 1, 2, 4, 6, 8, 10)
  plot <- ggplot(d) +
    geom_hline(aes(yintercept = trueEffectSize), size = 1, color = rgb(0.8, 0, 0), linetype = "dashed", data = perFacet) +
    geom_line(aes(x = periodId, y = rr, group = outcomeId), size = 1, color = rgb(0, 0, 0), alpha = 0.5) +
    geom_point(aes(x = periodId, y = rr, shape = .data$Signal), size = 3, color = rgb(0, 0, 0), fill = rgb(1, 1, 1), alpha = 0.7) +
    scale_shape_manual(name = "p < 0.05", values = c(21,16)) +
    scale_x_continuous("Time (Months)", breaks = 1:max(d$periodId), limits = c(1, max(d$periodId))) +
    scale_y_log10("Effect Size Estimate", breaks = yBreaks, labels = yBreaks) +
    coord_cartesian(ylim = c(0.25, 10)) +
    facet_grid(. ~ Group) +
    ggplot2::theme(axis.text.y = themeRA,
                   axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = theme,
                   axis.ticks.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text = theme,
                   strip.background = element_blank(),
                   legend.text = themeLA,
                   legend.title = themeLA,
                   legend.position = "top")
  return(plot)
}
