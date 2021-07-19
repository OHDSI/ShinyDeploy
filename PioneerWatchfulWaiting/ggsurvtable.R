cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggsurvtable_custom <- function (fit, data = NULL, survtable = c("cumevents",  "cumcensor", "risk.table"),
                         risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"),
                         title = NULL, risk.table.title = NULL, cumevents.title = title, cumcensor.title = title,
                         color = "black", palette = cb_palette, break.time.by = NULL,  xlim = NULL,
                         xscale = 1, xlab = "Time", ylab = "Strata",
                         xlog = FALSE, legend = "top",
                         legend.title = "Strata", legend.labs = NULL,
                         y.text = TRUE, y.text.col = TRUE,
                         fontsize = 4.5, font.family = "",
                         axes.offset = TRUE,
                         ggtheme = theme_survminer(),
                         tables.theme = ggtheme, ...)
{
  
  if(is.data.frame(fit)){}
  else if(.is_list(fit)){
    if(!all(c("time", "table") %in% names(fit)))
      stop("fit should contain the following component: time and table")
  }
  else if(!.is_survfit(fit))
    stop("Can't handle an object of class ", class(fit))
  
  # Define time axis breaks
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  xmin <- ifelse(xlog, min(c(1, fit$time)), 0)
  if(is.null(xlim)) xlim <- c(xmin, max(fit$time))
  times <- .get_default_breaks(fit$time, .log = xlog)
  if(!is.null(break.time.by) &!xlog) times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)
  
  
  
  # Surv summary at specific time points
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # if(.is_survfit(fit)){
  #   data <- .get_data(fit, data = data)
  #   survsummary <- .get_timepoints_survsummary(fit, data, times)
  # }
  # else if(.is_list(fit)){
  #   survsummary <- fit$table
  # }
  else if(inherits(fit, "data.frame")){
    survsummary <- as.data.frame(.get_timepoints_survsummary(fit, data, times))
  }
  
  opts <- list(
    survsummary = survsummary, times = times,
    survtable = survtable, risk.table.type = risk.table.type,  color = color, palette = cb_palette,
    xlim = xlim, xscale = xscale,
    title = title, xlab = xlab, ylab = ylab, xlog = xlog,
    legend = legend, legend.title = legend.title, legend.labs = legend.labs,
    y.text = y.text, y.text.col = y.text.col,
    fontsize = fontsize, font.family = font.family,
    axes.offset = axes.offset,
    ggtheme = ggtheme, tables.theme = tables.theme,...)
  
  res <- list()
  time <- strata <- label <- n.event <- cum.n.event <- NULL
  
  # Ploting the cumulative number of events table
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  if("cumevents" %in% survtable){
    opts$survtable = "cumevents"
    opts$title <- ifelse(is.null(cumevents.title),
                         "Cumulative number of events", cumevents.title)
    res$cumevents <- do.call(.plot_survtable, opts)
    
  }
  
  if("cumcensor" %in% survtable){
    opts$survtable = "cumcensor"
    opts$title <- ifelse(is.null(cumcensor.title),
                         "Cumulative number of events", cumcensor.title)
    res$cumcensor <- do.call(.plot_survtable, opts)
    
  }
  if("risk.table" %in% survtable){
    opts$survtable = "risk.table"
    if(is.null(risk.table.title)) opts$title <- NULL
    else opts$title <- risk.table.title
    res$risk.table <- do.call(.plot_survtable, opts)
  }
  
  
  if(length(res) == 1) res <- res[[1]]
  res
}








# Helper function to plot a specific survival table
.plot_survtable <- function (survsummary, times, survtable = c("cumevents", "risk.table", "cumcensor"),
                             risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"),
                             color = "black", palette = cb_palette, xlim = NULL,
                             xscale = 1,
                             title = NULL, xlab = "Time", ylab = "Strata",
                             xlog = FALSE, legend = "top",
                             legend.title = "Strata", legend.labs = NULL,
                             y.text = TRUE, y.text.col = TRUE, fontsize = 4.5,
                             font.family = "",
                             axes.offset = TRUE,
                             ggtheme = theme_survminer(), tables.theme = ggtheme,
                             ...)
{
  
  survtable <- match.arg(survtable)
  risk.table.type <- match.arg(risk.table.type)
  
  # Defining plot title
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is.null(title)){
    
    if(survtable == "risk.table"){
      risk.table.type <- match.arg(risk.table.type)
      title <- switch(risk.table.type,
                      absolute = "Number at risk",
                      percentage = "Percentage at risk",
                      abs_pct = "Number at risk: n (%)",
                      nrisk_cumcensor = "Number at risk (number censored)",
                      nrisk_cumevents = "Number at risk (number of events)",
                      "Number at risk")
      
    }
    else
      title <- switch(survtable,
                      cumevents = "Cumulative number of events",
                      cumcensor = "Number of censored subjects"
      )
  }
  
  # Legend labels
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is.null(color))
    color <- .strata.var <- "strata"
  else if(color %in% colnames(survsummary))
    .strata.var <- color
  else
    .strata.var <- "strata"
  
  # Number of strata and strata names
  .strata <- survsummary[, .strata.var]
  strata_names <- .levels(.strata)
  n.strata <- length(strata_names)
  
  # Check legend labels and title
  if(!is.null(legend.labs)){
    if(n.strata != length(legend.labs))
      warning("The length of legend.labs should be ", n.strata )
    else survsummary$strata <- factor(survsummary$strata, labels = legend.labs)
  }
  else if(is.null(legend.labs))
    legend.labs <- strata_names
  
  
  
  # Adjust table y axis tick labels in case of long strata
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  yticklabs <- rev(levels(survsummary$strata))
  n_strata <- length(levels(survsummary$strata))
  if(!y.text) yticklabs <- rep("\\-", n_strata)
  
  time <- strata <- label <- n.event <- cum.n.event  <- cum.n.censor<- NULL
  
  # Ploting the cumulative number of events table
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(survtable == "cumevents"){
    mapping <- ggplot2::aes(x = time, y = rev(strata),
                   label = cum.n.event, shape = rev(strata))
  }
  else if (survtable == "cumcensor"){
    mapping <- ggplot2::aes(x = time, y = rev(strata),
                   label = cum.n.censor, shape = rev(strata))
    
  }
  else if (survtable == "risk.table"){
    # risk table labels depending on the type argument
    pct.risk <- abs_pct.risk <- n.risk <- NULL
    llabels <- switch(risk.table.type,
                      percentage = round(survsummary$n.risk*100/survsummary$strata_size),
                      abs_pct = paste0(survsummary$n.risk, " (", survsummary$pct.risk, ")"),
                      nrisk_cumcensor = paste0(survsummary$n.risk, " (", survsummary$cum.n.censor, ")"),
                      nrisk_cumevents = paste0(survsummary$n.risk, " (", survsummary$cum.n.event, ")"),
                      survsummary$n.risk
    )
    survsummary$llabels <- llabels
    mapping <- ggplot2::aes(x = time, y = rev(strata),
                   label = llabels, shape = rev(strata))
    
  }
  
  
  # Plotting survival table
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .expand <- ggplot2::waiver()
  # Tables labels Offset from origing
  if(!axes.offset){
    .expand <- c(0,0)
    offset <- max(xlim)/30
    survsummary <- survsummary %>%
      dplyr::mutate(time = ifelse(time == 0, offset, time))
  }
  
  p <- ggplot2::ggplot(survsummary, mapping) +
    ggplot2::scale_shape_manual(values = 1:length(levels(survsummary$strata)))+
    ggpubr::geom_exec(ggplot2::geom_text, data = survsummary, size = fontsize, color = color, family = font.family) +
    ggtheme +
    ggplot2::scale_y_discrete(breaks = as.character(levels(survsummary$strata)),labels = yticklabs ) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::labs(title = title, x = xlab, y = ylab, color = legend.title, shape = legend.title)
  
  if (survtable == "risk.table")
    p <- .set_risktable_gpar(p, ...) # For backward compatibility
  
  p <- ggpubr::ggpar(p, legend = legend, palette = palette,...)
  
  # Customize axis ticks
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  xticklabels <- .format_xticklabels(labels = times, xscale = xscale)
  if(!xlog) p <- p + ggplot2::scale_x_continuous(breaks = times, labels = xticklabels, expand = .expand)
  else p <- p + ggplot2::scale_x_continuous(breaks = times,
                                            trans = "log10", labels = xticklabels)
  
  p <- p + tables.theme
  
  if(!y.text) {
    p <- .set_large_dash_as_ytext(p)
  }
  
  # Color table tick labels by strata
  if(is.logical(y.text.col) & y.text.col[1] == TRUE){
    cols <- .extract_ggplot_colors(p, grp.levels = legend.labs)
    p <- p + ggplot2::theme(axis.text.y = ggtext::element_markdown(colour = rev(cols)))
  }
  else if(is.character(y.text.col))
    p <- p + ggplot2::theme(axis.text.y = ggtext::element_markdown(colour = rev(y.text.col)))
  
  p
  
}



# For backward compatibility
# Specific graphical params to risk.table
.set_risktable_gpar <- function(p,  ...){
  extra.params <- list(...)
  ggpubr:::.labs(p,
                 font.main = extra.params$font.risk.table.title,
                 font.x = extra.params$font.risk.table.x,
                 font.y = extra.params$font.risk.table.y,
                 submain = extra.params$risk.table.subtitle,
                 caption = extra.params$risk.table.caption,
                 font.submain = extra.params$font.risk.table.subtitle,
                 font.caption = extra.params$font.risk.table.caption)
}
