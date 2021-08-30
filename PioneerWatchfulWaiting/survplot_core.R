ggsurvplot_core <- function(fit, data = NULL, d = NULL, fun = NULL,
                            color = NULL, palette = NULL, linetype = 1,
                            break.x.by = NULL, break.y.by = NULL,  break.time.by = NULL,
                            surv.scale = c("default", "percent"), xscale = 1,
                            conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                            conf.int.alpha = 0.3,
                            censor = TRUE, censor.shape = "+", censor.size = 4.5,
                            pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                            test.for.trend = FALSE,
                            pval.method = FALSE, pval.method.size = pval.size, pval.method.coord = c(NULL, NULL),
                            log.rank.weights = c("survdiff", "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1"),
                            title = NULL,  xlab = "Time", ylab = "Survival probability",
                            xlim = NULL, ylim = NULL, axes.offset = TRUE,
                            legend = c("top", "bottom", "left", "right", "none"),
                            legend.title = "Strata", legend.labs = NULL,
                            fontsize = 4.5, font.family = "",
                            tables.height = 0.25, tables.y.text = TRUE, tables.col = "black",
                            tables.y.text.col = TRUE,
                            risk.table = FALSE, risk.table.pos = c("out", "in"), risk.table.title = NULL,
                            risk.table.col = tables.col, risk.table.fontsize = fontsize,
                            risk.table.y.text = tables.y.text,
                            risk.table.y.text.col = tables.y.text.col,
                            risk.table.height = tables.height, surv.plot.height = 0.75,
                            ncensor.plot.height = tables.height, cumevents.height = tables.height,
                            cumcensor.height = tables.height,
                            ncensor.plot = FALSE,
                            ncensor.plot.title = NULL,
                            cumevents = FALSE, cumevents.col = tables.col, cumevents.title = NULL,
                            cumevents.y.text = tables.y.text, cumevents.y.text.col = tables.y.text.col,
                            cumcensor = FALSE, cumcensor.col = tables.col, cumcensor.title = NULL,
                            cumcensor.y.text = tables.y.text, cumcensor.y.text.col = tables.y.text.col,
                            surv.median.line = c("none", "hv", "h", "v"),
                            ggtheme = theme_survminer(),
                            tables.theme = ggtheme,
                            cmap = NULL,
                            ...
){
  d <- fit
 
  # if(!inherits(fit, "survfit"))
  #   stop("Can't handle an object of class ", class(fit))
  surv.median.line <- match.arg(surv.median.line)
  stopifnot(log.rank.weights %in% c("survdiff", "1", "n", "sqrtN", "S1", "S2","FH_p=1_q=1"))
  log.rank.weights <- match.arg(log.rank.weights)
  
  # Make sure that user can do either ncensor.plot or cumcensor
  # But not both
  if(ncensor.plot & cumcensor){
    warning("Both ncensor.plot and cumsensor are TRUE.",
            "In this case, we consider only cumcensor.", call. = FALSE)
    ncensor.plot <- FALSE
  }
  if(cumcensor) ncensor.plot.height <- cumcensor.height
  if(is.null(ncensor.plot.title))
    ncensor.plot.title <- "Number of censoring"
  if(is.null(cumcensor.title))
    cumcensor.title <- "Cumulative number of censoring"
  if(is.null(cumevents.title))
    cumevents.title <- "Cumulative number of events"
  
  # risk.table argument
  
#==============================================================
#==============================================================
  risk.table.pos <- match.arg(risk.table.pos)
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type
  extra.params <- list(...)
  
  # Axes offset
  .expand <- ggplot2::waiver()
  if(!axes.offset)
    .expand <- c(0, 0)
  
  
  # Data
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # data used to compute survfit
  # data <- .get_data(fit, data = data, complain = FALSE)
  # Data for survival plot
  # d <- surv_summary(fit, data = data)
  if(!is.null(fit$start.time)) d <- subset(d, d$time >= fit$start.time )
  
  # Axis limits
  xmin <- ifelse(.is_cloglog(fun), min(c(1, d$time)), 0)
  if(!is.null(fit$start.time)) xmin <- fit$start.time
  xmax <- .get_default_breaks(d$time, .log = .is_cloglog(fun)) %>% max()
  if(is.null(xlim)) xlim <- c(xmin, xmax)
  
  # Main survival curves
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  legend.labs <- sort(legend.labs)
  palette <- c()
  for(plot in legend.labs){
    palette <- c(palette, cmap[[plot]]) 
  }
  
  p <- survminer::ggsurvplot_df(d, fun = fun,
                     color = color, palette = cb_palette, linetype = linetype,
                     break.x.by = break.x.by, break.time.by = break.time.by, break.y.by = break.y.by,
                     surv.scale = surv.scale, xscale = xscale,
                     conf.int = conf.int, conf.int.fill = conf.int.fill, conf.int.style = conf.int.style,
                     conf.int.alpha = conf.int.alpha,
                     censor = censor, censor.shape = censor.shape, censor.size = censor.size,
                     title = title,  xlab = xlab, ylab = ylab,
                     xlim = xlim, ylim = ylim, axes.offset = axes.offset,
                     legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                     ggtheme = ggtheme, ...)
  
  
  
  
  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  color <- surv.color <- pms$color <-cb_palette


  res <- list(plot = p)

  # Extract strata colors used in survival curves
  # Will be used to color the y.text of risk table and cumevents table
  if(risk.table | cumevents | cumcensor | ncensor.plot){
    # scurve_cols <- .extract_ggplot_colors (p, grp.levels = pms$legend.labs)
    scurve_cols <- cmap[legend.labs]
  }


  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  surv.color <- pms$color

  pms$fit <- fit
  pms$risk.table.type <- risk.table.type
  pms$risk.table.title <- risk.table.title
  pms$cumevents.title <- cumevents.title
  pms$cumcensor.title <- cumcensor.title
  pms$fontsize <- fontsize
  pms$ggtheme <- ggtheme
  pms$ylab <- pms$legend.title
  pms$tables.theme <- tables.theme
  pms$y.text <- tables.y.text
  pms$color <- tables.col
  pms$font.family <- font.family
  pms$axes.offset <- axes.offset


  # Add risk table
  if(risk.table){
    if(risk.table.pos == "in") risk.table.col = surv.color
    pms$color <- risk.table.col
    pms$title <- risk.table.title
    pms$y.text <- risk.table.y.text
    pms$y.text.col <- risk.table.y.text.col
    pms$fontsize <- risk.table.fontsize
    pms$survtable <- "risk.table"
    # color risk.table ticks by strata
    if(risk.table.y.text.col) pms$y.text.col <- scurve_cols
    res$table <- risktable <- do.call(ggsurvtable_custom, pms)
  }

  # Add the cumulative number of events

  if(cumevents){
    pms$color <- cumevents.col
    pms$title <- cumevents.title
    pms$y.text <- cumevents.y.text
    if(cumevents.y.text.col) pms$y.text.col <- scurve_cols
    pms$fontsize <- fontsize
    pms$survtable <- "cumevents"
    res$cumevents <- do.call(ggsurvtable, pms)
  }

  # Add ncensor.plot or cumcensor plot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(ncensor.plot){
    ncensor_plot <- ggplot(d, aes_string("time", "n.censor")) +
      ggpubr::geom_exec(geom_bar, d, color = surv.color, fill = surv.color,
                        stat = "identity", position = "dodge")+
      coord_cartesian(xlim = xlim)+
      scale_y_continuous(breaks = sort(unique(d$n.censor))) +
      ggtheme

    ncensor_plot <- ggpubr::ggpar(ncensor_plot, palette = pms$palette)
    ncensor_plot <- ncensor_plot + ggplot2::labs(color = pms$legend.title, fill = pms$legend.title,
                                                 x = xlab, y = "n.censor", title = ncensor.plot.title)

    # For backward compatibility
    ncensor_plot <-  .set_general_gpar(ncensor_plot,  ...) # general graphical parameters
    ncensor_plot <- .set_ncensorplot_gpar(ncensor_plot,  ...) # specific graphical params
    ncensor_plot <- ncensor_plot + tables.theme

    if(!pms$xlog) ncensor_plot <- ncensor_plot + scale_x_continuous(breaks = pms$time.breaks,
                                                                    labels = pms$xticklabels, expand = .expand)
    else ncensor_plot <- ncensor_plot + ggplot2::scale_x_continuous(breaks = pms$time.breaks, trans = "log10", labels = pms$xticklabels)

  }
  else if(cumcensor){
    pms$color <- cumcensor.col
    pms$title <- cumcensor.title
    if(cumcensor.y.text.col) pms$y.text.col <- scurve_cols
    #pms$y.text.col <- cumcensor.y.text.col
    pms$fontsize <- fontsize
    pms$survtable <- "cumcensor"
    ncensor_plot  <- do.call(ggsurvtable, pms)
  }
  if(ncensor.plot | cumcensor)
    res$ncensor.plot <- ncensor_plot


  # Defining attributs for ggsurvplot
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  heights <- list(
    plot =  surv.plot.height,
    table =  ifelse(risk.table, risk.table.height, 0),
    ncensor.plot = ifelse(ncensor.plot | cumcensor, ncensor.plot.height, 0),
    cumevents = ifelse(cumevents, cumevents.height, 0)
  )
  y.text <- list(
    table =  risk.table.y.text,
    cumevents = cumevents.y.text,
    cumcensor = cumcensor.y.text
  )
  y.text.col <- list(
    table =  risk.table.y.text.col,
    cumevents = cumevents.y.text.col,
    cumcensor = cumcensor.y.text.col
  )


  class(res) <- c("ggsurvplot", "ggsurv", "list")
  attr(res, "heights") <- heights
  attr(res, "y.text") <- y.text
  attr(res, "y.text.col") <- y.text.col
  attr(res, "legend.position") <- legend
  attr(res, "legend.labs") <- legend.labs
  attr(res, "cumcensor") <- cumcensor
  attr(res, "risk.table.pos") <- risk.table.pos
  attr(res, "axes.offset") <- axes.offset
  res
}


# Parse risk.table argument
#%%%%%%%%%%%%%%%%%%%%%%%
# risk.table a logical value (TRUE/FALSE) or a string ("absolute", "percentage", "abs_pct")
.parse_risk_table_arg <- function(risk.table){
  res <- list(display = risk.table, type = "absolute")
  if(inherits(risk.table, "character") ){
    if(risk.table %in% c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents") )
      res <- list(display = TRUE, type = risk.table)
    else stop("Allowed values for risk.table are: TRUE, FALSE, 'absolute', 'percentage', 'nrisk_cumcensor', 'nrisk_cumevents' ")
  }
  res
}


# Check if fun is cloglog
.is_cloglog <- function(fun){
  res <- FALSE
  if(is.character(fun)){
    res <- fun == "cloglog"
  }
  res
}

