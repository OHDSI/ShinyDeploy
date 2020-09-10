library(dplyr)
library(plotly)
library(RColorBrewer)

analysisPath <- "./data"
mapOutcomes <- readRDS(
	file.path(
		analysisPath,
		"map_outcomes.rds"
	)
)

pathToHtml <- "./html"

mapOutcomes <- readRDS(
	file.path(
		analysisPath,
		"map_outcomes.rds"
	)
)

mapExposures <- readRDS(
	file.path(
		analysisPath,
		"map_exposures.rds"
	)
)

analyses <- readRDS(
	file.path(
		analysisPath,
		"analyses.rds"
	)
)

incidence <-
	readRDS(
		file.path(
			analysisPath,
			"incidence.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes, 
		by = c(
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes, 
		by = c(
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"treatmentId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatmentId") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename("comparator" = "exposure_name")

predictionPerformance <-
	readRDS(
		file.path(
			analysisPath,
			"predictionPerformance.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"treatmentId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatmentId") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallAbsoluteResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallAbsoluteResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallRelativeResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallRelativeResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes, 
		by = c(
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)


mappedOverallCasesResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallCasesResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures, 
		by = c(
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

databaseOptions <- unique(
	mappedOverallAbsoluteResults$database
)

analysisTypeOptions <- unique(
	mappedOverallAbsoluteResults$analysisType
)

stratOptions <- unique(
	mappedOverallAbsoluteResults$stratOutcome
)


getResults <- function(treat, comp, strat, est, db, anal,
					   mappedOverallRelativeResults,
					   mappedOverallAbsoluteResults,
					   mappedOverallCasesResults) {
	
	result <- list()
	
	result$relative <-
		mappedOverallRelativeResults %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)
	
	result$absolute <-
		mappedOverallAbsoluteResults %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)
	
	result$cases <-
		mappedOverallCasesResults %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)
	
	return(result)
	
	
}


getIncidence <- function(treat,
						 comp,
						 strat,
						 est,
						 db,
						 anal,
						 incidence) {
	incidence %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		) %>%
		return()
}


getPredictionPerformance <- function(treat,
									 comp,
									 strat,
									 coh,
									 db,
									 predictionPerformance) {
	
	predictionPerformance %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$cohort %in% coh & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db
		) %>%
		return()
	
}

getBalance <- function(treat,
					   comp,
					   strat,
					   est,
					   db,
					   anal,
					   analyses,
					   mapExposures,
					   mapOutcomes,
					   analysisPath) {
	
	res <- analyses %>%
		dplyr::left_join(
			mapExposures,
			by = c("treatment_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"treatment_name" = "exposure_name"
		) %>%
		dplyr::left_join(
			mapExposures,
			by = c("comparator_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"comparator_name" = "exposure_name"
		) %>%
		dplyr::filter(
			.$treatment_name == treat,
			.$comparator_name == comp,
			.$database == db,
			.$analysis_type == anal
		)
	
	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	readRDS(
		file.path(
			analysisPath,
			paste0(
				paste(
					"balance",
					res$analysis_id,
					treatmentId,
					comparatorId,
					stratOutcomeId,
					estOutcomeId,
					sep = "_"
				),
				".rds"
			)
		)
	)
}




getPsDensity <- function(treat,
						 comp,
						 strat,
						 est,
						 db,
						 anal,
						 analyses,
						 mapExposures,
						 mapOutcomes,
						 analysisPath) {
	
	res <- analyses %>%
		dplyr::left_join(
			mapExposures,
			by = c("treatment_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"treatment_name" = "exposure_name"
		) %>%
		dplyr::left_join(
			mapExposures,
			by = c("comparator_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"comparator_name" = "exposure_name"
		) %>%
		dplyr::filter(
			.$treatment_name == treat,
			.$comparator_name == comp,
			.$database == db,
			.$analysis_type == anal
		)
	
	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	readRDS(
		file.path(
			analysisPath,
			paste0(
				paste(
					"psDensity",
					res$analysis_id,
					treatmentId,
					comparatorId,
					stratOutcomeId,
					estOutcomeId,
					sep = "_"
				),
				".rds"
			)
		)
	)
}


getAuc <- function(treat,
				   comp,
				   strat,
				   db,
				   anal,
				   predictionPopulation,
				   analyses,
				   mapExposures,
				   mapOutcomes,
				   analysisPath) {
	
	res <- analyses %>%
		dplyr::left_join(
			mapExposures,
			by = c("treatment_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"treatment_name" = "exposure_name"
		) %>%
		dplyr::left_join(
			mapExposures,
			by = c("comparator_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"comparator_name" = "exposure_name"
		) %>%
		dplyr::filter(
			.$treatment_name == treat,
			.$comparator_name == comp,
			.$database == db,
			.$analysis_type == anal
		)
	
	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	pathList <- file.path(
		analysisPath,
		paste(
			paste(
				"auc",
				predictionPopulation,
				db,
				anal,
				treatmentId,
				comparatorId,
				stratOutcomeId,
				sep = "_"
			),
			"rds",
			sep = "."
		)
	)
	
	
	aucResultList <- lapply(
		pathList, 
		readRDS
	)
	
	names(aucResultList) <- predictionPopulation
	
	aucResultList %>%
		dplyr::bind_rows(
			.id = "cohort"
		) %>%
		return()
}


getCalibration <- function(
	treat,
	comp,
	strat,
	db,
	anal,
	predictionPopulation,
	analyses,
	mapExposures,
	mapOutcomes,
	analysisPath
)
{
	
	res <- analyses %>%
		dplyr::left_join(
			mapExposures,
			by = c("treatment_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"treatment_name" = "exposure_name"
		) %>%
		dplyr::left_join(
			mapExposures,
			by = c("comparator_id" = "exposure_id")
		) %>%
		dplyr::rename(
			"comparator_name" = "exposure_name"
		) %>%
		dplyr::filter(
			.$treatment_name == treat,
			.$comparator_name == comp,
			.$database == db,
			.$analysis_type == anal
		)
	
	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()
	
	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()
	
	pathList <- file.path(
		analysisPath,
		paste(
			paste(
				"calibration",
				predictionPopulation,
				db,
				anal,
				treatmentId,
				comparatorId,
				stratOutcomeId,
				sep = "_"
			),
			"rds",
			sep = "."
		)
	)
	
	
	calibrationResultList <- lapply(
		pathList, 
		readRDS
	)
	
	names(calibrationResultList) <- predictionPopulation
	
	calibrationResultList %>%
		dplyr::bind_rows(
			.id = "cohort"
		) %>%
		return()
}



combinedPlot <- function(
	cases,
	relative,
	absolute,
	treatment,
	comparator
) 
{
	
	
	customColors <- c(
		"#0099FF",
		"#009933",
		"#CC0000",
		"#FF9933",
		"#663399",
		"#CC9966"
	)
	
	nOutcomes <- length(
		unique(
			cases$estOutcome
		)
	)
	
	if (nOutcomes > 5) {
		stop("No more than 5 outcomes can be plotted at the same time")
	}
	
	m <- 0
	
	if ( nOutcomes == 2) {
		m <- c(-.15, .15)
	} else if (nOutcomes == 3) {
		m <- c(-.15, 0, .15)
	} else if (nOutcomes == 4) {
		m <- c(-.15, -.05, .05, .15)
	} else if (nOutcomes == 5) {
		m <- c(-.15, -.075, 0, .075, .15)
	}
	
	relative <- relative %>%
		dplyr::mutate(
			estOutcome = factor(
				.$estOutcome,
				levels = sort(
					unique(
						.$estOutcome
					)
				)
			)
		)
	
	absolute <- absolute %>%
		dplyr::mutate(
			estOutcome = factor(
				.$estOutcome,
				levels = sort(
					unique(
						.$estOutcome
					)
				)
			)
		)
	
	quickMap <- data.frame(
		estOutcome = levels(relative$estOutcome),
		m = m
	)
	
	cases <-
		reshape::melt(
			cases,
			id.vars = c(
				"riskStratum",
				"database",
				"estOutcome"
			),
			measure.vars = c(
				"casesComparator",
				"casesTreatment"
			)
		) %>%
		dplyr::mutate(
			variable = ifelse(
				variable == "casesComparator",
				comparator,
				treatment
			),
			g = paste(
				estOutcome,
				variable,
				sep = "/"
			),
			value = 100*.$value,
			riskStratum = as.numeric(
				as.factor(
					.$riskStratum
				)
			)
		)
	
	
	p1   <-
		plot_ly(
			data = cases,
			x = ~riskStratum,
			y = ~value,
			color = ~g,
			colors = "Paired",
			type = 'bar',
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				cases$estOutcome,
				"<br><b>Database:</b>",
				cases$database,
				"<br><b>Exposure:</b>",
				cases$variable,
				"<br><b>Event rate:</b>",
				paste0(
					round(cases$value, 2),
					"%"
				)
			),
			legendgroup = ~g
		) %>%
		plotly::layout(
			yaxis = list(
				title = 'Observed events (%)',
				autorange = "reversed"
			),
			xaxis = list(
				title = "Risk stratum"
			),
			barmode = 'group'
		)
	
	relative <-
		relative %>%
		dplyr::left_join(quickMap) %>%
		dplyr::mutate(
			risk = as.numeric(
				as.factor(
					.$riskStratum
				)
			)
		)
	
	p2 <-
		relative %>%
		dplyr::group_by(.$estOutcome) %>%
		plotly::plot_ly(
			mode = "markers",
			x = ~risk + m,
			y = ~estimate,
			color = ~estOutcome,
			colors = customColors[1:nOutcomes],
			type = "scatter",
			error_y = list(
				type = "data",
				array = relative$upper - relative$estimate,
				arrayminus = relative$estimate - relative$lower
			),
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				relative$estOutcome,
				"<br><b>Database:</b>",
				relative$database,
				"<br><b>HR:</b>",
				paste0(
					round(relative$estimate, 2),
					" (",
					paste(
						round(relative$lower, 2),
						round(relative$upper, 2),
						sep = ", "
					),
					")"
				)
			),
			legendgroup = ~estOutcome
		) %>%
		plotly::layout(
			yaxis = list(
				title = "Hazard ratio"
			),
			xaxis = list(
				title = "Risk stratum",
				tickformat = ',d'
			)
		) %>%
		plotly::layout(
			shapes = hline(1)
		)
	
	absolute <-
		absolute %>%
		dplyr::left_join(quickMap) %>%
		dplyr::mutate(
			estimate = 100*.$estimate,
			lower = 100*.$lower,
			upper = 100*.$upper,
			risk = as.numeric(
				as.factor(
					.$riskStratum
				)
			)
		)
	
	p3 <-
		absolute %>%
		plotly::plot_ly(
			mode = "markers",
			x = ~risk + m,
			y = ~estimate,
			color = ~estOutcome,
			colors = customColors[1:nOutcomes],
			type = "scatter",
			error_y = list(
				type = "data",
				array = absolute$upper - absolute$estimate,
				arrayminus = absolute$estimate - absolute$lower
			),
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				absolute$estOutcome,
				"<br><b>Database:</b>",
				absolute$database,
				"<br><b>Absolute difference:</b>",
				paste0(
					round(absolute$estimate, 2),
					" (",
					paste(
						round(absolute$lower, 2),
						round(absolute$upper, 2),
						sep = ", "
					),
					")"
				)
			),
			legendgroup = ~estOutcome,
			showlegend = FALSE
		) %>%
		plotly::layout(
			yaxis = list(
				title = "Absolute risk reduction (%)"
			),
			xaxis = list(
				title = "Risk stratum",
				tickformat = ',d'
			)
		)
	
	plotly::subplot(p1, p2, p3, shareX = TRUE, nrows = 3, titleY = T)
	
	
}



hline <- function(y = 0, color = "black") {
	list(
		type = "line",
		x0 = 0,
		x1 = 1,
		xref = "paper",
		y0 = y,
		y1 = y,
		line = list(
			color = color,
			dash = "dash"
		)
	)
}



addInfo <- function(item, infoId) {
	infoTag <- tags$small(
		class = "badge pull-right action-button",
		style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
		type = "button",
		id = infoId,
		"i"
	)
	
	item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
	
	return(item)
}