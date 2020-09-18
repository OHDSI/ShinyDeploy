library(dplyr)
shiny::shinyServer(
	function(
		input, 
		output, 
		session
	)
	{
		
		
		shiny::observe(
			{
				stratificationOutcome <- input$stratOutcome
				filteredEstimationOutcomes <- mappedOverallRelativeResults %>%
					dplyr::filter(
						stratOutcome == stratificationOutcome
					) %>%
					dplyr::select(
						estOutcome
					)
				
				
				shiny::updateSelectInput(
					session = session,
					inputId = "estOutcome",
					choices = unique(filteredEstimationOutcomes)
				)
			}
		)
		
		resultSubset <- shiny::reactive(
			{
				
				results <- getResults(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					est = input$estOutcome,
					anal = input$analysis, 
					db = input$database,
					mappedOverallRelativeResults = mappedOverallRelativeResults,
					mappedOverallAbsoluteResults = mappedOverallAbsoluteResults,
					mappedOverallCasesResults = mappedOverallCasesResults
				)
				
				return(results)
				
			}
		)
		
		incidenceSubset <- shiny::reactive(
			{
				
				res <- getIncidence(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					est = input$estOutcome,
					anal = input$analysis,
					db = input$database,
					incidence = incidence
				)
				
				return(res)
				
			}
		)
		
		
		
		output$mainTableIncidence <- DT::renderDataTable(
			{
				
				res <- incidenceSubset()
				
				treatment <- res$treatment[1]
				comparator <- res$comparator[1]
				outcome <- res$stratOutcome[1]
				
				table <- res %>%
					dplyr::select(
						estOutcome,
						riskStratum,
						treatmentPersons,
						treatmentDays,
						treatmentOutcomes,
						comparatorPersons,
						comparatorDays,
						comparatorOutcomes
					) %>%
					dplyr::mutate(
						treatmentDays = treatmentDays / 365.25,
						comparatorDays = comparatorDays / 365.25
					)
				
				table <-
					DT::datatable(
						table,
						colnames = c(
							"Outcome",
							"Risk stratum",
							"Treatment subjects",
							"Treatment years",
							"Treatment events",
							"Comparator subjects",
							"Comparator years",
							"Comparator events"
						),
						caption = htmltools::tags$caption(
							style = "caption-side: top; text-align: left;",
							"Table 1: Number of subjects, follow-up time (in years), event rates in the treatment",
							htmltools::em(
								paste0(
									"(",
									treatment,
									")"
								)
							),
							"and the comparator",
							htmltools::em(
								paste0(
									"(",
									comparator,
									")"
								)
							),
							"groups within strata of predicted risk",
							htmltools::em(
								paste0(
									"(",
									outcome,
									")"
								)
							)
						)
						
					) %>%
					DT::formatCurrency(
						columns =  "treatmentPersons",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					) %>%
					DT::formatCurrency(
						"comparatorPersons",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					) %>%
					DT::formatCurrency(
						"treatmentDays",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					) %>%
					DT::formatCurrency(
						"comparatorDays",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					) %>%
					DT::formatCurrency(
						"treatmentOutcomes",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					) %>%
					DT::formatCurrency(
						"comparatorOutcomes",
						currency = "",
						interval = 3,
						mark = ",",
						digits = 0
					)
				
				return(table)
				
			}
		)
		
		output$mainTableRelative <- DT::renderDataTable(
			{
				
				res <- resultSubset()
				
				treatment <- res$relative$treatment[1]
				comparator <- res$relative$comparator[1]
				outcome <- res$relative$stratOutcome[1]
				
				table <- res$relative %>%
					dplyr::mutate(
						combined = paste(
							round(estimate, 2),
							paste0(
								"(",
								round(lower, 2),
								", ",
								round(upper, 2),
								")"
							)
						)
					) %>%
					dplyr::rename(
						Outcome = estOutcome
					) %>%
					dplyr::select(
						Outcome,
						riskStratum,
						combined
					) %>%
					tidyr::spread(riskStratum, combined) %>%
					DT::datatable(
						caption = htmltools::tags$caption(
							style = "caption-side: top; text-align: left;",
							"Table 2: Hazard ratios comparing treatment",
							htmltools::em(
								paste0(
									"(",
									treatment,
									")"
								)
							),
							"to comparator",
							htmltools::em(
								paste0(
									"(",
									comparator,
									")"
								)
							),
							"within strata of predicted risk",
							htmltools::em(
								paste0(
									"(",
									outcome,
									")"
								)
							)
							
						)
					)
				
				return(table)
				
			}
		)
		
		output$mainTableAbsolute <- DT::renderDataTable(
			{
				
				res <- resultSubset()
				
				treatment <- res$absolute$treatment[1]
				comparator <- res$absolute$comparator[1]
				outcome <- res$absolute$stratOutcome[1]
				
				table <-
					res$absolute %>%
					dplyr::mutate(
						combined = paste(
							round(100*estimate, 2),
							paste0(
								"(",
								round(100*lower, 2),
								", ",
								round(100*upper, 2),
								")"
							)
						)
					) %>%
					dplyr::rename(
						Outcome = estOutcome
					) %>%
					dplyr::select(
						Outcome,
						riskStratum,
						combined
					) %>%
					tidyr::spread(riskStratum, combined) %>%
					DT::datatable(
						caption = htmltools::tags$caption(
							style = "caption-side: top; text-align: left;",
							"Table 3: Absolute risk reduction (%) when comparing treatment",
							htmltools::em(
								paste0(
									"(",
									treatment,
									")"
								)
							),
							"to comparator",
							htmltools::em(
								paste0(
									"(",
									comparator,
									")"
								)
							),
							"within strata of predicted risk",
							htmltools::em(
								paste0(
									"(",
									outcome,
									")"
								)
							)
						)
					)
				
				
				return(table)
				
			}
		)
		
		
		output$combinedPlot <- plotly::renderPlotly(
			{
				
				res <- resultSubset()
				
				plot <-
					combinedPlot(
						cases = res$cases,
						relative = res$relative,
						absolute = res$absolute,
						treatment = input$treatment,
						comparator = input$comparator
					)
				return(plot)
				
			}
		)
		
		shiny::observe(
			{
				x <- input$estOutcome
				
				shiny::updateSelectInput(
					session = session,
					inputId = "estOutcomeEstimation",
					choices = x
				)
			}
		)
		
		
		balanceSubset <- shiny::reactive(
			{
				res <- getBalance(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					anal = input$analysis,
					est = input$estOutcomeEstimation,
					db = input$database,
					analyses = analyses,
					mapExposures = mapExposures,
					mapOutcomes = mapOutcomes,
					analysisPath = analysisPath)
				return(res)
			}
		)
		
		output$balanceTable <- DT::renderDataTable(
			{
				res <- balanceSubset() %>%
					dplyr::mutate(
						afterMatchingStdDiff = abs(
							round(
								.$afterMatchingStdDiff,
								2
							)
						),
						beforeMatchingStdDiff = abs(
							round(
								.$beforeMatchingStdDiff,
								2
							)
						)
					) %>%
					dplyr::select(
						c(
							"riskStratum",
							"covariateName",
							"beforeMatchingStdDiff",
							"afterMatchingStdDiff"
						)
					)
				
				DT::datatable(
					res,
					colnames = c(
						"Risk stratum",
						"Covariate name",
						"Before",
						"After"
					)
				) %>%
					return()
			}
		)
		
		psDensitySubset <- shiny::reactive(
			{
				res <- getPsDensity(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					anal = input$analysis,
					est = input$estOutcomeEstimation,
					db = input$database,
					analyses = analyses,
					mapExposures = mapExposures,
					mapOutcomes = mapOutcomes,
					analysisPath = analysisPath)
				return(res)
			}
		)
		
		output$evaluationPlotPs <- shiny::renderPlot(
			{
				
				psDensitySubset() %>%
					dplyr::left_join(
						mapExposures,
						by = c("treatment" = "exposure_id")
					) %>%
					ggplot2::ggplot(
						ggplot2::aes(
							x = x,
							y = y
						)
					) +
					ggplot2::geom_density(
						stat = "identity",
						ggplot2::aes(
							color = exposure_name,
							group = exposure_name,
							fill = exposure_name
						)
					) +
					ggplot2::facet_grid(~riskStratum) +
					ggplot2::ylab(
						label = "Density"
					) +
					ggplot2::xlab(
						label = "Preference score"
					) +
					ggplot2::scale_fill_manual(
						values = c(
							rgb(
								red = 0.8,
								green = 0,
								blue = 0, 
								alpha = 0.5
							),
							rgb(
								red = 0,
								green = 0, 
								blue = 0.8, 
								alpha = 0.5
							)
						)
					) +
					ggplot2::scale_color_manual(
						values = c(
							rgb(
								red = 0.8,
								green = 0,
								blue = 0, 
								alpha = 0.5
							),
							rgb(
								red = 0,
								green = 0,
								blue = 0.8,
								alpha = 0.5
							)
						)
					) +
					ggplot2::theme(
						legend.title = ggplot2::element_blank(),
						legend.position = "top",
						legend.text = ggplot2::element_text(
							margin = ggplot2::margin(
								t = 0, 
								r = 0.5,
								b = 0,
								l = 0.1, 
								unit = "cm"
							)
						)
					) 
			}
		)
		
		output$evaluationPlotBalance <- shiny::renderPlot(
			{
				balanceSubset() %>%
					CohortMethod::plotCovariateBalanceScatterPlot(
						beforeLabel = "Before stratification",
						afterLabel = "After stratification"
					) +
					ggplot2::facet_grid(
						~riskStratum
					)
			}
		)
		
		calibrationSubset <- shiny::reactive(
			{
				res <- getCalibration(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					db = input$database,
					anal = input$analysis,
					predictionPopulation = input$predictionPopulation,
					analyses = analyses,
					mapExposures = mapExposures,
					mapOutcomes = mapOutcomes,
					analysisPath = analysisPath)
				return(res)
			}
		)
		
		output$calibrationPlot <- shiny::renderPlot(
			{
				calibrationSubset() %>%
					dplyr::mutate(
						cohort = factor(
							.$cohort,
							levels = c(
								"Matched",
								"EntirePopulation",
								"Treatment",
								"Comparator"
							)
						)
					) %>%
					ggplot2::ggplot(
						ggplot2::aes(
							x = averagePredictedProbability,
							y = observedIncidence,
							ymin = lower,
							ymax = upper
						)
					) +
					ggplot2::geom_point(
						size = 2,
						color = "black"
					) +
					ggplot2::geom_errorbar() +
					ggplot2::geom_line(
						colour = 'darkgrey'
					) +
					ggplot2::geom_abline(
						intercept = 0,
						slope = 1,
						linetype = 5,
						size = 0.4,
						show.legend = TRUE
					) +
					ggplot2::scale_x_continuous(
						name = "Average Predicted Probability"
					) +
					ggplot2::scale_y_continuous(
						name = "Observed Fraction With Outcome"
					) +
					ggplot2::facet_grid(
						~cohort
					) +
					ggplot2::theme_bw()
				
			}
		)
		
		aucSubset <- shiny::reactive(
			{
				res <- getAuc(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					db = input$database,
					anal = input$analysis,
					predictionPopulation = input$predictionPopulation,
					analyses = analyses,
					mapExposures = mapExposures,
					mapOutcomes = mapOutcomes,
					analysisPath = analysisPath)
				return(res)
			}
		)
		
		predictionPerformanceSubset <- shiny::reactive(
			{
				res <- getPredictionPerformance(
					treat = input$treatment,
					comp = input$comparator,
					strat = input$stratOutcome,
					coh = input$predictionPopulation,
					db = input$database,
					predictionPerformance = predictionPerformance
				)
				return(res)
			}
		)
		
		output$discriminationPlot <- shiny::renderPlot(
			{
				plot <-
					aucSubset() %>%
					dplyr::mutate(
						cohort = factor(
							.$cohort,
							levels = c(
								"Matched",
								"EntirePopulation",
								"Treatment",
								"Comparator"
							)
						)
					) %>%
					ggplot2::ggplot(
						ggplot2::aes(
							x = fpRate,
							y = sens
						)
					) +
					ggplot2::geom_abline(
						intercept = 0,
						slope = 1
					) +
					ggplot2::geom_area(
						color = grDevices::rgb(
							red = 0,
							green = 0,
							blue = 0.8, 
							alpha = 0.8
						),
						fill = grDevices::rgb(
							red = 0,
							green = 0,
							blue = 0.8,
							alpha = 0.4
						)
					) +
					ggplot2::scale_x_continuous(
						name = "1 - specificity"
					) +
					ggplot2::scale_y_continuous(
						name = "Sensitivity"
					) +
					ggplot2::theme_bw()
				
				labels <- predictionPerformanceSubset() %>%
					dplyr::mutate(
						cohort = factor(
							.$cohort,
							levels = c(
								"Matched",
								"EntirePopulation",
								"Treatment",
								"Comparator"
							)
						)
					) %>%
					dplyr::select(
						auc, 
						cohort
					) %>%
					dplyr::mutate(
						auc = paste(
							"AUC:",
							paste0(
								round(
									100*auc,
									digits = 2
								),
								"%"
							)
						)
					)
				
				plot <- plot +
					ggplot2::facet_grid(
						~cohort
					) +
					ggplot2::geom_label(
						data = labels,
						x = .05,
						y = .05,
						hjust = "left",
						vjust = "top",
						alpha = 0.8,
						ggplot2::aes(
							label = auc
						),
						size = 5.5
					)
				
				return(plot)
				
			}
		)
		
		showInfoBox <- function(title, htmlFileName) {
			showModal(
				modalDialog(
					title = title,
					easyClose = TRUE,
					footer = NULL,
					size = "l",
					HTML(
						readChar(
							htmlFileName,
							file.info(htmlFileName)$size) 
					)
				)
			)
		}
		
		observeEvent(input$testInfo,
					 {
					 	showInfoBox(
					 		"Database information",
					 		file.path(
					 			pathToHtml,
					 			paste(
					 				input$database,
					 				"html",
					 				sep = "."
					 			)
					 		)
					 	)
					 }
		)
	}
)
