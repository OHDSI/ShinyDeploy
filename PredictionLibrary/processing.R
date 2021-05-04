#' Extract a plpResult from the predictionLibrary database
#'
#' @details
#' Load the main results from database into a runPlp object
#'
#' @param chosenRow  The row from the summaryTable of the selected result
#' 
#' @export
loadPlpFromDb <- function(chosenRow){
  result_id <- chosenRow$result_id
  model_id <- chosenRow$model_id
  researcher_id <- chosenRow$researcher_id
  result <- list()
  #covariateSummary
  result$covariateSummary <- DBI::dbGetQuery(conn = con,
                                             paste0("SELECT * FROM covariate_summary WHERE result_id =",result_id))
  #inputSetting
  result$inputSetting <- list()
  tempModSettings <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM model_settings WHERE model_id = ", model_id))
  result$inputSetting$modelSettings <- jsonify::from_json(gsub('\\\\','',tempModSettings['model_settings_json']))

  tempCovSettings <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM covariate_settings WHERE model_id = ", model_id))
  result$inputSetting$dataExtrractionSettings$covariateSettings <- jsonify::from_json(tempCovSettings$covariate_settings_json)
  tempPopSettings <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM population_settings WHERE model_id = ", model_id))
  result$inputSetting$populationSettings <- jsonify::from_json(tempPopSettings$population_settings_json)
  
  #performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM evaluation_statistics WHERE result_id =", result_id))
  result$performanceEvaluation$thresholdSummary <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM threshold_summary WHERE result_id =", result_id))
  result$performanceEvaluation$thresholdSummary$Eval <- trimws(result$performanceEvaluation$thresholdSummary$Eval)
  result$performanceEvaluation$demographicSummary <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM demographic_summary WHERE result_id =", result_id))
  result$performanceEvaluation$calibrationSummary <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM calibration_summary WHERE result_id =", result_id))
  result$performanceEvaluation$calibrationSummary$Eval <- trimws(result$performanceEvaluation$calibrationSummary$Eval)
  result$performanceEvaluation$predictionDistribution <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM prediction_distribution WHERE result_id =", result_id))
  
  result$model$modelSettings <- result$inputSetting$modelSettings
  result$model$populationSettings <- result$inputSetting$populationSettings
  result$model$metaData$call$covariateSettings <- result$inputSetting$dataExtrractionSettings$covariateSettings
  result$researcherInfo <- DBI::dbGetQuery(conn = con, paste0("SELECT researcher_name, researcher_email FROM researchers WHERE researcher_id =", researcher_id))
  result$model_id <- model_id
  
  #hack so the check in plot,utlipl.. doesnt break it
  result$analysisRef <- ""
  result$executionSummary <- ""
  class(result) <- "runPlp"
  return(result)
}

