library(DatabaseConnector)
library(DBI)
library(odbc)

# dataLocation <- "~/git/PredictionLibrary/dataHoldingFolder/Ross D. Williamstestmodel4/"
addUserDevToDatabase <- function(dataLocation, con){
   #todo: do a bunch of checks lol
    inputdata <- readr::read_csv(file.path(dataLocation, 'inputdata.csv'))
    target_json <- readr::read_file(file.path(dataLocation, 'target.json'))
    outcome_json <- readr::read_file(file.path(dataLocation, 'outcome.json'))
    colnames(inputdata) <- SqlRender::camelCaseToSnakeCase(colnames(inputdata))
    # get database_id and ifnot insert and get new id
    database_id <- DBI::dbGetQuery(con, paste0("SELECT database_id FROM databases WHERE database_name = '", inputdata$database_name,"';"))[,1]
    if (identical(database_id, integer(0))){ 
      database_id <- insertSqlData(table = "databases",conn = con, primaryKey = "database_id", inputdata$database_name, 
                      inputdata$database_acronym, inputdata$database_desc, inputdata$database_type)[,1]
        database_id <- DBI::dbGetQuery(con, paste0("SELECT database_id FROM databases WHERE database_name = '", inputdata$database_name,"';"))[,1]
      }
    
    # get target_id and ifnot insert and get new id
    target_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$target_name,"';"))[,1]
    if (identical(target_id, integer(0))){ 
      target_id <- insertSqlData(table = "cohorts", conn = con, primaryKey = "cohort_id", inputdata$target_name, target_json)[,1]
      # target_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$target_name,"';"))[,1]
    }
    
    # get outcome_id and ifnot insert and get new id
    outcome_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$outcome_name,"';"))[,1]
    if (identical(outcome_id, integer(0))){ 
      outcome_id <- insertSqlData(table = "cohorts", conn = con, primaryKey = "cohort_id", inputdata$outcome_name, outcome_json)[,1]
      # outcome_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$outcome_name,"';"))[,1]
    }
    
    # get researcher_id and ifnot insert and get new id
    researcher_id <- DBI::dbGetQuery(con, paste0("SELECT researcher_id FROM researchers WHERE researcher_name = '", inputdata$researcher_name,"';"))[,1]
    if (identical(researcher_id, integer(0))){ 
      researcher_id <- addPlpResearcher <- insertSqlData(table = "researchers", conn = con, primaryKey = "researcher_id",
                                        inputdata$researcher_name, inputdata$researcher_email, inputdata$researcher_affiliation)[,1]
      # researcher_id <- DBI::dbGetQuery(con, paste0("SELECT researcher_id FROM researchers WHERE researcher_name = '", inputdata$researcher_name,"';"))[,1]
    }
    
    
    # get model
    plpResult <- readRDS(file.path(dataLocation, 'plpResult.rds'))
    tar <- plpResult$inputSetting$populationSettings$riskWindowEnd - plpResult$inputSetting$populationSettings$riskWindowStart
    # insert model info
    # TODO: fix the file insertion
    model_id <- DBI::dbGetQuery(con, paste0("SELECT model_id FROM models WHERE model_name = '", inputdata$model_name,"';"))[,1]
    if (identical(model_id, integer(0))){
      model_id <- insertSqlData(table ="models", conn = con, primaryKey = "model_id",
                                model_name = "test", target_id, outcome_id, tar,
                                researcher_id, database_id, plp_model_file = "temp")[,1]
      # model_id <- DBI::dbGetQuery(con, paste0("SELECT model_id FROM models WHERE model_name = '", inputdata$model_name,"';"))[,1]
    }

    #insert new result
    # todo: fix original_model_id
    result_id <- insertSqlData(table = "results", conn = con, primaryKey = "result_id",
                                  model_id, researcher_id, database_id,
                                  target_id, outcome_id, tar, inputdata$analysis_type, 
                                  as.character(plpResult$executionSummary$ExecutionDateTime),
                                  as.character(plpResult$executionSummary$PackageVersion$packageVersion),
                                  original_model_id = model_id)[,1]

    # add prediction distribution
    predDist <- plpResult$performanceEvaluation$predictionDistribution %>%
      mutate(result_id = result_id) 
    DBI::dbAppendTable(con = con, name = "prediction_distribution", predDist)

    # add covariateSummary
    # covSummary <- plpResult$covariateSummary
    covSummary <- plpResult$covariateSummary %>%
      filter(covariateValue !=0) %>%
      mutate(result_id = result_id)

     DBI::dbAppendTable(conn = con, name = "covariate_summary", covSummary)

    #thresholdSummary
    threshSum <- plpResult$performanceEvaluation$thresholdSummary %>%
      # mutate(result_id = result_id) %>%
      # # maybe want to make this something other than negative numbers, 999999 and -999999
      tidyr::replace_na(replace = list(predictionThreshold = -1, preferenceThreshold = -1, f1Score = -1, accuracy = -1,
                                       sensitivity = -1, falseNegativeRate = -1, falsePositiveRate = -1, specificity = -1, 
                                       positivePredictiveValue = -1, falseDiscoveryRate = -1, negativePredictiveValue = -1, falseOmissionRate = -1,
                                       positiveLikelihoodRatio = -1, negativeLikelihoodRatio = -1, diagnosticOddsRatio = -1)) %>%
      mutate_if(is.numeric, ~ if_else(is.infinite(.x) & .x > 0,999999,.x)) %>%
      mutate_if(is.numeric, ~ if_else(is.infinite(.x) & .x < 0 ,-999999,.x)) %>%
      mutate(result_id = result_id)

    
    DBI::dbAppendTable(conn = con, name = "threshold_summary", threshSum)
    
    #calibration  summary
    calSum <- plpResult$performanceEvaluation$calibrationSummary %>%
      mutate(result_id = result_id)
    DBI::dbAppendTable(con = con, name = "calibration_summary", calSum)
    
    
    # add evalutaion statistics
    evalStat <- as.data.frame(plpResult$performanceEvaluation$evaluationStatistics) %>%
      filter(Eval == "test") %>%
      select(Metric, Value) %>%
      tidyr::pivot_wider(names_from = Metric, values_from = Value) %>%
      mutate(result_id = result_id)

    
    colnames(evalStat) <- c("population_size", "outcome_count", 
                            "AUC_auc", "AUC_auc_lb95ci", "AUC_auc_ub95ci", "AUPRC", "brier_score", "brier_scaled",                 
                            "calibration_intercept", "calibration_slope", "result_id")
    nas <- which(is.na(evalStat)==TRUE)         # get index of NA values
    evalStat[nas] <- -1  
    DBI::dbAppendTable(con = con, name = "evaluation_statistics", evalStat)
    
    # demographic summary 
    demSum <- plpResult$performanceEvaluation$demographicSummary %>%
      mutate(result_id = result_id)
    DBI::dbAppendTable(con = con, name = "demographic_summary", demSum)
    
    #add populationSettings
    popSettings <- insertSqlData(table = "population_settings", conn = con, primaryKey = "population_setting_id", model_id, jsonify::to_json(plpResult$model$populationSettings))
    
    #add modelSettings
    modelSettings <- insertSqlData(table = "model_settings", conn = con, primaryKey = "model_setting_id", model_id, plpResult$model$modelSettings$model, jsonify::to_json(plpResult$model$modelSettings))
    
    covariateSettings <- insertSqlData(table = "covariate_settings", conn = con, primaryKey = "covariate_setting_id", model_id, jsonify::to_json(plpResult$model$metaData$call$covariateSettings))
}

addUserValToDatabase <- function(dataLocation, con){
  #todo: do a bunch of checks lol
  inputdata <- readr::read_csv(file.path(dataLocation, 'inputdata.csv'))
 
  colnames(inputdata) <- SqlRender::camelCaseToSnakeCase(colnames(inputdata))
  # get database_id and ifnot insert and get new id
  database_id <- DBI::dbGetQuery(con, paste0("SELECT database_id FROM databases WHERE database_name = '", inputdata$database_name,"';"))[,1]
  if (identical(database_id, integer(0))){ 
    database_id <- insertSqlData(table = "databases",conn = con, primaryKey = "database_id", inputdata$database_name, 
                                 inputdata$database_acronym, inputdata$database_desc, inputdata$database_type)[,1]
    database_id <- DBI::dbGetQuery(con, paste0("SELECT database_id FROM databases WHERE database_name = '", inputdata$database_name,"';"))[,1]
  }
  
  # # get target_id and ifnot insert and get new id
  # target_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$target_name,"';"))[,1]
  # if (identical(target_id, integer(0))){ 
  #   target_id <- insertSqlData(table = "cohorts", conn = con, primaryKey = "cohort_id", inputdata$target_name, target_json)[,1]
  #   # target_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$target_name,"';"))[,1]
  # }
  # 
  # # get outcome_id and ifnot insert and get new id
  # outcome_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$outcome_name,"';"))[,1]
  # if (identical(outcome_id, integer(0))){ 
  #   outcome_id <- insertSqlData(table = "cohorts", conn = con, primaryKey = "cohort_id", inputdata$outcome_name, outcome_json)[,1]
  #   # outcome_id <- DBI::dbGetQuery(con, paste0("SELECT cohort_id FROM cohorts WHERE cohort_name = '", inputdata$outcome_name,"';"))[,1]
  # }
  # 
  # get researcher_id and ifnot insert and get new id
  researcher_id <- DBI::dbGetQuery(con, paste0("SELECT researcher_id FROM researchers WHERE researcher_name = '", inputdata$researcher_name,"';"))[,1]
  if (identical(researcher_id, integer(0))){ 
    researcher_id <- addPlpResearcher <- insertSqlData(table = "researchers", conn = con, primaryKey = "researcher_id",
                                                       inputdata$researcher_name, inputdata$researcher_email, inputdata$researcher_affiliation)[,1]
    # researcher_id <- DBI::dbGetQuery(con, paste0("SELECT researcher_id FROM researchers WHERE researcher_name = '", inputdata$researcher_name,"';"))[,1]
  }
  
  
  # get model
  valResult <- readRDS(file.path(dataLocation, 'validationResult.rds'))
  tar <- valResult$inputSetting$populationSettings$riskWindowEnd - valResult$inputSetting$populationSettings$riskWindowStart
  # insert model info
  # TODO: fix the file insertion
  model_id <- inputdata$model_id
  model_info <- DBI::dbGetQuery(conn = con, paste0("SELECT * FROM models WHERE model_id = ", model_id))
  model_info
  #insert new result
  # todo: fix original_model_id
  result_id <- insertSqlData(table = "results", conn = con, primaryKey = "result_id",
                             model_id, researcher_id, database_id,
                             model_info$target_id, model_info$outcome_id, model_info$tar, inputdata$analysis_type, 
                             as.character(valResult$executionSummary$ExecutionDateTime),
                             as.character(valResult$executionSummary$PackageVersion$packageVersion),
                             original_model_id = model_id)[,1]
  
  # add prediction distribution
  predDist <- valResult$performanceEvaluation$predictionDistribution %>%
    mutate(result_id = result_id) 
  DBI::dbAppendTable(con = con, name = "prediction_distribution", predDist)
  
  # add covariateSummary
  # covSummary <- valResult$covariateSummary
  covSummary <- valResult$covariateSummary %>%
    filter(covariateValue !=0) %>%
    mutate(result_id = result_id)
  
  DBI::dbAppendTable(conn = con, name = "covariate_summary", covSummary)
  
  #thresholdSummary
  threshSum <- valResult$performanceEvaluation$thresholdSummary %>%
    # mutate(result_id = result_id) %>%
    # # maybe want to make this something other than negative numbers, 999999 and -999999
    tidyr::replace_na(replace = list(predictionThreshold = -1, preferenceThreshold = -1, f1Score = -1, accuracy = -1,
                                     sensitivity = -1, falseNegativeRate = -1, falsePositiveRate = -1, specificity = -1, 
                                     positivePredictiveValue = -1, falseDiscoveryRate = -1, negativePredictiveValue = -1, falseOmissionRate = -1,
                                     positiveLikelihoodRatio = -1, negativeLikelihoodRatio = -1, diagnosticOddsRatio = -1)) %>%
    mutate_if(is.numeric, ~ if_else(is.infinite(.x) & .x > 0,999999,.x)) %>%
    mutate_if(is.numeric, ~ if_else(is.infinite(.x) & .x < 0 ,-999999,.x)) %>%
    mutate(result_id = result_id)
  
  
  DBI::dbAppendTable(conn = con, name = "threshold_summary", threshSum)
  
  #calibration  summary
  calSum <- valResult$performanceEvaluation$calibrationSummary %>%
    mutate(result_id = result_id)
  DBI::dbAppendTable(con = con, name = "calibration_summary", calSum)
  
  
  # add evalutaion statistics
  evalStat <- as.data.frame(valResult$performanceEvaluation$evaluationStatistics) %>%
    filter(Eval == "validation") %>%
    select(Metric, Value) %>%
    tidyr::pivot_wider(names_from = Metric, values_from = Value) %>%
    mutate(result_id = result_id)
  
  
  colnames(evalStat) <- c("population_size", "outcome_count", 
                          "AUC_auc", "AUC_auc_lb95ci", "AUC_auc_ub95ci", "AUPRC", "brier_score", "brier_scaled",                 
                          "calibration_intercept", "calibration_slope", "result_id")
  nas <- which(is.na(evalStat)==TRUE)         # get index of NA values
  evalStat[nas] <- -1  
  DBI::dbAppendTable(con = con, name = "evaluation_statistics", evalStat)
  
  # demographic summary 
  demSum <- valResult$performanceEvaluation$demographicSummary %>%
    mutate(result_id = result_id)
  DBI::dbAppendTable(con = con, name = "demographic_summary", demSum)
  
  # #add populationSettings
  # popSettings <- insertSqlData(table = "population_settings", conn = con, primaryKey = "population_setting_id", model_id, jsonify::to_json(valResult$model$populationSettings))
  # 
  # #add modelSettings
  # modelSettings <- insertSqlData(table = "model_settings", conn = con, primaryKey = "model_setting_id", model_id, valResult$model$modelSettings$model, jsonify::to_json(valResult$model$modelSettings))
  # 
  # covariateSettings <- insertSqlData(table = "covariate_settings", conn = con, primaryKey = "covariate_setting_id", model_id, jsonify::to_json(valResult$model$metaData$call$covariateSettings))
}
