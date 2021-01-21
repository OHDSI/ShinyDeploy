loadPlpStudy <- function(location, output){
  res <- dir(location, pattern = 'Analysis')
  for(i in 1:length(res)){
    result <- PatientLevelPrediction::loadPlpFromCsv(file.path(location, res[i]))
    if(!dir.exists(file.path(output, res[i]))){
      dir.create(file.path(output, res[i]), recursive = T)
    }
    saveRDS(result,  file.path(output, res[i], 'validationResult.rds'))
  }
}


loadPlpStudy(location = 'D:/PCE/ccae_recalibration/export', output = 'D:/PCE/plpViewer/data/Validation/ccae_recalibration')
