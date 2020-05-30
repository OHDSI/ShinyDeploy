# uncomment if running standalone
##runPlp <- readRDS(file.path("data","results.rds"))
##validatePlp <- readRDS(file.path("data","extValidation.rds"))
source("processing.R")

if(is.null(.GlobalEnv$shinySettings$result)){
  result <- 'data'
  print('Extracting results from data folder')
} else{
  result <- .GlobalEnv$shinySettings$result
  print('Extracting results from .GlobalEnv$shinySettings')
}

if(is.null(.GlobalEnv$shinySettings$validation)){
  validation <- NULL
} else{
  validation <- .GlobalEnv$shinySettings$validation
}

inputType <- checkPlpInput(result) # this function checks 
if(!class(validation)%in%c('NULL', 'validatePlp')){
  stop('Incorrect validation class')
}
if(inputType == 'file' & !is.null(validation)){
  warning('Validation input ignored when result is a directory location')
}

summaryTable <- getSummary(result, inputType, validation)


myResultList <- lapply(1:nrow(summaryTable), function(i){paste('Within', as.character(summaryTable$T[i]),
                                                               'predict who will develop ',as.character(summaryTable$O[i]),
                                                               'during', as.character(summaryTable$TAR[i]),
                                                               '-- developed in', as.character(summaryTable$Dev[i]),
                                                               'validated in ',as.character(summaryTable$Val[i]),
                                                               'using model ', as.character(summaryTable$Model[i]),
                                                               'with predictor setting ', as.character(summaryTable$covariateSettingId[i]))})


