prepareData <- function(compareData,
                        databaseId,
                        analysisVariant,
                        domainIds = c(),
                        isBinary) {



  idx <- rep(TRUE, nrow(compareData))

  if (length(databaseId) != 0) {
    idx <- idx & compareData$databaseId %in% databaseId
  }

  if (length(analysisVariant != 0)) {
    idx <- idx & compareData$analysisVariant %in% analysisVariant
  }

  if (length(domainIds) != 0) {
    idx <- idx & compareData$domainId %in% domainIds
  }

  if (length(isBinary) != 0) {
    idx <- idx & compareData$isBinary %in% isBinary
  }

  result <- compareData[idx, ]
  if (nrow(result) == 0) {
    return(NULL)
  } else {
    return(result)
  }
}
