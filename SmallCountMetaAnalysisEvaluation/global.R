
resultsFixed <- readRDS("data/fixedFx.rds")
typesFixed <- unique(resultsFixed$type)
metricsFixed <- unique(resultsFixed$metric)
simParamsFixed <- colnames(resultsFixed)[!(colnames(resultsFixed) %in% c("type", "metric", "value"))]

resultsRandom <- readRDS("data/randomFx.rds")
typesRandom <- unique(resultsRandom$type)
metricsRandom <- unique(resultsRandom$metric)
simParamsRandom <- colnames(resultsRandom)[!(colnames(resultsRandom) %in% c("type", "metric", "value"))]
