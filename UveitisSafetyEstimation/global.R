
source("DataPulls.R")
source("PlotsAndTables.R")

shinyData <- "data"

shinySettings <- list(dataFolder = shinyData, blind = FALSE)
dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

load(file.path(dataFolder, "PreMergedShinyData.RData"))

source("DataClean.R")


