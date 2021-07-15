packageListUrl <- "https://raw.githubusercontent.com/OHDSI/Hades/master/extras/packages.csv"
hadesPackages <- read.table(packageListUrl, sep = ",", header = TRUE)
baseline <- readRDS("data/baseLine.rds")
