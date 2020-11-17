library(shiny)
library(DatabaseConnector)

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(storage = "filesystem", dataFolder = "data")
  } else {
    stop("Results data not found")
  }
}
dataStorage <- shinySettings$storage
dataFolder <- shinySettings$dataFolder

suppressWarnings(rm("cohort", "cohortCount", "databases"))


csvFiles <- list.files(dataFolder, pattern = ".csv", full.names = TRUE)

loadFile <- function(file, folder, overwrite) {
  # print(file)
  tableName <- gsub(".csv$", "", file)
  camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
  data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
  colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
  
  if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
    existingData <- get(camelCaseName, envir = .GlobalEnv)
    if (nrow(existingData) > 0) {
      if (nrow(data) > 0 &&
          all(colnames(existingData) %in% colnames(data)) &&
          all(colnames(data) %in% colnames(existingData))) {
        data <- data[, colnames(existingData)]
      }
      
      if (!isTRUE(all.equal(colnames(data), colnames(existingData), check.attributes = FALSE))) {
        stop("Table columns do no match previously seen columns. Columns in ", 
             file, 
             ":\n", 
             paste(colnames(data), collapse = ", "), 
             "\nPrevious columns:\n",
             paste(colnames(existingData), collapse = ", "))
      }
    }
    data <- rbind(existingData, data)
  }
  assign(camelCaseName, data, envir = .GlobalEnv)
  
  invisible(NULL)
}

for (i in 1:length(csvFiles)) {
  csvFile <- basename(csvFiles[[i]])
  writeLines(paste("Processing", csvFile))
  loadFile(csvFile, dataFolder, overwrite = 1)
}

countByDB <- unique(dmardsTotal[,c("database", "total")])

formatDbName <- function(fullName, dbKey, countByDB) {
  return(paste0(fullName, "\n(n=", format(countByDB[countByDB$database == dbKey, ]$total, big.mark=",", scientific = FALSE), ")"))
}

databaseList <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("databaseId", "databaseDescription"))
databaseList <- rbind(databaseList,data.frame(databaseId="Australia", databaseDescription=formatDbName("IQVIA IMS - EMR, AU", "Australia", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="Belgium", databaseDescription=formatDbName("IQVIA LPD - EMR, BE", "Belgium", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="Estonia", databaseDescription=formatDbName("Estonia - EMR, EE", "Estonia", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="SIDIAP", databaseDescription=formatDbName("SIDIAP - EMR, ES", "SIDIAP", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="France", databaseDescription=formatDbName("IQVIA IMS - EMR, FR", "France", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="JMDC", databaseDescription=formatDbName("JMDC - Claims, JP", "JMDC", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="IPCI", databaseDescription=formatDbName("IPCI - EMR, NL", "IPCI", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="THIN", databaseDescription=formatDbName("IQVIA THIN - EMR, UK", "THIN", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="CCAE", databaseDescription=formatDbName("IBM CCAE - Claims, US", "CCAE", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="MDCD", databaseDescription=formatDbName("IBM MDCD - Claims, US", "MDCD", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="MDCR", databaseDescription=formatDbName("IBM MDCR - Claims, US", "MDCR", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="AmbEMR", databaseDescription=formatDbName("IQVIA Amb - EMR, US", "AmbEMR", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="Optum_DOD", databaseDescription=formatDbName("OPTUM DOD - Claims, US", "Optum_DOD", countByDB)))
databaseList <- rbind(databaseList,data.frame(databaseId="Optum_Panther", databaseDescription=formatDbName("OPTUM EHR - EMR, US", "Optum_Panther", countByDB)))

drugLevels <- c("methotrexate", "hydroxychloroquine", "sulfasalazine", "leflunomide", "methotrexate +  hydroxychloroquine", "Other DMARDs & Minocycline")
drugLevelsColorBrew <- scale_fill_brewer(palette = "RdYlBu", direction = 1)

dmardsByYearAndDatabase <- merge(dmardsByYear, database, by.x = "database", by.y = "databaseId")
dmardsByYearAndDatabase <- merge(dmardsByYearAndDatabase, databaseList, by.x = "database", by.y = "databaseId")
