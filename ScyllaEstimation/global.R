library(dplyr)
source("DataPulls.R")
source("PlotsAndTables.R")

blind <- TRUE

designs <- data.frame(
  description = c("Admission to intensive services; washout; -1 end days; ",
                  "Admission to intensive services; no washout; 0 end days; ",
                  "During hospitalization to intensive services; washout; -1 end days; ",
                  "Post testing to hospitalization; washout; 0 end days; "),
  label = c("Treatment administered on the date of admission of hospitalization and prior to intensive services and 365d prior observation",
            "Treatment administered on the date of admission of hospitalization and prior to intensive services with no prior observation",
            "Treatment administered during hospitalization and prior to intensive services and 365d prior observation",
            "Persons with a COVID-19 diagnosis record or a SARS-CoV-2 positive test prior to inpatient visit or intensive services and 365d prior observation"),
  remove = c(" with Treatment administered on the date of admission of hospitalization and prior to intensive services and 365d prior observation",
             " with Treatment administered on the date of admission of hospitalization and prior to intensive services with no prior observation",
             " with Treatment administered during hospitalization and prior to intensive services and 365d prior observation",
             " with Persons with a COVID-19 diagnosis record or a SARS-CoV-2 positive test prior to inpatient visit or intensive services and 365d prior observation"),
  idMask = c(100, 200, 300, 400)
)


positiveControlOutcome <- NULL

connectionPool <- pool::dbPool(
  drv = DatabaseConnector::DatabaseConnectorDriver(),
  dbms = "postgresql",
  server = paste(Sys.getenv("shinydbServer"), Sys.getenv("shinydbDatabase"), sep = "/"),
  port = Sys.getenv("shinydbPort"),
  user = Sys.getenv("scylladbUser"),
  password = Sys.getenv("scylladbPw")
)

onStop(function() {
  if (DBI::dbIsValid(connectionPool)) {
    writeLines("Closing database pool")
    pool::poolClose(connectionPool)
  }
})

resultsDatabaseSchema <- "scylla_estimation"


loadResultsTable <- function(tableName) {
  tryCatch({
    table <- DatabaseConnector::dbReadTable(connectionPool, 
                                            paste(resultsDatabaseSchema, tableName, sep = "."))
  }, error = function(err) {
    stop("Error reading from ", paste(resultsDatabaseSchema, tableName, sep = "."), ": ", err$message)
  })
  colnames(table) <- SqlRender::snakeCaseToCamelCase(colnames(table))
  assign(SqlRender::snakeCaseToCamelCase(tableName), dplyr::as_tibble(table), envir = .GlobalEnv)
}

loadResultsTable("database")
loadResultsTable("exposure_of_interest")
loadResultsTable("outcome_of_interest")
loadResultsTable("cohort_method_analysis")

tcos <- DatabaseConnector::dbGetQuery(connectionPool, 
                                                    sprintf("SELECT DISTINCT target_id, comparator_id, outcome_id FROM %s.cohort_method_result;", resultsDatabaseSchema))
colnames(tcos) <- SqlRender::snakeCaseToCamelCase(colnames(tcos))
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
validExposureIds <- unique(c(tcos$targetId, tcos$comparatorId))

getDesign <- function(targetId) {
  tcoDesign <- as.integer(
    sub(pattern = "[[:digit:]]{4}", replacement = "",
        sub(pattern = "1$", replacement = "", targetId)))
  if (tcoDesign == 1) {
    return(100)
  } else if (tcoDesign == 2) {
    return(200)
  } else if (tcoDesign == 3) {
    return(300)
  } else if (tcoDesign == 2002) {
    return(400)
  } else {
    stop(paste("Unknown analysis plan for targetId", targetId))
  }
}

vecDesign <- Vectorize(getDesign)

exposureOfInterest$design <- vecDesign(exposureOfInterest$exposureId)

exposureOfInterest <- exposureOfInterest %>% 
  inner_join(designs %>% 
               select(idMask, remove), by = c("design" = "idMask")) %>%
  rowwise() %>% 
  mutate(shortName = sub(pattern = remove, replacement = "", x = exposureName)) %>%
  filter(exposureId %in% validExposureIds)

designs <- designs %>%
  filter(idMask %in% unique(exposureOfInterest$design))

cohortMethodAnalysis <- cohortMethodAnalysis %>% 
  mutate(design = floor(analysisId / 100) * 100) %>%
  inner_join(designs %>% 
               select(idMask, description) %>% 
               rename(remove = description), 
             by = c("design" = "idMask")) %>%
  rowwise() %>% 
  mutate(shortDescription = sub(pattern = remove, replacement = "", x = description))

