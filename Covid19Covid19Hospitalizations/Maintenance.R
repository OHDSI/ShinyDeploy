CohortDiagnostics::preMergeDiagnosticsFiles("data")


load(file.path("data", "PreMerged.RData"))
renaming <- read.csv(file.path("data", "covid names.csv"), stringsAsFactors = FALSE)

renaming <- data.frame(cohortName = renaming$id,
                       newCohortName = renaming$new.name)
nrow(cohort)
cohort <- merge(cohort, renaming)
nrow(cohort)
cohort$cohortFullName <- cohort$newCohortName
cohort$newCohortName <- NULL
rm(renaming)
save(list = ls(), file = file.path("data", "PreMerged.RData"))

unlink(list.files("data", ".zip", full.names = TRUE))
