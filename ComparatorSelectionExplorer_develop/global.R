

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("phenotypeLibrarydbUser"),
  password = Sys.getenv("phenotypeLibrarydbPw"),
  server = paste0(Sys.getenv("phenotypeLibraryServer"), "/", Sys.getenv("phenotypeLibrarydb")),
  port = 5432
)

tablePrefix <- "cse_062023_"
resultsSchema <- "comparator_selector"

# decimal formatters
fmtSim <- "%.3f"
fmtSmd <- "%.2f"
