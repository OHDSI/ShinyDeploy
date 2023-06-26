# details of results database
# Created for data.ohdsi.org
# details of results database

JDBC_PATH <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", "./.drivers")

if (!dir.exists(JDBC_PATH)) {
  dir.create(JDBC_PATH)
}

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("REWARD_DB_USER"),
  password = Sys.getenv("REWARD_DB_PASSWORD"),
  server = "reward.cterqq54xyuu.us-east-1.rds.amazonaws.com/rewardb_dev",
  port = 5432,
  pathToDriver = JDBC_PATH
)


if (connectionDetails$dbms != "sqlite" &&
  !any(grepl(connectionDetails$dbms, list.files(JDBC_PATH)))) {
  DatabaseConnector::downloadJdbcDrivers(connectionDetails$dbms)
}

resultsSchema <- "cse_062023"
tablePrefix <- "cse_"

# decimal formatters
fmtSim <- "%.3f"
fmtSmd <- "%.2f"
