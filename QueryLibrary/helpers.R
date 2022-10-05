executeQuery <- function(sql, connectionDetails, targetDialect) {
  con <- DatabaseConnector::connect(connectionDetails)
  sql <- SqlRender::translateSql(sql, targetDialect = targetDialect)$sql
  results <- DatabaseConnector::querySql(con, sql = sql)
  disconnect(con)
  return(results)
}

testQuery <- function(mdFile, connectionDetails, connection, inputValues, oracleTempSchema) {
  sqlSource <- getSqlFromMarkdown(mdFile)

  parameters <- getParameters(sqlSource)
  parameterValues <- list()
  for (param in parameters) {
    value <- inputValues[[param]]
    if (!is.null(value)) {
      parameterValues[[param]] <- value
    }
  }
  sql <- do.call("render", append(sqlSource, parameterValues))
  warningString <- c()
  handleWarning <- function(e) {
    output$warnings <- e$message
  }

  if (oracleTempSchema == "")
    oracleTempSchema <- NULL
  sql <- withCallingHandlers(suppressWarnings(translateSql(sql,
                                                           targetDialect = tolower(connectionDetails["dbms"]),
                                                           oracleTempSchema = oracleTempSchema)$sql), warning = handleWarning)
  if (!is.null(warningString))
    output$warnings <- warningString
  result <- "Syntactically correct"
  results <- ""
  tryCatch({
    results <- DatabaseConnector::querySql(connection, sql = sql)
  }, error = function(e) {
    result <<- as.character(e$message)
  })
  return(result)
}


getParameters <- function(sql) {
  params <- regmatches(sql, gregexpr("@[a-zA-Z0-9_]+", sql))[[1]]
  params <- unique(params)
  params <- params[order(params)]
  params <- substr(params, 2, nchar(params))
  return(params)
}


loadQueriesTable <- function(queryFolder, userFolder) {
  mdFiles <- list.files(queryFolder, recursive = TRUE, pattern = "*.md")

  group <- as.data.frame(sapply(1:length(mdFiles),
                                function(x) getVariableFromMarkdown(paste0(queryFolder,
                                                                           "/",
                                                                           mdFiles[x]), "Group")))
  name <- as.data.frame(sapply(1:length(mdFiles),
                               function(x) getVariableFromMarkdown(paste0(queryFolder,
                                                                          "/",
                                                                          mdFiles[x]), "Name")))
  cdmVersion <- as.data.frame(sapply(1:length(mdFiles),
                                     function(x) getVariableFromMarkdown(paste0(queryFolder,
                                                                                "/",
                                                                                mdFiles[x]), "CDM Version")))
  author <- as.data.frame(sapply(1:length(mdFiles),
                                 function(x) getVariableFromMarkdown(paste0(queryFolder,
                                                                            "/",
                                                                            mdFiles[x]), "Author")))
  queriesTable <- cbind(group, name, cdmVersion, author)
  colnames(queriesTable) <- c("Group", "Name", "CDM_Version", "Author")

  if (dir.exists(file.path(userFolder))) {
    userFiles <- list.files(userFolder, recursive = TRUE, pattern = "*.md")
    group <- as.data.frame(sapply(1:length(mdFiles),
                                  function(x) getVariableFromMarkdown(paste0(userFolder,
                                                                             "/",
                                                                             mdFiles[x]), "Group")))
    name <- as.data.frame(sapply(1:length(mdFiles),
                                 function(x) getVariableFromMarkdown(paste0(userFolder,
                                                                            "/",
                                                                            mdFiles[x]), "Name")))
    cdmVersion <- as.data.frame(sapply(1:length(userFiles),
                                       function(x) getVariableFromMarkdown(paste0(userFolder, "/", userFiles[x]),
                                                                           "CDM Version")))
    author <- as.data.frame(sapply(1:length(userFiles),
                                   function(x) getVariableFromMarkdown(paste0(userFolder,
                                                                              "/",
                                                                              userFiles[x]), "Author")))
    userTable <- cbind(group, name, cdmVersion, author)
    userTable["Custom"] <- TRUE
    colnames(queriesTable) <- c("Group", "Name", "CDM_version", "Author", "Custom")
    queriesTable <- rbind(queriesTable, userTable)
  }

  queriesTable <- data.frame(lapply(queriesTable, function(x) {
    gsub("_", " ", x)
  }))

  queriesTable <- data.frame(lapply(queriesTable, function(x) {
    gsub(".md", "", x)
  }))
  return(queriesTable)
}


