getInputConceptsSql <- SqlRender::readSql("getInputConcepts.sql")

# function that suggest one recommendation based on input string and domain
loadDataFromDB <- function(connPool, sourceDomain, sourceString) {
  
  # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
  sourceString <-  gsub("[^a-zA-Z0-9 ]", " ", sourceString)
  sourceDomain <-  gsub("[^a-zA-Z0-9 ]", " ", sourceDomain)
  
  sql <- SqlRender::render(getInputConceptsSql, 
                           target_database_schema = "results", 
                           vocabulary_database_schema = "vocabulary",
                           source_domain = sourceDomain,
                           source_string = sourceString)
  data <- DatabaseConnector::dbGetQuery(connPool, sql)
  return(data)
}
