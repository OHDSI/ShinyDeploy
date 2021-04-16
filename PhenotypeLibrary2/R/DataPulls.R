# given a list of standard conceptIds, get recommended concepts.
loadRecommenderStandardFromDatabase <-
  function(dataSource = .GlobalEnv,
           conceptList,
           sql = SqlRender::readSql("sql/RecommendationStandard.sql")) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
      conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
      
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          sql = sql,
          # results_database_schema = dataSource$resultsDatabaseSchema,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          source_list = conceptList,
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::filter(!.data$conceptId %in% conceptList) %>% 
        dplyr:::arrange(dplyr::desc(.data$descendantRecordCount))
      return(data)
    }
  }


# given a list of non standard conceptIds, get recommended conceptIds
loadRecommenderSourceFromDatabase <-
  function(dataSource = .GlobalEnv,
           conceptList,
           sql = SqlRender::readSql("sql/RecommendationSource.sql")) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
      conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
      
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          sql = sql,
          # results_database_schema = dataSource$resultsDatabaseSchema,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          source_list = conceptList,
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::filter(!.data$conceptId %in% conceptList)  %>% 
        dplyr::arrange(dplyr::desc(.data$descendantRecordCount))
      return(data)
    }
  }



# given a concept set expression, get optimized concept set expression
getOptimizationRecommendationForConceptSetExpression <-
  function(dataSource = .GlobalEnv,
           sqlWithTemporaryTable = SqlRender::readSql("sql/optimizeConceptSetWithTemporaryTable.sql"),
           sqlWithoutTemporaryTable = SqlRender::readSql("sql/optimizeConceptSetWithoutTempTable.sql"),
           conceptSetExpression) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      conceptSetExpressionTable <-
        getConceptSetDataFrameFromConceptSetExpression(conceptSetExpression =
                                                         conceptSetExpression)
      conceptSetConceptIdsExcluded <- conceptSetExpressionTable %>%
        dplyr::filter(.data$isExcluded == TRUE) %>%
        dplyr::pull(.data$conceptId)
      conceptSetConceptIdsDescendantsExcluded <-
        conceptSetExpressionTable %>%
        dplyr::filter(.data$isExcluded == TRUE &&
                        .data$includeDescendants == TRUE) %>%
        dplyr::pull(.data$conceptId)
      conceptSetConceptIdsNotExcluded <-
        conceptSetExpressionTable %>%
        dplyr::filter(!.data$isExcluded == TRUE) %>%
        dplyr::pull(.data$conceptId)
      conceptSetConceptIdsDescendantsNotExcluded <-
        conceptSetExpressionTable %>%
        dplyr::filter(!.data$isExcluded == TRUE &&
                        .data$includeDescendants == TRUE) %>%
        dplyr::pull(.data$conceptId)
      
      if (any(is.null(conceptSetConceptIdsExcluded),
              is.na(conceptSetConceptIdsExcluded),
              length(conceptSetConceptIdsExcluded) == 0)) {
        conceptSetConceptIdsExcluded <- 0
      }
      if (any(
        is.null(conceptSetConceptIdsDescendantsExcluded),
        is.na(conceptSetConceptIdsDescendantsExcluded),
        length(conceptSetConceptIdsDescendantsExcluded) == 0
      )) {
        conceptSetConceptIdsDescendantsExcluded <- 0
      }
      if (any(
        is.null(conceptSetConceptIdsNotExcluded),
        is.na(conceptSetConceptIdsNotExcluded),
        length(conceptSetConceptIdsNotExcluded) == 0
      )) {
        conceptSetConceptIdsNotExcluded <- 0
      }
      if (any(
        is.null(conceptSetConceptIdsDescendantsNotExcluded),
        is.na(conceptSetConceptIdsDescendantsNotExcluded),
        length(conceptSetConceptIdsDescendantsNotExcluded) == 0
      )) {
        conceptSetConceptIdsDescendantsNotExcluded <- 0
      }
      
      #switch between sql with or without temp table based on
      #number of concept ids to optimize
      if (length(unique(
        c(
          conceptSetConceptIdsExcluded,
          conceptSetConceptIdsDescendantsExcluded,
          conceptSetConceptIdsNotExcluded,
          conceptSetConceptIdsDescendantsNotExcluded
        )
      )) > 250) {
        sql <- sqlWithTemporaryTable
      } else {
        sql <- sqlWithoutTemporaryTable
      }
      
      sql <- SqlRender::render(
        sql = sql,
        vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
        conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
      )
      
      
      
      if (length(unique(
        c(
          conceptSetConceptIdsExcluded,
          conceptSetConceptIdsDescendantsExcluded,
          conceptSetConceptIdsNotExcluded,
          conceptSetConceptIdsDescendantsNotExcluded
        )
      )) > 250) {
        DatabaseConnector::renderTranslateExecuteSql(connection = dataSource$connection,
                                                     sql = sql)
        retrieveSql <-
          SqlRender::render(sql = "SELECT * FROM #optimized_set;")
      } else {
        retrieveSql <- sql
      }
      
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          sql = retrieveSql,
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::arrange(1)
      return(data)
    }
  }


# given a list of conceptIds, get their descendants
getDescendantConceptsFromDatabase <-
  function(dataSource = .GlobalEnv,
           sql = SqlRender::readSql("sql/GetDescendantConcepts.sql"),
           descendantConceptId) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      if (any(is.null(descendantConceptId),
              is.na(descendantConceptId),
              length(descendantConceptId) == 0)) {
        descendantConceptId <- 0
      }
      sql <- SqlRender::render(sql = sql)
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          conceptsIdsToGetDescendants = descendantConceptId,
          sql = sql, 
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::arrange(1)
      return(data)
    }
  }


# given a list of conceptIds, get their mapped
getMappedConceptsFromDatabase <-
  function(dataSource = .GlobalEnv,
           sql = SqlRender::readSql("sql/GetMappedSourcecodes.sql"),
           mappedConceptId) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      if (any(is.null(mappedConceptId),
              is.na(mappedConceptId),
              length(mappedConceptId) == 0)) {
        mappedConceptId <- 0
      }
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          conceptsIdsToGetMapped = mappedConceptId,
          sql = sql, 
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::arrange(1)
      return(data)
    }
  }


# Given a concept set expression, get the resolved concepts
resolveConceptSetExpressionUsingDatabase <- function(dataSource = .GlobalEnv,
                                                     conceptSetExpression) {
  if (is(dataSource, "environment")) {
    dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
  } else {
  # convert concept set expression R object (list) to data frame
  conceptSetExpressionTable <- getConceptSetDataFrameFromConceptSetExpression(conceptSetExpression = 
                                                                                conceptSetExpression)
  
  
  # get all descendant concept ids (as dataframe) for concepts that have includeDescendants selected in conceptSetExpression
  descendantConcepts <-
    getDescendantConceptsFromDatabase(
      dataSource = dataSource,
      descendantConceptId = conceptSetExpressionTable %>%
        dplyr::filter(.data$includeDescendants == TRUE) %>%
        dplyr::pull(.data$conceptId)
    )
  
  # get all conceptIds (as dataframe) that are excluded in concept set expression
  excludedConceptIds <- conceptSetExpressionTable %>% 
    dplyr::filter(.data$isExcluded == TRUE) %>% 
    dplyr::select(.data$conceptId)
  # get all conceptIds (as dataframe) that are excluded in concept set expression with descendants
  excludedConceptIdsWithDescendants <- descendantConcepts %>% 
    dplyr::filter(.data$ancestorConceptId %in% (conceptSetExpressionTable %>% 
                                                  dplyr::filter(.data$isExcluded == TRUE) %>% 
                                                  dplyr::filter(.data$includeDescendants == TRUE) %>% 
                                                  dplyr::pull(.data$conceptId)))
  
  # conceptIds in conceptSetExpression table
  conceptIdsInConceptSetExpressionTableToBeIncluded <-
    union(x = conceptSetExpressionTable %>%
            dplyr::pull(.data$conceptId),
          y = descendantConcepts %>% dplyr::pull(.data$conceptId))
  conceptIdsInConceptSetExpressionTableToBeExcluded <-
    union(x = excludedConceptIds %>% dplyr::pull(.data$conceptId),
          y = excludedConceptIdsWithDescendants %>% dplyr::pull(.data$conceptId))
  
  
  # removed all excluded conceptIds including those with descendants == TRUE
  resolvedConceptIds <- setdiff(x = conceptIdsInConceptSetExpressionTableToBeIncluded,
                                y = conceptIdsInConceptSetExpressionTableToBeExcluded)
  
  #get all resolved concept Ids as data frame
  resolvedConceptIds <- dplyr::union(conceptSetExpressionTable %>% 
                                       dplyr::filter(.data$isExcluded == FALSE) %>% 
                                       dplyr::select(.data$conceptId, .data$conceptName, .data$conceptCode,
                                                     .data$domainId, .data$vocabularyId, .data$conceptClassId,
                                                     .data$standardConcept, .data$invalidReason),
                                     descendantConcepts %>% 
                                       dplyr::select(.data$conceptId, .data$conceptName, .data$conceptCode,
                                                     .data$domainId, .data$vocabularyId, .data$conceptClassId,
                                                     .data$standardConcept, .data$invalidReason)) %>% 
    dplyr::filter(.data$conceptId %in% resolvedConceptIds) %>% 
    dplyr::select(.data$conceptId, .data$conceptName, .data$conceptCode,
                  .data$domainId, .data$vocabularyId, .data$conceptClassId,
                  .data$standardConcept, .data$invalidReason) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(.data$conceptId)
  
  # get the mapped concepts for the resolved conceptIds
  mappedConcepts <-
    getMappedConceptsFromDatabase(
      dataSource = dataSource,
      mappedConceptId =  resolvedConceptIds %>% 
        dplyr::pull(.data$conceptId)
    ) %>% 
    dplyr::filter(.data$standardConceptId != .data$conceptId) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(y = resolvedConceptIds %>% 
                       dplyr::select(.data$conceptId, 
                                     .data$conceptName) %>% 
                       dplyr::rename(standardConceptName = .data$conceptName),
                     by = c("standardConceptId" = "conceptId")) %>% 
    dplyr::relocate(.data$standardConceptId, .data$standardConceptName) %>% 
    dplyr::arrange(.data$standardConceptId, .data$standardConceptName)
  
  output <- list(resolvedConcepts = resolvedConceptIds,
                 mappedConcepts = mappedConcepts)
  return(output)
  }
}




getConceptSetDetailsFromCohortDefinition <-
  function(dataSource = .GlobalEnv,
           cohortDefinitionExpression) {
    if ("expression" %in% names(cohortDefinitionExpression)) {
      expression <- cohortDefinitionExpression$expression
    }
    else {
      expression <- cohortDefinitionExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(dplyr::tibble())
    }
    
    conceptSetExpression <- expression$ConceptSets %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(json = RJSONIO::toJSON(x = .data$expression,
                                           pretty = TRUE))
    
    conceptSetExpressionDetails <- list()
    i <- 0
    for (id in conceptSetExpression$id) {
      i <- i + 1
      conceptSetExpressionDetails[[i]] <-
        getConceptSetDataFrameFromConceptSetExpression(conceptSetExpression =
                                                         conceptSetExpression[i, ]$expression$items) %>%
        dplyr::rename(
          is_excluded = .data$isExcluded,
          include_descendants = .data$includeDescendants,
          include_mapped = .data$includeMapped
        ) %>%
        dplyr::mutate(id = id) %>%
        dplyr::relocate(.data$id) %>% 
        dplyr::arrange(.data$id)
    }
    conceptSetExpressionDetails <-
      dplyr::bind_rows(conceptSetExpressionDetails)
    colnames(conceptSetExpressionDetails) <-
      snakeCaseToCamelCase(colnames(conceptSetExpressionDetails))
    output <- list(conceptSetExpression = conceptSetExpression,
                   conceptSetExpressionDetails = conceptSetExpressionDetails)
    return(output)
  }


getConceptSetExpressionFromConceptTable <- function(conceptTable) {
  conceptTable <- conceptTable %>%
    dplyr::select(.data$conceptId,
                  .data$exclude,
                  .data$descendants,
                  .data$mapped)
  conceptTable <-
    readr::type_convert(df = conceptTable,
                        col_types = readr::cols(),
                        trim_ws = TRUE)
  
  conceptSetExpression <- list()
  conceptSetExpression$items <- list()
  for (i in (1:nrow(conceptTable))) {
    conceptSetExpression$items[[i]] <- list()
    conceptSetExpression$items[[i]]$concept$conceptId <-
      conceptTable[i, ]$conceptId
    conceptSetExpression$items[[i]]$isExcluded <-
      conceptTable$exclude[i]
    conceptSetExpression$items[[i]]$includeDescendants <-
      conceptTable$descendants[i]
    conceptSetExpression$items[[i]]$includeMapped <-
      conceptTable$mapped[i]
  }
  return(conceptSetExpression)
}


getDetailsForConceptIds <- function(dataSource = dataSource,
                                    conceptIds) {
  sql <- "SELECT CONCEPT_ID,
        	CONCEPT_NAME,
        	VOCABULARY_ID,
        	STANDARD_CONCEPT,
        	INVALID_REASON,
        	CONCEPT_CODE,
        	CONCEPT_CLASS_ID,
        	DOMAIN_ID
        FROM @vocabulary_database_schema.concept
        WHERE CONCEPT_ID IN (@concept_id_list);"
  
  data <-
    renderTranslateQuerySql(
      connection = dataSource$connection,
      vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
      oncept_id_list = conceptIds,
      sql = sql, 
      snakeCaseToCamelCase = TRUE
    ) %>%  
    dplyr::arrange(1)
  return(data)
}


getConceptPrevalenceCountsForConceptIds <- function(dataSource = .GlobalEnv,
                                                    conceptIdsList) {
  
  sql <- "select *
          from concept_prevalence.cp_master
          where concept_id in (@concept_list);"
  if (length(conceptIdsList) > 0) {
    data <-
      renderTranslateQuerySql(
        connection = dataSource$connection,
        concept_list = conceptIdsList,
        sql = sql, 
        snakeCaseToCamelCase = TRUE
      ) %>%  
      dplyr::arrange(1)
    return(data) 
  } else {
    return(dplyr::tibble())
  }
}