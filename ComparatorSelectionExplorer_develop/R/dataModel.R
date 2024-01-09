# Functions for querying data model
#

getCohortDefinitions <- function(qns) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb("select distinct
               t.cohort_definition_id,
               short_name,
               atc_flag as is_atc
             from @schema.@cohort_definition t
             where t.cohort_definition_id is not null
             --and   atc_flag in (0, 1)
             order by short_name")
}


getDbDataSourcesTable <- function(qns, reactableTable = TRUE) {
  checkmate::assertClass(qns, "QueryNamespace")
  dataSourceData <- qns$queryDb("select
              cdm_source_abbreviation,
              cdm_holder,
              source_description,
              cdm_version,
              vocabulary_version,
              source_release_date
      from @schema.@cdm_source_info t")

  if (reactableTable) {
    colnames(dataSourceData) <- SqlRender::camelCaseToTitleCase(colnames(dataSourceData))
    rt <- reactable::reactable(
      data = dataSourceData,
      columns = list(
        "Source Description" = reactable::colDef(
          minWidth = 300)),
      defaultPageSize = 5
    )

    return(rt)
  }

  return(dataSourceData)
}

#' Get data for covariates that occur on the same day as the exposure event
#'
#'
getCoOccurenceTableData <- function(qns,
                                    databaseIds,
                                    prevInputHighMax,
                                    prevInputHighMin,
                                    prevInputLowMax,
                                    prevInputLowMin,
                                    cohortDefinitionId1,
                                    cohortDefinitionId2) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb(
    sql = "
            with means_cte as (
              	select
              	    c1.database_id,
              		@cohortDefinitionId1 as cohort_definition_id_1,
              		@cohortDefinitionId2 as cohort_definition_id_2,
              		case
              			when c1.covariate_type is null then c2.covariate_type
              			when c2.covariate_type is null then c1.covariate_type
              			else c1.covariate_type
              		end as covariate_type,
              		case
              			when c1.covariate_id is null then c2.covariate_id
              			when c2.covariate_id is null then c1.covariate_id
              			else c1.covariate_id
              		end as covariate_id,
              		case
              			when c1.covariate_name is null then c2.covariate_name
              			when c2.covariate_name is null then c1.covariate_name
              			else c1.covariate_name
              		end as covariate_short_name,
              		case
              			when c1.covariate_mean is null then 0.0
              			else c1.covariate_mean
              		end as mean_1,
              		case
              			when c2.covariate_mean is null then 0.0
              			else c2.covariate_mean
              		end as mean_2
              	from (
              	  select t.*, covd.covariate_name, covd.covariate_type
              	  from @schema.@covariate_mean t
              	  inner join @schema.@covariate_definition covd on covd.covariate_id = t.covariate_id
              	  where t.cohort_definition_id = @cohortDefinitionId1
              	  and t.database_id IN (@database_ids)
            	  ) as c1
              	left join (
              	  select t.*, covd.covariate_name, covd.covariate_type
              	  from @schema.@covariate_mean t
              	  inner join @schema.@covariate_definition covd on covd.covariate_id = t.covariate_id
              	  where t.cohort_definition_id = @cohortDefinitionId2
              	  and t.database_id IN (@database_ids)
              	 ) as c2
              on c1.covariate_id = c2.covariate_id AND c1.database_id = c2.database_id
              WHERE (c1.covariate_type IS NULL OR c1.covariate_type = 'Co-occurrence')
              AND   (c2.covariate_type IS NULL OR c2.covariate_type = 'Co-occurrence')
            )

            select
              m.*,
              d.cdm_source_abbreviation,
              case
                  when m.mean_1 = m.mean_2 then 0.0
                  when m.mean_1 = 0.0 and m.mean_2 = 1.0 then null
                  when m.mean_1 = 1.0 and m.mean_2 = 0.0 then null
                  else (mean_1 - mean_2) / (sqrt((mean_1 * (1 - mean_1) + mean_2 * (1 - mean_2)) / 2))
              end as std_diff,
            c1.num_persons as n_1,
            c2.num_persons as n_2
            from means_cte as m
            inner join @schema.@cohort_count as c1
            on m.cohort_definition_id_1 = c1.cohort_definition_id and c1.database_id = m.database_id
            inner join @schema.@cohort_count as c2
              on m.cohort_definition_id_2 = c2.cohort_definition_id and c2.database_id = m.database_id
            inner join @schema.@cdm_source_info as d on d.database_id = m.database_id

            WHERE (
              (m.mean_1 > @prevInputHighMax AND m.mean_2 < @prevInputHighMin) OR
              (m.mean_2 > @prevInputHighMax AND m.mean_1 < @prevInputHighMin)
            ) OR (
              (m.mean_1 > @prevInputLowMax AND m.mean_2 < @prevInputLowMin) OR
              (m.mean_2 > @prevInputLowMax AND m.mean_1 < @prevInputLowMin)
            )
              ;",
    database_ids = databaseIds,
    prevInputHighMax = prevInputHighMax,
    prevInputHighMin = prevInputHighMin,
    prevInputLowMax = prevInputLowMax,
    prevInputLowMin = prevInputLowMin,
    cohortDefinitionId1 = cohortDefinitionId1,
    cohortDefinitionId2 = cohortDefinitionId2)
}

getCohortDefinitionsTable <- function(qns, databaseId, counts = TRUE) {
  qns$queryDb(
    sql = "select distinct
               t.cohort_definition_id,
               short_name,
               atc_flag as is_atc,
               c.num_persons,
               c.database_id
             from @schema.@cohort_definition t
             inner join @schema.@cohort_count c ON c.cohort_definition_id = t.cohort_definition_id
             where t.cohort_definition_id is not null
             -- and   atc_flag in (0, 1)
             and c.database_id IN (@database_id)
             order by short_name",
    database_id = databaseId,
    counts = counts
  )
}


getPairwiseCovariateData <- function(qns, databaseId, cohortDefinitionId1, cohortDefinitionId2) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb(
    sql = "with means as (
              	select
              		@cohortDefinitionId1 as cohort_definition_id_1,
              		@cohortDefinitionId2 as cohort_definition_id_2,
              		case
              			when c1.covariate_type is null then c2.covariate_type
              			when c2.covariate_type is null then c1.covariate_type
              			else c1.covariate_type
              		end as covariate_type,
              		case
              			when c1.covariate_id is null then c2.covariate_id
              			when c2.covariate_id is null then c1.covariate_id
              			else c1.covariate_id
              		end as covariate_id,
              		case
              			when c1.covariate_name is null then c2.covariate_name
              			when c2.covariate_name is null then c1.covariate_name
              			else c1.covariate_name
              		end as covariate_short_name,
              		case
              			when c1.covariate_mean is null then 0.0
              			else c1.covariate_mean
              		end as mean_1,
              		case
              			when c2.covariate_mean is null then 0.0
              			else c2.covariate_mean
              		end as mean_2
              	from (
              	  select t.*, covd.covariate_name, covd.covariate_type from @schema.@covariate_mean t
              	  inner join @schema.@covariate_definition covd on covd.covariate_id = t.covariate_id
              	  where t.cohort_definition_id = @cohortDefinitionId1
              	  and t.database_id = @database_id
            	  ) as c1
              	full join (
              	  select t.*, covd.covariate_name, covd.covariate_type from @schema.@covariate_mean t
              	  inner join @schema.@covariate_definition covd on covd.covariate_id = t.covariate_id
              	   where t.cohort_definition_id = @cohortDefinitionId2
              	   and t.database_id = @database_id
              	 ) as c2
              on c1.covariate_id = c2.covariate_id)
              select
              	m.*,
              	case
              		when m.mean_1 = m.mean_2 then 0.0
              		when m.mean_1 = 0.0 and m.mean_2 = 1.0 then null
              		when m.mean_1 = 1.0 and m.mean_2 = 0.0 then null
              		else (mean_1 - mean_2) / (sqrt((mean_1 * (1 - mean_1) + mean_2 * (1 - mean_2)) / 2))
              	end as std_diff,
              c1.num_persons as n_1,
              c2.num_persons as n_2
              from means as m
              join @schema.@cohort_count as c1
              	on m.cohort_definition_id_1 = c1.cohort_definition_id and c1.database_id = @database_id
              join @schema.@cohort_count as c2
              	on m.cohort_definition_id_2 = c2.cohort_definition_id and c2.database_id = @database_id
              ;",
    database_id = databaseId,
    cohortDefinitionId1 = cohortDefinitionId1,
    cohortDefinitionId2 = cohortDefinitionId2)
}


getCohortSimilarityScores <- function(qns, targetCohortId) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb(
    sql = "
            select distinct
               csi.database_id,
               csi.cdm_source_abbreviation,
               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN t.cohort_definition_id_2
                  ELSE t.cohort_definition_id_1
               END as cohort_definition_id_2,

               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN cd2.atc_flag
                  ELSE cd.atc_flag
               END as is_atc_2,

               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN cd2.short_name
                  ELSE cd.short_name
               END as short_name,
               cosine_similarity,
               atc.atc_4_related,
               atc.atc_3_related,
               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN ec.num_persons
                  ELSE ec2.num_persons
               END as num_persons
             from @schema.@cosine_similarity_score  t
             inner join @schema.@cohort_count ec ON ec.cohort_definition_id = t.cohort_definition_id_2 and ec.database_id = t.database_id
             inner join @schema.@cohort_count ec2 ON ec2.cohort_definition_id = t.cohort_definition_id_1 and ec2.database_id = t.database_id
             inner join @schema.@cdm_source_info csi ON csi.database_id = t.database_id
             inner join @schema.@cohort_definition cd ON cd.cohort_definition_id = t.cohort_definition_id_1
             inner join @schema.@cohort_definition cd2 ON cd2.cohort_definition_id = t.cohort_definition_id_2
             left join @schema.@atc_level atc on (t.cohort_definition_id_1 = atc.cohort_definition_id_1 and t.cohort_definition_id_2 = atc.cohort_definition_id_2) or (t.cohort_definition_id_2 = atc.cohort_definition_id_1 and t.cohort_definition_id_1 = atc.cohort_definition_id_2)
             where (t.cohort_definition_id_1 = @targetCohortId or t.cohort_definition_id_2 = @targetCohortId)
             and t.covariate_type = 'average'
           ",
    targetCohortId = targetCohortId)
}


getDatabaseSimilarityScores <- function(qns, targetCohortId, databaseIds) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb(
    sql = "
            select distinct
               d.cdm_source_abbreviation,
               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN t.cohort_definition_id_2
                  ELSE t.cohort_definition_id_1
               END as cohort_definition_id_2,

               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN cd2.atc_flag
                  ELSE cd.atc_flag
               END as is_atc_2,

               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN cd2.short_name
                  ELSE cd.short_name
               END as short_name,
               cosine_similarity,
               CASE
                  WHEN t.cohort_definition_id_1 = @targetCohortId THEN ec.num_persons
                  ELSE ec2.num_persons
               END as num_persons,
               t.covariate_type
             from @schema.@cosine_similarity_score t
             inner join @schema.@cohort_count ec ON ec.cohort_definition_id = t.cohort_definition_id_2 and ec.database_id = t.database_id
             inner join @schema.@cohort_count ec2 ON ec2.cohort_definition_id = t.cohort_definition_id_1 and ec2.database_id = t.database_id
             inner join @schema.@cohort_definition cd ON cd.cohort_definition_id = t.cohort_definition_id_1
             inner join @schema.@cohort_definition cd2 ON cd2.cohort_definition_id = t.cohort_definition_id_2
             inner join @schema.@cdm_source_info d ON t.database_id = d.database_id
             where (t.cohort_definition_id_1 = @targetCohortId or t.cohort_definition_id_2 = @targetCohortId)
             and t.database_id IN (@database_ids)
           ",
    targetCohortId = targetCohortId,
    database_ids = databaseIds)
}


getDbCosineSimilarityTable <- function(qns, targetCohortId, comparatorCohortId, databaseId, returnReactable = FALSE) {
  checkmate::assertClass(qns, "QueryNamespace")
  sql <- "SELECT covariate_type, cosine_similarity FROM @schema.@cosine_similarity_score
    WHERE database_id = @database_id
    AND cohort_definition_id_1 in (@target, @comparator)
    AND cohort_definition_id_2 in (@target, @comparator)
    "
    detailData <- qns$queryDb(sql,
                              database_id = databaseId,
                              target = targetCohortId,
                              comparator = comparatorCohortId)


    detailData <- detailData %>%
      dplyr::filter(!covariateType %in% c('Co-occurrence')) %>%
      dplyr::mutate(
        covariateType = factor(
          covariateType,
          levels = c("Demographics",
                     "Presentation",
                     "Medical history",
                     "prior meds",
                     "visit context",
                     "average"))) %>%
      dplyr::arrange(covariateType)

  if (returnReactable) {
    rt <- reactable::reactable(
      data = detailData,
      columns = list(
        "covariateType" = reactable::colDef(
          name = "Covariate Domain"),
        "cosineSimilarity" = reactable::colDef(
          name = "Cosine Similarity",
          cell = function(value) { sprintf(fmtSim, value) }
        )
      )
    )
    return(rt)
  }
}


getDatabaseSources <- function(qns) {
  checkmate::assertClass(qns, "QueryNamespace")
  qns$queryDb(sql = "select distinct * from @schema.@cdm_source_info t")
}
