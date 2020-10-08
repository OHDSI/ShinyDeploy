
# function that suggest one recommendation based on input string and domain
loadDataFromDB <- function(connPool,SourceDomain, SourceString) {

  if (SourceDomain == '') {
    sql <-paste0(
      "with ss as (  
select distinct u.*
from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs on cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(u.concept_name) ~* '", SourceString,"'

union

select distinct u.*
  from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs ON cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(cs.concept_synonym_name) ~* '", SourceString,"'
--and u.concept_id not in (select s1.concept_id from select_1 s1)
),
sss as (
select ss.*, rank() over (partition by domain_id order by drc desc)
from ss
where not exists
(select 1 from @vocabulary_database_schema.concept_ancestor ca 
 join ss s2 on ca.descendant_concept_id = s2.concept_id and ca.min_levels_of_separation > 0
 and concept_id = ca.ancestor_concept_id)
)
select concept_id,concept_name,vocabulary_id,domain_id,standard_concept,rc,dbc,drc,ddbc 
                from sss
                where rank=1;")
  }
  else {
    sql <-paste0(
      "with ss as (  
select distinct u.*
from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs on cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(u.concept_name) ~* '", SourceString,"'

union

select distinct u.*
  from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs ON cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(cs.concept_synonym_name) ~* '", SourceString,"'
--and u.concept_id not in (select s1.concept_id from select_1 s1)
)
select *
from ss
where not exists
(select 1 from @vocabulary_database_schema.concept_ancestor ca 
 join ss s2 on ca.descendant_concept_id = s2.concept_id and ca.min_levels_of_separation > 0
 and concept_id = ca.ancestor_concept_id)
 order by drc desc limit 1;
")
  }
  
  sql <- SqlRender::render(sql, target_database_schema = "results", vocabulary_database_schema = "vocabulary")
  data <- DatabaseConnector::dbGetQuery(connPool, sql)
  DatabaseConnector::disconnect(connPool)
  return(data)
}
