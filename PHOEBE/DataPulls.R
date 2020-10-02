
# function that suggest one recommendation based on input string and domain
loadDataFromDB <- function(connPool,SourceDomain, SourceString) {

sql <-paste0("DROP TABLE IF EXISTS select_1;

create table select_1
as
select distinct u.*
from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs on cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(u.concept_name) ~* '", SourceString,"'
;

insert into select_1
select distinct u.*
  from @target_database_schema.universe u -- in case standard_concept changed over time
join @vocabulary_database_schema.concept_synonym cs ON cs.concept_id = u.concept_id
where u.standard_concept = 'S'
and (lower(u.domain_id) = lower('", SourceDomain,"') or 
-- case for no domain input
    ('", SourceDomain,"'  = '' and u.domain_id in ('Condition','Procedure','Drug','Measurement','Observation'))
     )
and lower(cs.concept_synonym_name) ~* '", SourceString,"'
and u.concept_id not in (select s1.concept_id from select_1 s1)
;

delete
from select_1 ss
where ss.concept_id in (
  select s.concept_id from select_1 s
  join @vocabulary_database_schema.concept_ancestor ca
  on descendant_concept_id = s.concept_id and min_levels_of_separation > 0
  and ancestor_concept_id in (select s1.concept_id from select_1 s1)
);
"
)
  sql <- SqlRender::render(sql, target_database_schema = "results", vocabulary_database_schema = "vocabulary")
  sql <- SqlRender::translate(sql, targetDialect = Sys.getenv("shinydbDbms"))
  executeSql(connPool, sql)
  if (SourceDomain == '') {
  data <- querySql(connPool,
                'select concept_id,concept_name,vocabulary_id,domain_id,standard_concept,rc,dbc,drc,ddbc 
                from (
                select s.*, rank() over (partition by domain_id order by drc desc)
                from select_1 s) a
                where rank=1;')
  }
  else {
  data <- querySql(connPool,'select * from select_1 order by drc desc limit 1;')
  }
  DatabaseConnector::disconnect(connPool)
  return(data)
}
