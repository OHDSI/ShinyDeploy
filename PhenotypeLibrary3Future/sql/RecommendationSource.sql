with list as (
  select concept_id, concept_name, domain_id, vocabulary_id, standard_concept
  from @vocabulary_database_schema.concept
  where concept_id in (@source_list)
)
select distinct c2.concept_id, c2.concept_name, c2.domain_id, c2.vocabulary_id, c2.standard_concept, cp2.rc as record_count, cp2.dbc as database_count,
                cp2.drc as descendant_record_count, cp2.ddbc as descendant_database_count, c.concept_id as source_concept_id, c.concept_name as source_concept_name,
                c.vocabulary_id as source_vocabulary_id, c.concept_code as source_concept_code, cp.rc as source_record_count, cp.dbc as source_database_count
from list l
join concept_prevalence.recommender_set r on l.concept_id = r.source_id
join @vocabulary_database_schema.concept c on c.concept_id = r.concept_id and c.standard_concept is null
join concept_prevalence.cp_master cp on cp.concept_id = c.concept_id
join @vocabulary_database_schema.concept_relationship cr on cr.concept_id_1 = c.concept_id and cr.relationship_id in ('Maps to','Maps to value')
join @vocabulary_database_schema.concept c2 on c2.concept_id = cr.concept_id_2 and c2.standard_concept = 'S'
join concept_prevalence.cp_master cp2 on cp2.concept_id = c2.concept_id
left join concept_prevalence.recommended_blacklist rb on c2.concept_id = rb.concept_id
where rb.concept_id is null
and not exists (select 1 from list l2
                          join @vocabulary_database_schema.concept_relationship cr1 on l2.concept_id = cr1.concept_id_2 and cr1.relationship_id = 'Maps to'
                          where cr1.concept_id_1 = r.concept_id )
order by cp.rc desc, cp.dbc desc
;