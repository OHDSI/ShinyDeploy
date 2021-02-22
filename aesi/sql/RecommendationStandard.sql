with list as (
  select concept_id, concept_name, domain_id, vocabulary_id, standard_concept
  from @vocabulary_database_schema.concept
  where concept_id in (@source_list)
),
recommendations as (
  --list all included concepts
  select i.concept_id, 'Included' as concept_in_set
  from list i
  union
  --find not included concepts found by orphan check via standards
  select rc1.concept_id, 'Not included - recommended via standard' as concept_in_set
  from list i
         join @results_database_schema.recommender_set rc1 on i.concept_id = rc1.source_id
         join @vocabulary_database_schema.concept c1 on rc1.concept_id = c1.concept_id and c1.standard_concept = 'S'
  union
  select cr1.concept_id_2, 'Not included - recommended via source' as concept_in_set
  from list i
         join @results_database_schema.recommender_set rc1 on i.concept_id = rc1.source_id
         join @vocabulary_database_schema.concept c1 on rc1.concept_id = c1.concept_id and c1.standard_concept is null
         join @vocabulary_database_schema.concept_relationship cr1 on c1.concept_id = cr1.concept_id_1 and cr1.relationship_id in ('Maps to', 'Maps to value')
             -- excluding those sources that already have one standard counterpart in our input list
        left join (select *
                     from list l
                     join @vocabulary_database_schema.concept_relationship cr2 on l.concept_id = cr2.concept_id_2 and cr2.relationship_id = 'Maps to'
                  ) a on a.concept_id_1 = cr1.concept_id_1
              where a.concept_id_2 is null
  union
  -- find all not included parents
  select ca.ancestor_concept_id, 'Not included - parent' as concept_in_set
  from list i
         join @vocabulary_database_schema.concept_ancestor ca
              on i.concept_id = ca.descendant_concept_id and ca.min_levels_of_separation = 1
  union
  -- find all not included children
  select ca.descendant_concept_id, 'Not included - descendant' as concept_in_set
  from list i
         join @vocabulary_database_schema.concept_ancestor ca on i.concept_id = ca.ancestor_concept_id
)
select c.concept_id, c.concept_name,  c.vocabulary_id, c.domain_id, c.standard_concept, concept_in_set, coalesce(cp.rc,0) as record_count,
       coalesce(cp.dbc,0) as database_count, coalesce(cp.drc,0) as descendant_record_count, coalesce(cp.ddbc,0) as descendant_database_count
from recommendations r
join @vocabulary_database_schema.concept c on c.concept_id = r.concept_id
left join @results_database_schema.cp_master cp on r.concept_id = cp.concept_id
left join @results_database_schema.recommended_blacklist rb on r.concept_id = rb.concept_id
where (rb.concept_id is null 
and  not exists   (select 1 from list l
                   join @vocabulary_database_schema.concept_relationship cr1 on l.concept_id = cr1.concept_id_2 and cr1.relationship_id = 'Maps to'
                    where cr1.concept_id_1 = r.concept_id) or concept_in_set = 'Included')
order by coalesce(cp.rc,0) desc, coalesce(cp.dbc,0) desc, coalesce(cp.drc,0) desc, coalesce(cp.ddbc,0) desc
;