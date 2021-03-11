--------------------------------------------------------------------
-- COHORT TABLE: Provides a list of all cohorts and strata used
-- in the study. The cohort_id is used in the results tables
-- and this table details the target/strata/cohort_type combinations
-- for each cohort_id entry
--------------------------------------------------------------------
SELECT *
FROM charybdis.cohort

--------------------------------------------------------------------
-- Example: Use the COHORT table to identify the different strata
-- cohorts used in the study. The "cohort_type" column is used
-- to determine if the cohort_id reprents the target with strata (TwS)
-- or without the strata (TwoS):
--    "TwS" == Target with Strata 
--    "TwoS" == Target without Strata
--------------------------------------------------------------------
select distinct strata_id, strata_name, cohort_type
from charybdis.cohort
order by strata_id, cohort_type
;

--------------------------------------------------------------------
-- Example: Use the COHORT table to subset to a strata of interest
--------------------------------------------------------------------
select * 
from charybdis.cohort
where strata_id = 120 -- "Prevalent Asthma without COPD"
  and cohort_type = 'TwS' -- Target with Strata
;

--------------------------------------------------------------------
-- Example: Get the cohort counts for a strata of interest
--------------------------------------------------------------------
select * 
from charybdis.cohort c 
inner join charybdis.cohort_count cnt ON c.cohort_id = cnt.cohort_id
where c.strata_id = 120 -- "with Prevalent Asthma without COPD"
  and c.cohort_type = 'TwS' -- Target with Strata
;

--------------------------------------------------------------------
-- Example: Get the cohort characterization values for the strata
-- of interest with mean > 1%. 
--------------------------------------------------------------------
select * 
from charybdis.cohort c 
inner join charybdis.covariate_value cv ON c.cohort_id = cv.cohort_id
where c.strata_id = 120 -- "with Prevalent Asthma without COPD"
  and c.cohort_type = 'TwS' -- "TwS == Target with Strata, TwoS == Target without Strata"
  and cv.mean > 0.01
;



