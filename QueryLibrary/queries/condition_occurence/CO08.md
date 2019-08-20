<!---
Group:condition occurrence
Name:CO08 Duration of hospitalization for a conditions
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO08 :Duration of hospitalization for a conditions

## Description
Returns the the average length in days of all hospitalizations where a certain condition was reported

## Query
```sql
SELECT
  avg(hosp_no_days) AS average_hosp_duration_count
FROM
  (SELECT DISTINCT
    hosp_no_days,
    person_id,
    from_visit.visit_occurrence_id
   FROM
    (SELECT
      visit_occurrence_id,
      condition_start_date,
      condition_end_date,
      person_id
     FROM @cdm.condition_occurrence
     WHERE condition_concept_id = 31967 -- Input condition_concept_id
           AND visit_occurrence_id IS NOT NULL
    ) AS from_cond
INNER JOIN
  (SELECT
    DATEDIFF(DAY, visit_start_date, visit_end_date) + 1 AS hosp_no_days,
    visit_start_date,
    visit_occurrence_id,
    place_of_service_concept_id
   FROM @cdm.visit_occurrence v
   INNER JOIN @cdm.care_site c
   ON v.care_site_id=c.care_site_id
   WHERE place_of_service_concept_id = 8717 -- Inpatient hospital
  ) AS from_visit
ON from_cond.visit_occurrence_id = from_visit.visit_occurrence_id
 ) AS hosp_duration_count
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept identifier for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| average_hosp_duration_count | Average length in days of all hospitalization where a certain condition was reported. +1 was added for partial days (e.g. 1.5 days were counted as 2 days). |

## Example output record

| Field |  Description |
| --- | --- |
| average_hosp_duration_count | 7 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
