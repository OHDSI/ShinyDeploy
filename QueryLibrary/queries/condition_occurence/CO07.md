<!---
Group:condition occurrence
Name:CO07 Frequency of hospitalized for a condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO07 :Frequency of hospitalized for a condition

## Description
Returns the distribution of number of times a person has been hospitalized where a certain condition was reported.

## Query
```sql
SELECT
  number_of_hospitalizations,
  count(*)          AS persons_freq
FROM 
  (SELECT
    person_id,
    COUNT(*)        AS number_of_hospitalizations
   FROM 
    (SELECT 
      DISTINCT
        condition_era_id,
        era.person_id
     FROM 
      (SELECT
        condition_start_date,
        condition_end_date,
        from_cond.person_id
       FROM 
        (SELECT
          visit_occurrence_id,
          condition_start_date,
          condition_end_date,
          person_id
         FROM @cdm.condition_occurrence
         WHERE condition_concept_id=31967 -- Input condition_concept_id 
               AND visit_occurrence_id IS NOT NULL
        ) AS FROM_cond
       INNER JOIN @cdm.visit_occurrence FROM_visit
        ON FROM_cond.visit_occurrence_id=FROM_visit.visit_occurrence_id
       INNER JOIN @cdm.care_site cs 
        ON from_visit.care_site_id=cs.care_site_id
       WHERE  place_of_service_concept_id=8717 -- Inpatient hospital
    ) AS occurr,
    (SELECT
        condition_era_id,
        person_id,
        condition_era_start_date,
        condition_era_end_date
     FROM @cdm.condition_era
     WHERE condition_concept_id=31967 -- Input condition_concept_id 
    ) AS era
   WHERE era.person_id=occurr.person_id 
         AND era.condition_era_start_date <= occurr.condition_end_date 
         AND (era.condition_era_end_date IS NULL OR era.condition_era_end_date >= occurr.condition_start_date)
   ) AS c
  GROUP BY person_id
  ) AS hospitalizations_freq
GROUP BY number_of_hospitalizations
ORDER BY persons_freq DESC;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept identifier for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| number_of_hospitalizations | Number of times a person was reported to be hospitalized with a certain condition. |
| persons_freq | Number of persons which were reported to have a certain number of hospilizations with a certain condition. |

## Example output record

| Field |  Description |
| --- | --- |
| number_of_hospitalizations | 2 |
| persons_freq | 177 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
