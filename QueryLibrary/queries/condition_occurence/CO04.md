<!---
Group:condition occurrence
Name:CO04 Count In what place of service where condition diagnoses.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO04: Count In what place of service where condition diagnoses.

## Description
Returns the distribution of the visit place of service where the condition was reported.

## Query
```sql
SELECT
  concept_name AS place_of_service_name,
  place_freq
FROM
  (SELECT
    care_site_id,
    count(*)  AS place_freq
    FROM
      (SELECT
        care_site_id
      FROM
        (SELECT
          visit_occurrence_id
          FROM @cdm.condition_occurrence
          WHERE condition_concept_id = 31967 -- Input condition
                AND visit_occurrence_id IS NOT NULL
        ) AS from_cond
      LEFT JOIN
        (SELECT
          visit_occurrence_id,
          care_site_id
         FROM @cdm.visit_occurrence
        ) AS from_visit
      ON from_cond.visit_occurrence_id=from_visit.visit_occurrence_id
      ) AS from_cond_visit
    GROUP BY care_site_id
  ) AS place_id_count

  LEFT JOIN

  (SELECT
    concept_id,
    concept_name
   FROM @vocab.concept
  ) AS place_concept
  ON place_id_count.care_site_id=place_concept.concept_id
ORDER BY place_freq;
```

## Input
|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept ID for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| place_of_service_name | The place of service where the condition was reported. |
| place_freq | Frequency of the place of service. |

## Example output record

|  Field |  Description |
| --- | --- |
| place_of_service_name | Emergency Room |
| place_freq | 35343 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
