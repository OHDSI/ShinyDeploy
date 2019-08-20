<!---
Group:condition occurrence
Name:CO05 Breakout of condition by gender, age
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO05: Breakout of condition by gender, age

## Description
Returns the distribution of condition breakouts per gender and age.

## Query
```sql
SELECT
  concept_name AS gender,
  age,
  gender_age_freq
FROM 
  (SELECT
    gender_concept_id,
    age,
    COUNT(1) gender_age_freq
   FROM 
    (SELECT
      year_of_birth,
      month_of_birth,
      day_of_birth,
      gender_concept_id,
      condition_start_date,
      YEAR(condition_start_date) - year_of_birth    AS age
     FROM 
      (SELECT
        person_id,
        condition_start_date
       FROM @cdm.condition_occurrence
       WHERE condition_concept_id = 31967 -- Input condition_concept_id 
             AND person_id IS NOT NULL
      ) AS from_cond
    LEFT JOIN @cdm.person from_person 
      ON from_cond.person_id=from_person.person_id 
  ) AS gender_count
  GROUP BY
    gender_concept_id,
    age
  ) AS gender_id_age_count
LEFT JOIN @vocab.concept concept_list 
  ON gender_id_age_count.gender_concept_id=concept_list.concept_id
ORDER BY gender_age_freq DESC;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept ID for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| gender | A person's gender |
| age | A person's age in years |
| gender_age_freq | The frequency of a condition breakout for person gender at a certain age. |

## Example output record

|  Field |  Description |
| --- | --- |
| gender | Female |
| age | 50 |
| gender_age_freq | 3136 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
