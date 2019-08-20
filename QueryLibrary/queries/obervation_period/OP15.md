<!---
Group:observation period
Name:OP15 Counts of age, stratified by gender
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP15: Counts of age, stratified by gender

## Description
This query is used to count the age across all observation records stratified by gender (gender_concept_id). The age value is defined by the earliest observation date. Age is summarized for all existing gender_concept_id values.

## Query
```sql
SELECT
  age,
  gender,
  COUNT(*) AS num_people
FROM
  ( SELECT
      w.person_id,
      ISNULL( concept_name, 'MISSING' )                     AS gender,
      YEAR(first_observation_date ) - year_of_birth         AS age
    FROM
      ( SELECT
          person_id,
          MIN( observation_period_start_date ) AS first_observation_date
        FROM @cdm.observation_period
        GROUP BY person_id
      ) AS w
     INNER JOIN @cdm.person
     ON w.person_id = person.person_id
     LEFT OUTER JOIN @vocab.concept
     ON person.gender_concept_id = concept.concept_id
     WHERE YEAR(first_observation_date) - year_of_birth >= 0
  ) AS z
GROUP BY age, gender
ORDER BY age, gender;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| age | Age across within observation |
| gender | Gender concept name stratification |
| num_people | Number of person within group |

## Example output record

| Field |  Description |
| --- | --- |
| age |  1 |
| gender |  MALE |
| num_people |  22501 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
