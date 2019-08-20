<!---
Group:condition occurrence
Name:CO06 What are a person's comorbidities.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO06: What are a person's comorbidities.

## Description
Returns all coexisting conditions for a given person as defined in the condition_era table.

## Query
```sql
SELECT DISTINCT
  CASE 
    WHEN concept_name_1>concept_name_2 THEN concept_name_1 
    ELSE concept_name_2 
  END AS condition1,
  CASE 
    WHEN concept_name_1>concept_name_2 THEN concept_name_2 
    ELSE concept_name_1 
  END AS condition2
FROM 
  (SELECT
    concept_name_1  AS concept_name_1,
    concept_name    AS concept_name_2
   FROM 
    (SELECT
      condition_concept_id_2,
      concept_name AS concept_name_1
     FROM 
      (SELECT
        table1.condition_concept_id AS condition_concept_id_1,
        table2.condition_concept_id AS condition_concept_id_2
       FROM
          (SELECT * FROM @cdm.condition_era 
           WHERE person_id = 136931019 -- Input person_id
           ) AS table1,
          (SELECT * FROM @cdm.condition_era 
           WHERE person_id = 136931019 -- Input person_id
          ) AS table2
      WHERE table2.condition_era_start_date <= table1.condition_era_end_date 
            AND (table2.condition_era_end_date IS NULL OR table2.condition_era_end_date >= table1.condition_era_start_date) 
            AND table1.condition_concept_id<>table2.condition_concept_id
      ) AS comorb
    LEFT JOIN @vocab.concept AS concept_list 
    ON comorb.condition_concept_id_1=concept_list.concept_id
    ) AS comorb2
  LEFT JOIN @vocab.concept AS concept_list 
  ON comorb2.condition_concept_id_2=concept_list.concept_id
  ) AS condition_pairs;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| person_id | 136931019 | Yes | Randomly picked person identifier |

## Output

|  Field |  Description |
| --- | --- |
| Condition 1 | Name of one condition in the comorbidity. |
| Condition 2 | Name of the other condition in the comorbidity. |

## Example output record

|  Field |  Description |
| --- | --- |
| Condition 1 | Hyperlipidemia |
| Condition 2 | Abnormal breathing |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
