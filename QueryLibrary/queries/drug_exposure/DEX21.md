<!---
Group:drug exposure
Name:DEX21 How many people have a diagnosis of a contraindication for the drug they are taking?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX21: How many people have a diagnosis of a contraindication for the drug they are taking?

## Description

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue  ;


```sql
WITH con_rel AS (
SELECT r1.concept_id_1,
       r2.concept_id_2
  FROM @vocab.concept_relationship AS r1
  JOIN @vocab.concept_relationship r2
    ON r2.concept_id_1    = r1.concept_id_2
 WHERE r1.relationship_id = 'Has CI'
   AND r2.relationship_id = 'Ind/CI - SNOMED'
)
SELECT COUNT(DISTINCT d.person_id) AS count_value
  FROM con_rel cr
  JOIN @cdm.drug_exposure d
    ON cr.concept_id_1 = d.drug_concept_id
  JOIN @cdm.condition_occurrence c
    ON cr.concept_id_2  = c.condition_concept_id
   AND d.person_id      = c.person_id
 WHERE d.drug_exposure_start_date >= c.condition_start_date
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|   |   |   |  |

## Output

|  Field |  Description |
| --- | --- |
| count_value | The number of peaople with a contraindication |

## Example output record

|  Field |  Description |
| --- | --- |
| count_value | 0 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
