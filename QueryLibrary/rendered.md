<!---
Group:condition era
Name:CE02 Age/gender of patients with condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# CE02: Age/gender of patients with condition

## Description
List of patient counts of specific age and gender for specific medical condition

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- | 
| concept_name | Closed fracture of hip |  Yes |  Concept ID=4230399 |


## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
WITH hip_fracture AS (
SELECT DISTINCT ca.descendant_concept_id 
  FROM cdm.concept c 
  JOIN cdm.concept_ancestor ca
    ON ca.ancestor_concept_id = c.concept_id 
 WHERE c.concept_code = '359817006'  
)
SELECT gender, 
       age, 
       COUNT(*) AS num_patients 
  FROM (
SELECT DISTINCT p.person_id, 
                c.concept_name  AS gender, 
                YEAR(ce.condition_era_start_date) - p.year_of_birth AS age 
  FROM cdm.condition_era ce 
  JOIN hip_fracture hf  
    ON hf.descendant_concept_id = ce.condition_concept_id 
  JOIN cdm.person p
    ON p.person_id = ce.person_id 
  JOIN cdm.concept c 
    ON c.concept_id = p.gender_concept_id 
       ) TMP
 GROUP BY gender, age 
 ORDER BY gender, age;
```

## Output



|  Field |  Description |
| --- | --- |
| gender | Patients gender, i.e. MALE, FEMALE |
| age | The year of birth of the person. For data sources with date of birth, the year is extracted. For data sources where the year of birth is not available, the approximate year of birth is derived based on any age group categorization available. |
| num_patients | Number of patients for specific gender and age and selected condition |

## Example output record

|  Field |  Description |
| --- | --- |
| gender |  FEMALE |
| age |  16 |
| num_patients |  22 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
