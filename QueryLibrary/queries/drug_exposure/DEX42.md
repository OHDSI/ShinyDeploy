<!---
Group:drug exposure
Name:DEX42 Counts of genders, stratified by drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX42: Counts of genders, stratified by drug

## Description
This query is used to count all gender values (gender_concept_id) for all exposed persons
stratified by drug (drug_concept_id).
The input to the query is a value (or a comma-separated list of values) of a gender_concept_id and drug_concept_id.
If the input is omitted, all existing value combinations are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  gender_concept_id,
  drug_concept_id,
  COUNT(1) AS gender_count
FROM @cdm.drug_exposure
INNER JOIN @cdm.person
ON drug_exposure.person_id = person.person_id
      -- Filter by input list of drug_concept_id
WHERE drug_concept_id IN (906805, 1517070, 19010522)
      -- Filter by input list of gender_concept_id  
      AND gender_concept_id IN (8507, 8532)
GROUP BY drug_concept_id, gender_concept_id
ORDER BY drug_concept_id, gender_concept_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes |   
| list of gender_concept_id | 8507, 8532 | Yes | Male, Female |

## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The concept ID of the drug. |
| gender_concept_id | The concept ID of the gender. |
| Count | The number of drug exposures for the drug for people of the given gender. |

## Example output record

|  Field |  Description |
| --- | --- |
| drug_concept_id |  1517070 |
| gender_concept_id | 8507  |
| gender_count |  28 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
