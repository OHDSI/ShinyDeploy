<!---
Group:drug exposure
Name:DEX40 Counts of drugs, stratified by relevant condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX40: Counts of drugs, stratified by relevant condition

## Description
This query is used to count all drugs (drug_concept_id) across all drug exposure records stratified
by condition (relevant_condition_concept_id).
The input to the query is a value (or a comma-separated list of values) of a drug_concept_id and a
relevant_condition_concept_id. If the input is omitted, all existing value combinations are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  drug_concept_id,
  condition_concept_id  AS relevant_condition_concept_id,
  COUNT(1)              AS drugs_count
FROM @cdm.drug_exposure
INNER JOIN @cdm.condition_occurrence
ON drug_exposure.visit_occurrence_id = condition_occurrence.visit_occurrence_id
   AND       drug_exposure.person_id = condition_occurrence.person_id
      -- Filter by input drug_concept_id
WHERE drug_concept_id in (906805, 1517070, 19010522)
      -- filter by input condition_concept_id
      AND condition_concept_id in (26052, 258375)
GROUP BY condition_concept_id, drug_concept_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes |  
| list of relevant_condition_concept_id | 26052, 258375 | Yes |   


## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The concept ID of the drug. |
| relevant_condition_concept_id | The concept ID of the condition that was the cause for initiation of the procedure. Note that this is not a direct reference to a specific condition record in the condition table, but rather a condition concept in the vocabulary |
| drugs_count | The number of drug exposures. |


## Example output record

|  Field |  Description |
| --- | --- |
| drug_concept_id | 1517070|  
| relevant_condition_concept_id |  26052 |
| drugs_count |  78 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
