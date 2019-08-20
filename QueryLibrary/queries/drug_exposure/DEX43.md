<!---
Group:drug exposure
Name:DEX43 Counts of drug exposure records per person, stratified by drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX43: Counts of drug exposure records per person, stratified by drug

## Description
This query is used to count the number of drug exposure records for all exposed persons stratified by drug
(drug_concept_id). The input to the query is a value (or a comma-separated list of values) of a drug_concept_id.
If the input is omitted, all existing values are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  drug_concept_id,
  person_id,
  COUNT(1) AS drug_exposure_count
FROM @cdm.drug_exposure
      -- Filter by input list of drug_concept_id
WHERE drug_concept_id in (906805, 1517070, 19010522)
GROUP BY person_id, drug_concept_id
ORDER BY drug_concept_id, person_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes |  


## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The concept ID of the drug. |
| person_id | The ID of the person. |
| drug_exposure_count | The number of drug exposures of the given drug for the person. |


## Example output record

|  Field |  Description |
| --- | --- |
| drug_concept_id | 906805  |
| person_id |  29 |
| count | 5  |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
