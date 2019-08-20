<!---
Group:drug exposure
Name:DEX28 Counts of drug types
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX28: Counts of drug types

## Description
This query is used to count the drug type concepts (drug_type_concept_id) across all drug exposure records. The input to the query is a value (or a comma-separated list of values) of a drug_type_concept_id. If the input is omitted, all possible values are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  drug_type_concept_id AS drug_type_concept_id,
  count(*)             AS exposure_occurrence_count
FROM @cdm.drug_exposure
WHERE
  drug_concept_id IN (SELECT DISTINCT drug_concept_id FROM @cdm.drug_era)
  AND drug_type_concept_id IN (38000175, 38000180, 43542356)
GROUP BY drug_type_concept_id
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_type_concept_id | 38000175, 38000180 | Yes |

## Output

|  Field |  Description |
| --- | --- |
| drug_type_concept_id | The type of drug. |
| exposure_occurrence_count | The number of individual drug exposure occurrences used to construct the drug era. |


## Example output record

|  Field | Sample |
| --- | --- |
| drug_type_concept_id | 43542356 (Physician administered drug) |
| exposure_occurrence_count | 34583  |  

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/drug_era
