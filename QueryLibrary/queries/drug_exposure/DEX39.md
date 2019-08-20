<!---
Group:drug exposure
Name:DEX39 Counts of drugs, stratified by drug type
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX39: Counts of drugs, stratified by drug type

## Description
This query is used to count drugs (drug_concept_id) across all drug exposure records stratified by drug exposure type (drug_type_concept_id, in CDM V2 drug_exposure_type).
The input to the query is a value (or a comma-separated list of values) of a drug_concept_id or a drug_type_concept_id.
If the input is omitted, all existing value combinations are summarized.

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  drug_concept_id,
  drug_type_concept_id,
  COUNT(1) AS drugs_count
FROM @cdm.drug_exposure
      -- Filter by input list of drug_concept_id
WHERE drug_concept_id IN (906805, 1517070, 19010522)
      -- Filter by input list of drug_type_concept_id
      AND drug_type_concept_id IN (38000175,38000179)
GROUP BY drug_type_concept_id, drug_concept_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes |
| list of drug_type_concept_id | 38000175,38000179 | Yes |

## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The concept ID of the drug. |
| drug_type_concept_id | The concept ID of the type of the drug. |
| drugs_count | The number of exposures of the drug |

## Example output record

|  Field |  Description |
| --- | --- |
| drug_concept_id |  906805 |
| drug_type_concept_id |  38000175 |
| drugs_count |  45 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
