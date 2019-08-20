<!---
Group:drug exposure
Name:DEX25 Counts of drug records
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX25: Counts of drug records

## Description
| This query is used to count drugs (drug_concept_id) across all drug exposure records. The input to the query is a value (or a comma-separated list of values) of a drug_concept_id. If the input is omitted, all possible values are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
SELECT COUNT(*) as exposure_occurrence_count  
  FROM @cdm.drug_exposure
 WHERE drug_concept_id IN (906805, 1517070, 19010522);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes | Metoclopramid, Desmopressin, Cyclosprin |

## Output

|  Field |  Description |
| --- | --- |
| exposure_occurrence_count | The number of individual drug exposure occurrences used to construct the drug era. |

## Example output record

|  Field |  Value |
| --- | --- |
| exposure_occurrence_count |  88318 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
