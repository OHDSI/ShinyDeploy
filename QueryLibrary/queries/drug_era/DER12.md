<!---
Group:drug era
Name:DER12 Counts of drug types
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER12: Counts of drug types

## Description
This query is used to count the drug types (drug_type_concept_id) across all drug era records. The input to the query is a value (or a comma-separated list of values) of a drug_type_concept_id. If the input is ommitted, all possible values are summarized.

## Query
```sql
select count(1) as cntRecs, r.drug_type_concept_id
from @cdm.drug_exposure r
group by r.drug_type_concept_id;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| cntrecs |  Count of drug types |
| drug_type_concept_id | Drug type standardized unique identifier |

## Example output record

|  Field |  Value |
| --- | --- |
| cntrecs | 6544017 |
| drug_type_concept_id | 38000179 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
