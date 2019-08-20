<!---
Group:drug era
Name:DER16 Counts of drug era records per person
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER16: Counts of drug era records per person

## Description
This query is used to count the number of drug era records (drug_era_id) for all persons. The input to the query is a value (or a comma-separated list of values) for a number of records per person. If the input is ommitted, all possible values are summarized.

## Query
```sql
SELECT
  count(1) AS s_count,
  t.person_id
FROM @cdm.drug_era t
group by t.person_id
having count(1) in (3, 4);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_type_concept_id | 3, 4 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| s_count | number of drug era records for all persons. |
| person_id | Person unique identifier |

## Example output record

|  Field |  Description |
| --- | --- |
| s_count |  4 |
| person_id | 10015532 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
