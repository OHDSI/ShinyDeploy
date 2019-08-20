<!---
Group:drug era
Name:DER14 Counts of number of distinct drugs persons take
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER14: Counts of number of distinct drugs persons take

## Description
This query is used to count the number of different distinct drugs (drug_concept_id) of all exposed persons. The input to the query is a value (or a comma-separated list of values) for a number of concepts. If the input is ommitted, all possible values are summarized.

## Query
```sql
SELECT count(
distinct t.drug_concept_id) AS drug_count, t.person_id
FROM @cdm.drug_era t
group by t.person_id
having count(
distinct t.drug_concept_id)
in (3, 4);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_type_concept_id | 3, 4 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| Drug_count | Counts of number of distinct drugs |
| Person_id | A foreign key identifier to the person who is subjected to the drug. The demographic details of that person are stored in the person table. |

## Example output record

|  Field |  Description |
| --- | --- |
| Drug_count | 3 |
| Person_id | 17 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
