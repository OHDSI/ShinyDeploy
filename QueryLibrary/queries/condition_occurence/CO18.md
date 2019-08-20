<!---
Group:condition occurrence
Name:CO18 Counts of condition occurrence records
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO18: Counts of condition occurrence records

## Description
This query is used to count the number of condition occurrence records (condition_occurrence_id) for all persons. The input to the query is a condition concept identifier.

## Query
```sql
SELECT
  COUNT(*) AS number_condition_occurrences
FROM @cdm.condition_occurrence
WHERE condition_concept_id = 31967;
```

## Input

|  Parameter |  Example |  Mandatory | Notes|
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept identifier for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| number_of_condition_occurrences | The number of the condition occurrences for all persons |

## Example output record

|  Field |  Description |
| --- | --- |
| number_of_condition_occurrences | 142100 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
