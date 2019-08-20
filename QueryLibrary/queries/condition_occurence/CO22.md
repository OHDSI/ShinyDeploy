<!---
Group:condition occurrence
Name:CO22 Counts of conditions, stratified by condition type
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO22: Counts of conditions, stratified by condition type

## Description
This query is used to count conditions across all condition occurrence records stratified by condition occurrence type

## Query
```sql
SELECT 
  concept_name                      AS condition_occurrence_type, 
  condition_type_concept_id, 
  COUNT(*)                          AS occurrence_type_count
FROM @cdm.condition_occurrence
INNER JOIN @vocab.concept 
ON concept_id = condition_type_concept_id
GROUP BY concept_name, condition_type_concept_id;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| condition_occurrence_type | Name of the condition occurrence type |
| condition_type_concept_id | Concept identifier for condition type |
| occurrence_types_count | Number of occurrence types |

## Example output record

|  Field |  Description |
| --- | --- |
| condition_occurrence_type |  EHR Chief Complaint |
| condition_type_concept_id |  42894222 |
| occurrence_types_count |  65445068 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
