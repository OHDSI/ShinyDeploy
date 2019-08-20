<!---
Group:condition occurrence
Name:CO14 Counts of condition types
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO14: Counts of condition types

## Description
This query is used to count the condition type concepts (condition_type_concept_id, in the CDM V2 condition_occurrence_type) across all condition occurrence records. The input to the query is a value of a condition_type_concept_id.

## Query
```sql
SELECT condition_type_freq, condition_type_concept_id, concept_name
FROM (
    SELECT condition_type_concept_id, 
           count(*) AS condition_type_freq
    FROM @cdm.condition_occurrence
    WHERE condition_concept_id = 372409
    GROUP BY condition_type_concept_id
    ) AS condition_type_count
LEFT JOIN (
    SELECT concept_id, 
           concept_name
    FROM @vocab.concept
    ) AS type_concept 
    ON condition_type_count.condition_type_concept_id=type_concept.concept_id
ORDER BY condition_type_freq;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 31967 | Yes | Condition concept identifier for 'Nausea' |

## Output

|  Field |  Description |
| --- | --- |
| condition_type_freq | Frequency of a specific condition_type |
| condition_type_concept_id | Unique ID for condition_type |
| concept_name |  Description of the condition's data source |

## Example output record

|  Field |  Description |
| --- | --- |
| condition_type_freq | 4735 |
| condition_type_concept_id | 38000235 |
| concept_name | Outpatient header - 6th position |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
