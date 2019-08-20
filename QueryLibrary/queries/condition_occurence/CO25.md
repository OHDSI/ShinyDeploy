<!---
Group:condition occurrence
Name:CO25 Counts of condition records per person, stratified by condition.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO25: Counts of condition records per person, stratified by condition.

## Description
Count number of condition occurrences per person stratified by condition.

## Query
```sql
SELECT condition_concept_id, 
       num_of_occurrences,
       count(*) num_of_patients
FROM 
  (SELECT condition_concept_id, 
          person_id, 
          count(*) num_of_occurrences
    FROM @cdm.condition_occurrence condition
    WHERE condition.condition_concept_id = 200219 -- Input condition
    GROUP BY person_id, condition_concept_id
  ) AS counts_condition_person
GROUP BY condition_concept_id, num_of_occurrences;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 200219 | Yes | Condition concept identifier for 'Abdominal pain' |

## Output

|  Field |  Description |
| --- | --- |
| condition_concept_id | Condition concept identifier |
| num_occurrences | Number of condition occurrences |
| num_of_patients | Number of patients with num_occurrences |

## Example output record

|  Field |  Description |
| --- | --- |
| condition_concept_id |  200219 |
| num_occurrences |  10 |
| num_of_patients |  3681 |



## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
