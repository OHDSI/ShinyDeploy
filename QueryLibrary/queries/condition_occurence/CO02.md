<!---
Group:condition occurrence
Name:CO02 How to determine the onset of a condition.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO02: How to determine the onset of a condition.

## Description
Determines the onset of a condition in a dataset. 

## Query
```sql
SELECT
  MIN(condition_start_date) AS onset_condition_date
FROM @cdm.condition_occurrence
WHERE condition_concept_id = 23988 
      AND   EXISTS (SELECT 1 FROM @cdm.observation_period op 
                    WHERE op.person_id = condition_occurrence.person_id
                          AND  DATEADD(month,6,op.observation_period_start_date) < condition_occurrence.condition_start_date)
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_id | 23988 | Yes | Condition concept ID for 'Hemolytic anemia due to glutathione metabolism disorder' |  

## Output

|  Field |  Description |
| --- | --- |
| onset_condition_date | The date when the instance of the condition is recorded first time. | 

## Example output record

|  Field |  Description |
| --- | --- |
| onset_condition_date |  9-Jul-2003 | 


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
