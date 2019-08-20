<!---
Group:condition occurrence
Name:CO19 Counts of condition occurrence records stratified by observation month
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO19: Counts of condition occurrence records stratified by observation month

## Description
This query is used to count the condition occurrence records stratified by observation month.

## Query
```sql
SELECT 
  MONTH(condition_start_date) month_number, 
  COUNT(*) AS number_of_conditions_in_month
FROM @cdm.condition_occurrence
GROUP BY MONTH(condition_start_date)
ORDER BY 1;
```

## Input

None

## Output

| Field |  Description |
| --- | --- |
| Month_number | Month number |
| Number_of_conditions_in_month |  The number of the condition occurrences is a specified month. |

## Example output record

|  Field |  Description |
| --- | --- |
| Month_number |  3 |
| Number_of_conditions_in_month |  20643257 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
