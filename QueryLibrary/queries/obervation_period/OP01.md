<!---
Group:observation period
Name:OP01 Count number of people who have at least one observation period in the database that is longer than 365 days.
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP01: Count number of people who have at least one observation period in the database that is longer than 365 days.

## Description
## Query
```sql
SELECT COUNT(DISTINCT person_ID) AS NUM_persons
FROM @cdm.observation_period op
WHERE DATEDIFF(day,op.observation_period_START_DATE,op.observation_period_END_DATE) >= 365;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| Num_Persons | Number of patients who have at least one observation period in the database that is longer than 365 days |

## Example output record

|  Field |  Value |
| --- | --- |
| Num_Persons | 105119818 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
