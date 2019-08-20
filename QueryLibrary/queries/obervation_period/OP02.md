<!---
Group:observation period
Name:OP02 Distribution of length of observation, in months, among first observation periods across the population
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP02: Distribution of length of observation, in months, among first observation periods across the population

## Description
Count distribution of length of observation, in months, among first observation periods across the population.

## Query
```sql
SELECT
  FLOOR(DATEDIFF(day, observation_period_start_date, observation_period_end_date)/30) AS num_months,
  COUNT(DISTINCT person_id) AS num_persons
FROM
  ( SELECT
      person_ID,
      observation_period_START_DATE,
      observation_period_END_DATE,
      rank() OVER (PARTITION BY person_ID ORDER BY observation_period_START_DATE ASC) AS OP_NUMBER
    FROM @cdm.observation_period
  ) AS OP1
WHERE op_number = 1
GROUP BY FLOOR(DATEDIFF(day,observation_period_START_DATE, observation_period_END_DATE)/30)
ORDER BY FLOOR(DATEDIFF(day,observation_period_START_DATE, observation_period_END_DATE)/30) ASC
;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| num_month | Number of month duration |
| num_persons | Number of patients whose observation period with num_month duration |

## Example output record

|  Field |  Value |
| --- | --- |
| num_months |  1 |
| num_persons | 4132770 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
