<!---
Group:observation period
Name:OP06 Average length of observation, in days.
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP06: Average length of observation, in days.

## Description
Count average number of observation period days.

## Query
```sql
SELECT 
  AVG(DATEDIFF(day,observation_period_start_date,observation_period_end_date)) AS num_days
FROM @cdm.observation_period;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| num_days |  Average length of observation period, in days |

## Example output record

|  Field |  Value |
| --- | --- |
| num_days |  966 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
