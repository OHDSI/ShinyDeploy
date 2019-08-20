<!---
Group:observation period
Name:OP05 Average length of observation, in month.
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP05: Average length of observation, in month.

## Description
Count average length of observation period in months.

## Query
```sql
SELECT 
  FLOOR(AVG(DATEDIFF(day, observation_period_start_date, observation_period_end_date)/30)) AS num_months
FROM @cdm.observation_period;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| num_months |  Average length of observation, in month |

## Example output record

|  Field |  Value |
| --- | --- |
| num_months |  30 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
