<!---
Group:observation period
Name:OP04 Number of people who have gap in observation (two or more observations)
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP04: Number of people who have gap in observation (two or more observations)

## Description
Count number of people who have two or more observations.

## Query
```sql
SELECT COUNT_BIG(person_id) AS num_persons
FROM -- more than one observation period
  (SELECT
    person_id
   FROM @cdm.observation_period
   GROUP BY person_id
   HAVING COUNT_BIG( person_id ) > 1
  ) AS t;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| num_persons |  Number of patients who have two or more observations |

## Example output record

|  Field |  Value |
| --- | --- |
| num_persons |  18650793 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
