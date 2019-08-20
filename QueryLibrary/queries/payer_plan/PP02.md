<!---
Group:payer plan
Name:PP02 Patient distribution by plan type
Author:Patrick Ryan
CDM Version: 5.3
-->

# PP02: Patient distribution by plan type

## Description
## Query
```sql
SELECT
  t.plan_source_value,
  t.pat_cnt AS num_patients,
  100.00*t.pat_cnt/ (sum(t.pat_cnt) over()) perc_of_total_count
FROM 
  (SELECT 
    p.plan_source_value, 
    count(1) AS pat_cnt
   FROM @cdm.payer_plan_period p
   GROUP BY p.plan_source_value
  ) AS t
ORDER BY t.plan_source_value;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| plan_source_value | The source code for the person's coverage plan as it appears in the source data. |
| num_patients | Number of patients |
| perc_of_total_count | Total count |

## Example output record

|  Field |  Value |
| --- | --- |
| plan_source_value | Preferred Provider Organization |
| num_patients | 148348803 |
| perc_of_total_count | 68.632428630338134 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
