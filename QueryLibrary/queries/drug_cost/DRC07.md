<!---
Group:drug cost
Name:DRC07 Distribution of costs paid by payer.
Author:Patrick Ryan
CDM Version: 5.3
-->

# DRC07: Distribution of costs paid by payer.

## Description
This query is used to to provide summary statistics for costs paid by coinsurance (paid_coinsurance) across all drug cost records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
```sql
WITH tt AS (
    SELECT c1.paid_patient_coinsurance                                         AS stat_value,
           ROW_NUMBER() OVER (ORDER BY c1.paid_patient_coinsurance)            AS order_nr,
           (SELECT COUNT(*) FROM @cdm.cost WHERE paid_patient_coinsurance > 0) AS population_size
    FROM @cdm.cost c1
    WHERE c1.paid_patient_coinsurance > 0
)
SELECT MIN(tt.stat_value)                                                            AS min_value,
       MAX(tt.stat_value)                                                            AS max_value,
       AVG(tt.stat_value)                                                            AS avg_value,
       (ROUND(STDEV(tt.stat_value), 0))                                              AS STDEV_value,
       MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE stat_value END) AS percentile_25,
       MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE stat_value END) AS median_value,
       MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE stat_value END) AS percentile_75
FROM tt;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| min_value | The minimum costs paid |
| max_value | The maximum costs paid |
| avg_value | The average costs paid |
| STDEV_value | The standard deviation of the costs paid |
| percentile_25 | The 25th percentile of the costs paid |
| median_value | The median of the costs paid |
| percentile_75 | The 75th percentile of the costs paid |

## Example output record

|  Field |  Description |
| --- | --- |
| min_value | 1.3  |
| max_value | 435.81  |
| avg_value |  5.8887 |
| STDEV_value |  9 |
| percentile_25 | 3.87  |
| median_value | 5.63  |
| percentile_75 | 12.54  |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/COST
