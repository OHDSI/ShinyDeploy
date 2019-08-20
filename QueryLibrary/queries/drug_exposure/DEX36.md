<!---
Group:drug exposure
Name:DEX36 Distribution of drug refills
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX36: Distribution of drug refills

## Description
This query is used to provide summary statistics for drug refills (refills) across all drug exposure records:
the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile,
the maximum and the number of missing values. No input is required for this query.

## Query
The following is a sample run of the query.

```sql
SELECT
    MIN(stat_value)                                                                    AS min_value,
    MAX(stat_value)                                                                    AS max_value,
    AVG(stat_value)                                                                    AS avg_value,
    ROUND(STDEV(stat_value), 1)                                                        AS STDEV_value,
    MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE stat_value END)      AS percentile_25,
    MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE stat_value END)      AS median_value,
    MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE stat_value END)      AS percentile_75

FROM (
  SELECT
    refills                                                     AS stat_value,
    ROW_NUMBER() OVER (ORDER BY refills)                        AS order_nr,
    (SELECT COUNT(*) FROM @cdm.drug_exposure WHERE refills > 0) AS population_size
  FROM @cdm.drug_exposure
  -- Retrieve only positive quantities
  WHERE refills > 0
) ordered_data;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| min_value | The minimum number of refills |
| max_value | The maximum number of refills |
| avg_value | The average number of refills |
| STDEV_value | The standard deviation of the number of refills |
| percentile_25 | The 25th percentile of the number of refills |
| median_value | The median of the number of refills |
| percentile_75 | The 75th percentile of the number of refills |

## Example output record

|  Field |  Description |
| --- | --- |
| min_value | 1  |
| max_value |  25 |
| avg_value |  3.74 |
| STDEV_value |  3 |
| percentile_25 | 2  |
| median_value |  3 |
| percentile_75 |  5 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
