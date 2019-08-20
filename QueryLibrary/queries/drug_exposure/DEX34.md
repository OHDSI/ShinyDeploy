<!---
Group:drug exposure
Name:DEX34 Distribution of drug quantity
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX34: Distribution of drug quantity

## Description
This query is used to provide summary statistics for drug quantity (quantity) across all drug exposure records:
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
    quantity                                                     AS stat_value,
    ROW_NUMBER() OVER (ORDER BY quantity)                        AS order_nr,
    (SELECT COUNT(*) FROM @cdm.drug_exposure WHERE quantity > 0) AS population_size
  FROM @cdm.drug_exposure
  -- Retrieve only positive quantities
  WHERE quantity > 0
) ordered_data;
```

## Input

 None

## Output

|  Field |  Description |
| --- | --- |
| min_value |  minimum quantity |
| max_value |  maximum quantity |
| avg_value | average quantity  |
| STDEV_value |  quantity standard deviation |
| percentile_25 | quantity 25th percentile  |
| median_value | quantity median  |
| percentile_75 | quantity 75th percentile  |

## Example output record

|  Field |  Description |
| --- | --- |
| min_value | 0.14  |
| max_value |  16650 |
| avg_value | 254  |
| STDEV_value | 699  |
| percentile_25 |  29 |
| median_value | 98  |
| percentile_75 |  237 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
