<!---
Group:drug exposure
Name:DEX31 Distribution of drug exposure records per person
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX31: Distribution of drug exposure records per person

## Description
This query is used to provide summary statistics for the number of drug exposure records (drug_exposure_id) for all persons:
the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile and the maximum.
There is no input required for this query.

## Query
The following is a sample run of the query.

```sql
SELECT
    MIN(stat_value)                                                                    AS min_value,
    MAX(stat_value)                                                                    AS max_value,
    ROUND(AVG(stat_value), 1)                                                          AS avg_value,
    ROUND(STDEV(stat_value), 1)                                                        AS STDEV_value,
    MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE stat_value END)      AS percentile_25,
    MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE stat_value END)      AS median_value,
    MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE stat_value END)      AS percentile_75

FROM (
  SELECT
    COUNT(*)                                                        AS stat_value,
    ROW_NUMBER() OVER (ORDER BY count(*) )                          AS order_nr,
    (SELECT COUNT(DISTINCT person_id) FROM @cdm.drug_exposure )     AS population_size
  FROM @cdm.drug_exposure
  GROUP BY person_id
) ordered_data;
```

## Input

 None

## Output

|  Field |  Description |
| --- | --- |
| min_value | The minimum number of drug exposures |
| max_value | The maximum number of drug exposures |
| avg_value | The average number of drug exposures |
| STDEV_value | The standard deviation for the number of drug exposures |
| percentile_25 | The 25th percentile of the number of drug exposures |
| median_value | The median of the number of drug exposures |
| percentile_75 | The 75th percentile of the number of drug exposures |

## Example output record

|  Field |  Description |
| --- | --- |
| min_value | 1 |
| max_value | 816 |
| avg_value | 60 |
| STDEV_value | 62.1 |
| percentile_25 | 10 |
| median_value | 44 |
| percentile_75 | 96 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
