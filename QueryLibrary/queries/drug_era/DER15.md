<!---
Group:drug era
Name:DER15 Distribution of drug era records per person
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER15: Distribution of drug era records per person

## Description
This query is used to provide summary statistics for the number of drug era records (drug_era_id) for all persons: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. There is no input required for this query.

## Query
```sql
WITH tt AS
(
  SELECT COUNT(1) AS stat_value
  ,      ROW_NUMBER() OVER (ORDER BY COUNT(1)) order_nr
  ,      (SELECT COUNT(DISTINCT person_id) FROM @cdm.drug_era) AS population_size
  FROM @cdm.drug_era t
  GROUP BY t.person_id
)
SELECT MIN(tt.stat_value) AS min_value
,      MAX(tt.stat_value) AS max_value
,      AVG(tt.stat_value) AS avg_value
,      ROUND(STDEV(tt.stat_value), 0) AS STDEV_value
,      MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE stat_value END) AS percentile_25
,      MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE stat_value END) AS median_value
,      MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE stat_value END) AS percentile_75
FROM tt;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| Min_value | Minimum number of drug era records for all persons |
| Max_value | Maximum number of drug era records for all persons |
| Avg_value | Average number of drug era records for all persons |
| Stdev_value | Standard deviation of drug era record count across all drug era records |
| percentile_25 | 25th percentile number of drug era record count for all persons |
| median_value | Median number of drug era record for all persons |
| percentile_75 | the 75th percentile number of drug era record for all persons |

## Example output record

|  Field |  Description |
| --- | --- |
| Min_value | 1 |
| Max_value | 1908 |
| Avg_value | 23 |
| Stdev_value | 47 |
| percentile_25 | 3 |
| median_value | 7 |
| percentile_75 | 22 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
