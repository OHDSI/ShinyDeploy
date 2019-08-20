<!---
Group:drug era
Name:DER13 Distribution of number of distinct drugs persons take
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER13: Distribution of number of distinct drugs persons take

## Description
This query is used to provide summary statistics for the number of number of different distinct drugs (drug_concept_id) of all exposed persons: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
```sql
WITH tt AS (
  SELECT COUNT(DISTINCT t.drug_concept_id) AS stat_value
  ,      ROW_NUMBER() OVER (ORDER BY COUNT(DISTINCT t.drug_concept_id)) order_nr
  ,      (SELECT COUNT(DISTINCT person_id) FROM @cdm.drug_era) AS population_size
  FROM @cdm.drug_era t
  WHERE ISNULL(t.drug_concept_id, 0) > 0
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
| Min_value | Minimum number of distinct drugs persons take |
| Max_value | Maximum number of distinct drugs persons take |
| Avg_value | Average number of distinct drugs persons take |
| Stdev_value | Standard deviation of drug era start date across all drug era records |
| percentile_25 | 25th percentile number of distinct drugs persons take |
| median_value | Median number of distinct drugs persons take |
| percentile_75 | the 75th percentile number of distinct drugs persons take |

## Example output record

|  Field |  Description |
| --- | --- |
| Min_value | 1 |
| Max_value | 580 |
| Avg_value | 12 |
| Stdev_value | 17 |
| percentile_25 | 3 |
| median_value | 6 |
| percentile_75 | 16 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
