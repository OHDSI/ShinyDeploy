<!---
Group:drug era
Name:DER10 Distribution of drug era end dates
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER10: Distribution of drug era end dates

## Description
This query is used to to provide summary statistics for drug era end dates (drug_era_end_date) across all drug era records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
```sql
SELECT DISTINCT MIN(tt.end_date) over () AS min_date
,      MAX(tt.end_date) over () AS max_date
,      DATEADD(day, (AVG(CAST(tt.end_date_num AS BIGINT)) OVER ()), tt.min_date) AS avg_date
,      ROUND(STDEV(tt.end_date_num) over (), 0) AS STDEV_days
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .25 * tt.population_size THEN 9999 ELSE tt.end_date_num END) over (), tt.min_date) AS percentile_25_date
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .50 * tt.population_size THEN 9999 ELSE tt.end_date_num END) over (), tt.min_date) AS median_date
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .75 * tt.population_size THEN 9999 ELSE tt.end_date_num END) over (), tt.min_date) AS percentile_75_date
FROM
    (
        SELECT DATEDIFF(day, (MIN(t.drug_era_end_date) OVER()), t.drug_era_end_date) AS end_date_num
        ,      t.drug_era_end_date AS end_date
        ,      MIN(t.drug_era_end_date) OVER() AS min_date
        ,      MAX(t.drug_era_end_date) OVER () AS max_date
        ,      ROW_NUMBER() OVER (ORDER BY t.drug_era_end_date) order_nr
        ,      (SELECT COUNT(*) FROM @cdm.drug_era) AS population_size
        FROM @cdm.drug_era t
    ) tt
GROUP BY tt.order_nr
,        tt.population_size
,        tt.min_date
,        tt.end_date
,        tt.end_date_num;

```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
| Min_date | Minimum drug era end date across all drug era records |
| Max_date | Maximum drug era end date across all drug era records |
| Avg_date | Average drug era end date across all drug era records |
| percentile_25_date | 25th percentile of the drug era end date |
| median_date | Median of the drug era end date |
| percentile_75_date | the 75th percentile of the drug era end date |

## Example output record

|  Field |  Description |
| --- | --- |
| Min_date | 2006-01-01 00:00:00 |
| Max_date | 2017-09-30 00:00:00 |
| Avg_date | 2011-11-16 00:00:00 |
| percentile_25_date | 2008-12-08 00:00:00 |
| median_date | 2011-11-16 00:00:00 |
| percentile_75_date | 2014-10-24 00:00:00 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
