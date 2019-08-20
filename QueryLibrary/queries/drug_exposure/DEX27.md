<!---
Group:drug exposure
Name:DEX27 Distribution of drug exposure start dates
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX27: Distribution of drug exposure start dates

## Description
This query is used to to provide summary statistics for drug exposure start dates (drug_exposure_start_date) across all drug exposure records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile and the maximum. No input is required for this query.

## Query
The following is a sample run of the query.  

```sql
WITH dexp_start_dates AS
( SELECT
    DATEDIFF(d,(SELECT MIN(drug_exposure_start_date) FROM @cdm.drug_exposure), drug_exposure_start_date) AS start_date_num,
    drug_exposure_start_date                                                                             AS start_date,
    (SELECT MIN(drug_exposure_start_date) FROM @cdm.drug_exposure)                                       AS min_date
  FROM @cdm.drug_exposure
)
SELECT
  min(start_date)                                                                                             AS min_date,
  max(start_date)                                                                                             AS max_date,
  dateadd(dd, avg(CAST(start_date_num AS BIGINT)), min_date)                                                                  AS avg_date,
  round(STDEV(start_date_num), 0)                                                                             AS stdev_days,
  dateadd(dd,  MIN(CASE WHEN order_nr < .25 * population_size THEN 99999999 ELSE start_date_num END), min_date) AS percentile_25_date,
  dateadd(dd,  MIN(CASE WHEN order_nr < .50 * population_size THEN 99999999 ELSE start_date_num END), min_date) AS median_date,
  dateadd(dd,  MIN(CASE WHEN order_nr < .75 * population_size THEN 99999999 ELSE start_date_num END), min_date) AS percentile_75_date
FROM
 ( SELECT
    start_date_num,                                                              
    start_date,
    min_date,
    ROW_NUMBER() OVER ( ORDER BY start_date_num)      AS  order_nr,
    (SELECT COUNT(*) FROM dexp_start_dates)           AS population_size
  FROM dexp_start_dates
) AS ordered_data
GROUP BY min_date;
```

## Input

 None

## Output

|  Field |  Description |
| --- | --- |
| min_date | The minimum start date of drug exposure |
| max_date | The maximum start date of drug exposure |
| avg_date | The average start date of drug exposure |
| STDEV_days | The standard deviation in days for the start date of drug exposure |
| percentile_25_date | The 25th percentile start date of drug exposure |
| median_date | The median start date of drug exposure |
| percentile_75_date | The 75th percentile start date of drug exposure |

## Example output record

|  Field |  Description |
| --- | --- |
| min_date | 13859 |
| max_date | 14974 |
| avg_date | 14381 |
| STDEV_days | 281 |
| percentile_25_date | 14143 |
| median_date | 14374 |
| percentile_75_date | 14608 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
