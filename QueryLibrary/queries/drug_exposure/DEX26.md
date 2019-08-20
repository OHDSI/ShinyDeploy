<!---
Group:drug exposure
Name:DEX26 Distribution of drug exposure end dates
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX26: Distribution of drug exposure end dates

## Description
This query is used to to provide summary statistics for drug exposure end dates (`drug_exposure_end_date`) across all drug
exposure records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile,
the maximum and the number of missing values. No input is required for this query.

## Query
The following is a sample run of the query.

```sql
WITH dexp_end_dates AS
( SELECT
    DATEDIFF(d,(SELECT MIN(drug_exposure_end_date) FROM @cdm.drug_exposure), drug_exposure_end_date) AS end_date_num,
    drug_exposure_end_date                                                                           AS end_date,
    (SELECT MIN(drug_exposure_end_date) FROM @cdm.drug_exposure)                                     AS min_date
  FROM @cdm.drug_exposure
)
SELECT
  min(end_date)                                                                                             AS min_date,
  max(end_date)                                                                                             AS max_date,
  dateadd(dd, avg(CAST(end_date_num AS BIGINT)), min_date)                                                                  AS avg_date,
  round(STDEV(end_date_num), 0)                                                                             AS stdev_days,
  dateadd(dd,  MIN(CASE WHEN order_nr < .25 * population_size THEN 999999 ELSE end_date_num END), min_date) AS percentile_25_date,
  dateadd(dd,  MIN(CASE WHEN order_nr < .50 * population_size THEN 999999 ELSE end_date_num END), min_date) AS median_date,
  dateadd(dd,  MIN(CASE WHEN order_nr < .75 * population_size THEN 999999 ELSE end_date_num END), min_date) AS percentile_75_date
FROM
 ( SELECT
    end_date_num,                                                              
    end_date,
    min_date,
    ROW_NUMBER() OVER ( ORDER BY end_date_num)      AS  order_nr,
    (SELECT COUNT(*) FROM dexp_end_dates)           AS population_size
  FROM dexp_end_dates
) AS ordered_data
GROUP BY min_date;
```

## Input

 None

## Output

|  Field |  Description |
| --- | --- |
| min_date | The minimum end date of drug exposure |
| max_date | The maximum end date of drug exposure |
| avg_date | The average end date of drug exposure |
| STDEV_days | The standard deviation in days for the end date of drug exposure |
| percentile_25_date | The 25th percentile end date of drug exposure |
| median_date | The median end date of drug exposure |
| percentile_75_date | The 75th percentile end date of drug exposure |

## Example output record

|  Field |  Description |
| --- | --- |
| min_date | 13859 |
| max_date | 15064 |
| avg_date | 14411 |
| STDEV_days | 283 |
| percentile_25_date | 14173 |
| median_date | 14404 |
| percentile_75_date | 14639 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
