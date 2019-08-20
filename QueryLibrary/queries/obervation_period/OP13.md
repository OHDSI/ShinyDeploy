<!---
Group:observation period
Name:OP13 Distribution of observation period start dates
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP13: Distribution of observation period start dates

## Description
This query is used to to provide summary statistics for observation period start dates (observation_period_start_date) across all observation period records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
```sql
WITH op AS
  (SELECT
      DATEDIFF(day,DATEFROMPARTS(1900,1,1),observation_period_start_date) diffs,
      ROW_NUMBER() over (order by observation_period_start_date) AS order_nr,
      CAST(CONVERT(VARCHAR, observation_period_start_date, 112) AS INTEGER) AS start_date,
      observation_period_start_date AS org_start_date,
      COUNT(*)OVER() AS population_size
   FROM @cdm.observation_period
  )

SELECT
  MIN(org_start_date) AS min_start_date,
  MAX(org_start_date) AS max_start_date,
  DATEADD(day, ROUND(AVG(CAST(diffs AS BIGINT)),1), DATEFROMPARTS(1900,1,1)) AS avg_start_date,
  ROUND(STDEV(start_date), 1)                                             AS STDEV_days,
  MIN(CASE WHEN order_nr < .25 * population_size THEN DATEFROMPARTS(9999,12,31) ELSE org_start_date END) AS percentile_25,
  MIN(CASE WHEN order_nr < .50 * population_size THEN DATEFROMPARTS(9999,12,31) ELSE org_start_date END) AS median,
  MIN(CASE WHEN order_nr < .75 * population_size THEN DATEFROMPARTS(9999,12,31) ELSE org_start_date END) AS percentile_75
FROM op
;
```

## Input

None

## Output

| Field |  Description |
| --- | --- |
|  min_start_date |  Minimum start date value |
|  max_start_date |  Maximum start date value |
|  avg_start_date |  Average start date value |
|  STDEV_days |  Standard Deviation of start date |
|  percentile_25 |  25th percentile of start date |
|  median |  Median of start date |
|  percentile_75 |  75th percentile of start date |

## Example output record

|  Field |  Value |
| --- | --- |
|  min_start_date |  1/1/2003 |
|  max_start_date |  6/30/2011 |
|  avg_start_date |  2/5/2008 |
|  STDEV_days |  741 |
|  percentile_25 |  1/1/2006 |
|  median |  1/1/2009 |
|  percentile_75 |  1/1/2010 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
