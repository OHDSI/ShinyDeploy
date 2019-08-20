<!---
Group:condition occurrence
Name:CO12 Distribution of condition end dates.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO12: Distribution of condition end dates.

## Description
This query is used to to provide summary statistics for condition occurrence end dates (condition_occurrence_end_date) across all condition occurrence records: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
```sql
WITH end_rank AS 
  (SELECT
    condition_end_date,
    SUM(1) over (partition BY 1 ORDER BY condition_end_date ASC ROWS BETWEEN unbounded preceding AND CURRENT row) AS rownumASc
  FROM @cdm.condition_occurrence
  ),
    other_stat AS 
  (SELECT
    COUNT(condition_end_date)                                                                                   AS condition_end_date_count,
    MIN(condition_end_date)                                                                                     AS condition_end_date_min,
    MAX(condition_end_date)                                                                                     AS condition_end_date_max,
    DATEADD(day, AVG(CAST(DATEDIFF(DAY,CONVERT(date,'00010101'), condition_end_date) AS BIGINT)), CONVERT(date,'00010101')) AS condition_end_date_average,
    STDEV(DATEDIFF(d,CONVERT(date,'00010101'), condition_end_date))                                           AS condition_end_date_stddev
   FROM @cdm.condition_occurrence
   WHERE condition_end_date IS NOT NULL
  )
SELECT
  (SELECT 
    COUNT(condition_end_date) 
   FROM @cdm.condition_occurrence 
   WHERE condition_end_date IS NULL
  ) AS condition_end_date_null_count,
  condition_end_date_count,condition_end_date_min,condition_end_date_max,condition_end_date_average,condition_end_date_stddev,
  condition_end_date_25pctile,condition_end_date_median,condition_end_date_75pctile

FROM other_stat,
    (SELECT DATEADD(day,AVG(CAST(DATEDIFF(DAY, CONVERT(date, '00010101'), condition_end_date) AS BIGINT)),CONVERT(date, '00010101'))  AS condition_end_date_25pctile
      FROM (
       SELECT condition_end_date, rownumAsc,(SELECT COUNT(*) FROM end_rank ) AS rowno 
         FROM end_rank 
           ) a_1
       WHERE ( rownumASc = CAST(rowno*0.25 AS int)   AND floor(rowno*25/100)  = rowno*25/100 ) 
          OR ( rownumASc = CAST(rowno*0.25 AS int)   AND floor(rowno*25/100) != rowno*25/100 )
          OR ( rownumASc = CAST(rowno*0.25 AS int)+1 AND floor(rowno*25/100) != rowno*25/100 )
     ) condition_end_date_25pctile,
    (SELECT DATEADD(day,AVG(CAST(DATEDIFF(DAY, CONVERT(date, '00010101'), condition_end_date) AS BIGINT)),CONVERT(date, '00010101'))  AS condition_end_date_median
      FROM (
       SELECT condition_end_date, rownumAsc,(SELECT COUNT(*) FROM end_rank ) AS rowno 
         FROM end_rank 
          ) a_1
       WHERE ( rownumASc = CAST(rowno*0.5 AS int)   AND floor(rowno*50/100)  = rowno*50/100 ) 
          OR ( rownumASc = CAST(rowno*0.5 AS int)   AND floor(rowno*50/100) != rowno*50/100 )
          OR ( rownumASc = CAST(rowno*0.5 AS int)+1 AND floor(rowno*50/100) != rowno*50/100 )
     ) condition_end_date_median,
    (SELECT DATEADD(day,AVG(CAST(DATEDIFF(DAY, CONVERT(date, '00010101'), condition_end_date) AS BIGINT)),CONVERT(date, '00010101'))AS condition_end_date_75pctile
      FROM (
       SELECT condition_end_date, rownumAsc,(SELECT COUNT(*) FROM end_rank ) AS rowno 
         FROM end_rank 
         ) a_1
       WHERE ( rownumASc = CAST(rowno*0.75 AS int)   AND floor(rowno*75/100)  = rowno*75/100 ) 
          OR ( rownumASc = CAST(rowno*0.75 AS int)   AND floor(rowno*75/100) != rowno*75/100 )
          OR ( rownumASc = CAST(rowno*0.75 AS int)+1 AND floor(rowno*75/100) != rowno*75/100 )
     ) condition_end_date_75pctile
;
```

## Input

None

## Output

| Field |  Description |
| --- | --- |
| condition_end_date_null_count | Number of condition occurrences where end date is null |
| condition_end_date_count | Number of condition occurrence end dates |
| condition_end_date_min | The earliest end date of a condition occurrence |
| condition_end_date_max | The latest end date of a condition occurrence |
| condition_end_date_average | The average end date (spanning from the earliest to the latest date and counted by days) |
| condition_end_date_stddev | The standard deviation of end dates, in number of days (spanning from the earliest to the latest date and counted by days) |
| condition_end_date_25percentile |  An end date where 25 percent of the other end dates are earlier |
| condition_end_date_median |  An end date where half of the other end dates are earlier and half are later |
| condition_end_date_75percentile |  An end date where 75 percent of the other end dates are earlier |

## Example output record

|  Field |  Value |
| --- | --- |
| condition_end_date_null_count | 0 |
| condition_end_date_count | 224523674 |
| condition_end_date_min | 2003-01-01 |
| condition_end_date_max | 011-12-15 |
| condition_end_date_average | 2008-11-30 |
| condition_end_date_stddev | 651.19 |
| condition_end_date_25percentile | 2007-10-30 |
| condition_end_date_median | 2009-05-07 |
| condition_end_date_75percentile | 2010-05-04 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
