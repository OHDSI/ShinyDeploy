<!---
Group:drug exposure
Name:DEX23 Distribution of days supply
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX23: Distribution of days supply

## Description
| This query is used to provide summary statistics for days supply (days_supply) across drug exposure records for a given year, month: the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. No input is required for this query.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
WITH days_supply_freq AS (
SELECT days_supply, COUNT(*) AS num_occurrences
  FROM @cdm.drug_exposure
 WHERE days_supply > 0
   AND YEAR(drug_exposure_start_date)  = 2008
   AND MONTH(drug_exposure_start_date) = 1
 GROUP BY days_supply
), ordered_data AS (
SELECT days_supply, num_occurrences,
       ROW_NUMBER()OVER(ORDER BY num_occurrences) order_nr,
       COUNT(*)OVER() population_size,
       MIN(days_supply)OVER() AS min_value,
       MAX(days_supply)OVER() AS max_value,
       ROUND(AVG(1.0*days_supply)OVER(),0) AS avg_value,    
       ROUND(STDEV(days_supply)OVER(),0)   AS stdev_value
  FROM days_supply_freq
)
SELECT min_value,max_value,avg_value,stdev_value,
       MAX(CASE WHEN order_nr = CEILING(population_size *.25) THEN days_supply END) AS pct_25,
       MAX(CASE WHEN order_nr = CEILING(population_size *.50) THEN days_supply END) AS median,
       MAX(CASE WHEN order_nr = CEILING(population_size *.75) THEN days_supply END) AS pct_75
  FROM ordered_data
 GROUP BY min_value,max_value,avg_value,stdev_value;
```

## Input

 None

## Output

|  Field |  Description |
| --- | --- |
| min_value | The minimum number of days supply |
| max_value | The maximum number of days supply |
| avg_value | The average number of days supply |
| STDEV_value | The standard deviation of the number of days supply |
| pct_25 | The 25th percentile of the number of days supply |
| median | The median of the number of days supply |
| pct_75 | The 75th percentile of the number of days supply |


## Example output record

|  Field |  Description |
| --- | --- |
| min_value | 10 |
| max_value | 90 |
| avg_value | 50 |
| STDEV_value | 27 |
| pct_25 | 80 |
| median | 60 |
| pct_75 | 90 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
