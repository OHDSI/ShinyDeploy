<!---
Group:drug exposure
Name:DEX41 Distribution of drug exposure start date, stratified by drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX41: Distribution of drug exposure start date, stratified by drug

## Description
This query is used to provide summary statistics for start dates (drug_exposure_start_date) across all drug exposure records
stratified by drug (drug_concept_id): the mean, the standard deviation, the minimum, the 25th percentile, the median,
the 75th percentile, the maximum and the number of missing values. The input to the query is a value (or a comma-separated
list of values) of a drug_concept_id. If the input is omitted, the drug_exposure_start_date for all existing values of
drug_concept_id are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
WITH drug_dates AS
  (
    SELECT
        DATEDIFF(d,MIN(drug_exposure_start_date) OVER(partition BY drug_concept_id), drug_exposure_end_date) AS start_date_num,
        drug_exposure_start_date                                                                             AS start_date,
        MIN(drug_exposure_start_date) OVER(partition BY drug_concept_id)                                     AS min_date,
        drug_concept_id                                                                                      AS drug_concept_id
    FROM @cdm.drug_exposure
    WHERE drug_concept_id IN (906805, 1517070, 19010522, 19031397)
  )
SELECT
  ordered_data.drug_concept_id,
  min(start_date)                                                                                AS min_date,
  max(start_date)                                                                                AS max_date,
  DATEADD(dd,avg(start_date_num),min_date)                                                       AS avg_date,
  round(STDEV(start_date_num), 1)                                                                AS STDEV_days,
  dateadd(day,MIN(CASE WHEN order_nr < .25 * population_size THEN 999999 ELSE start_date_num END),min_date) AS percentile_25_date,
  dateadd(day,MIN(CASE WHEN order_nr < .50 * population_size THEN 999999 ELSE start_date_num END),min_date) AS median_date,
  dateadd(day,MIN(CASE WHEN order_nr < .75 * population_size THEN 999999 ELSE start_date_num END),min_date) AS percentile_75_date
FROM
 ( SELECT
    drug_concept_id                                                               AS drug_concept_id,
    min_date,
    start_date,
    start_date_num                                                                AS start_date_num,
    ROW_NUMBER() OVER (PARTITION BY drug_concept_id ORDER BY start_date_num)      AS  order_nr
  FROM drug_dates
) AS ordered_data
INNER JOIN
 ( SELECT drug_concept_id,
    COUNT(*) AS population_size
   FROM drug_dates
   GROUP BY drug_concept_id
) AS population_sizes
 ON ordered_data.drug_concept_id = population_sizes.drug_concept_id
GROUP BY ordered_data.drug_concept_id, min_date;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 906805, 1517070, 19010522 | Yes |   

## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The concept ID of the drug |
| min_date | The minimum start date of drug exposure for the drug |
| max_date | The maximum start date of drug exposure for the drug |
| avg_date | The average start date of drug exposure for the drug |
| STDEV_days | The standard deviation in days for the start date of drug exposure for the drug |
| percentile_25_date | The 25th percentile start date of drug exposure for the drug |
| median_date | The median start date of drug exposure for the drug |
| percentile_75_date | The 75th percentile start date of drug exposure for the drug |

## Example output record
|  Field |  Description |
| --- | --- |
| drug_concept_id |  906805 |
| min_date |  2014-01-02 |
| max_date | 2015-12-11  |
| avg_date | 2015-03-17 02:24:00  |
| STDEV_days |  205 |
| percentile_25_date |  2014-11-21  |
| median_date | 2015-04-30  |
| percentile_75_date |  2015-09-18 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
