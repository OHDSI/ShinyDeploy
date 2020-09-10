<!---
Group:drug era
Name:DER23 Distribution of drug era start dates, stratified by drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER23: Distribution of drug era start dates, stratified by drug

## Description
This query is used to summary statistics of the drug era start dates (drug_era_start_date) across all drug era records, stratified by drug (drug_concept_id): the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. The input to the query is a value (or a comma-separated list of values) of a drug_concept_id. If the input is omitted, all possible values are summarized.

## Query
```sql
WITH drugs AS (
         SELECT DISTINCT drug_concept_id
         FROM   @cdm.drug_era
         WHERE drug_concept_id IN (1300978, 1304643, 1549080)
     ),
     start_date AS (
         SELECT t1.drug_concept_id
         ,      drug_era_start_date start_date
         ,      MIN(t1.drug_era_start_date) OVER(partition by t1.drug_concept_id) min_start_date
         ,      DATEDIFF(day, MIN(t1.drug_era_start_date) OVER(PARTITION BY t1.drug_concept_id), t1.drug_era_start_date) AS start_date_num
         FROM @cdm.drug_era t1
         WHERE t1.drug_concept_id IN (SELECT drug_concept_id FROM drugs)
     ),
     tt AS (
         SELECT start_date.drug_concept_id
              , start_date.start_date
              , start_date.min_start_date AS min_date
              , start_date.start_date_num
              , COUNT(*) OVER ( PARTITION BY start_date.drug_concept_id) AS population_size
              , ROW_NUMBER() OVER (PARTITION BY start_date.drug_concept_id ORDER BY start_date.drug_concept_id, start_date.start_date_num) order_nr
         FROM start_date
     )
SELECT tt.drug_concept_id
,      MIN(tt.start_date_num) AS min_value
,      MAX(tt.start_date_num) AS max_value
,      DATEADD(day, AVG(CAST(tt.start_date_num AS BIGINT)), tt.min_date) AS avg_value
,      ROUND(STDEV(tt.start_date_num), 0) AS STDEV_value
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .25 * tt.population_size THEN 99999999 ELSE tt.start_date_num END), tt.min_date) AS percentile_25
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .50 * tt.population_size THEN 99999999 ELSE tt.start_date_num END), tt.min_date) AS median_value
,      DATEADD(day, MIN(CASE WHEN tt.order_nr < .75 * tt.population_size THEN 99999999 ELSE tt.start_date_num END), tt.min_date) AS percentile_75
FROM tt
GROUP BY tt.drug_concept_id
,        tt.min_date
ORDER BY drug_concept_id;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 1300978, 1304643, 1549080 | Yes | 'Megestrol', 'darbepoetin alfa', 'Estrogens, Conjugated (USP)'  |

## Output
The start date for the drug era constructed from the individual instances of drug exposures. It is the start date of the very first chronologically recorded instance of utilization of a drug.
| Field |  Description |
| --- | --- |
| drug_concept_id | A foreign key that refers to a standard concept identifier in the vocabulary for the drug concept. |
| min_value | The minimum drug era start date, in days from the minimum |
| max_value | The maximum drug era start date, in days from the minimum |
| avg_value | The average drug era start date |
| stddev_value | The standard deviation of the drug era start date, in days  |
| percentile_25 | The first quartile of the drug era start dates     |
| median_value |  The median of the drug era start dates    |
| percentile_75 |  The third quartile of the drug era start dates    |

## Example output record

| Field |  Description |
| --- | --- |
| drug_concept_id | 1300978 |
| min_value | 0 |
| max_value | 7156 |
| avg_value | 2006-04-13 00:00:00 |
| stddev_value | 1808 |
| percentile_25 | 2000-03-21 00:00:00 |
| median_value | 2002-07-29 00:00:00 |
| percentile_75 | 2005-01-15 00:00:00 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/DRUG_ERA
