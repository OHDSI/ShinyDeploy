<!---
Group:drug exposure
Name:DEX09 Distribution of distinct drugs per person over some time period
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX09: Distribution of distinct drugs per person over some time period

## Description
| This query is to determine the distribution of distinct drugs patients are exposed to during a certain time period. If the time period is omitted, the entire time span of the database is considered.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.  

```sql
WITH drug_counts
AS (
	SELECT DE.person_id,
		COALESCE(drugs, 0) AS drugs
	FROM @cdm.person p
	LEFT JOIN (
		SELECT person_id,
			COUNT(DISTINCT drug_concept_id) AS drugs
		FROM @cdm.drug_exposure
		WHERE drug_exposure_start_date >= DATEFROMPARTS(2017, 01, 01)
			AND drug_exposure_start_date <= DATEFROMPARTS(2017, 12, 31)
		GROUP BY person_id
		) DE ON DE.person_id = p.person_id
	)
SELECT MIN(drugs) AS min,
	MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE drugs END) AS percentile_25,
	ROUND(CAST(AVG(drugs) AS NUMERIC), 2) AS mean,
	MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE drugs	END) AS median,
	MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE drugs	END) AS percentile_75,
	MAX(drugs) AS max,
	ROUND(CAST(STDEV(drugs) AS NUMERIC), 1) AS STDEV
FROM (
	SELECT person_id,
		drugs,
		ROW_NUMBER() OVER (
			ORDER BY drugs
			) AS order_nr
	FROM drug_counts
	) AS ordered_data
CROSS JOIN (
	SELECT count(*) AS population_size
	FROM drug_counts
	) AS population;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| date from | 01-Jan-2008 | Yes |   |
| date to | 31-Dec-2008 | Yes |   |

## Output

| Field |  Description |
| --- | --- |
| min | The minimum number of drugs taken by a patient |
| percentile_25 | The 25th percentile of the distibution |
| mean | The mean or average of drugs taken by patients |
| median | The median number of drugs take |
| percentile_75 | The 75th percentile of the distribution |
| max | The maximum number of drugs taken by a patient |
| stddev | The standard deviation of the age distribution |


## Example output record

| Field |  Content |
| --- | --- |
| min | 0 |
| percentile_25 | 0 |
| mean | 1.73 |
| median | 0 |
| percentile_75 | 1 |
| max | 141 |
| stddev | 4.2 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
