<!---
Group:drug exposure
Name:DEX07 Maximum number of drug exposure events per person over some time period
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX07: Maximum number of drug exposure events per person over some time period

## Description
| This query is to determine the maximum number of drug exposures that is recorded for a patient during a certain time period. If the time period is omitted, the entire time span of the database is considered. Instead of maximum, the query can be easily changed to obtained the average or the median number of drug records for all patients. See  vocabulary queries for obtaining valid drug_concept_id values.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
SELECT max(exposures) AS exposures_count
FROM (
	SELECT drug_exposure.person_id,
		COUNT(*) exposures
	FROM @cdm.drug_exposure
	INNER JOIN @cdm.person ON drug_exposure.person_id = person.person_id
	WHERE drug_concept_id IN (
			SELECT DISTINCT concept_id
			FROM @cdm.concept
			WHERE domain_id = 'Drug'
				AND standard_concept = 'S'
			)
		AND drug_exposure_start_date >= DATEFROMPARTS(2017, 01, 01)
		AND drug_exposure_start_date <= DATEFROMPARTS(2017, 12, 31)
	GROUP BY drug_exposure.person_id
	) EV;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| date from | 01-Jan-2008 | Yes | |
| date to | 31-Dec-2008 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| exposures_count | The number of drug exposure records for the patient with the maximum number of such records. |


## Example output record

|  Field |  Description |
| --- | --- |
| exposures_count | 1137 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
