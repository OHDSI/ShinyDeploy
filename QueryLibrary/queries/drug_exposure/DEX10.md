<!---
Group:drug exposure
Name:DEX10 Other drugs (conmeds) patients exposed to a certain drug take over some time period
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX10: Other drugs (conmeds) patients exposed to a certain drug take over some time period

## Description
This query is used to establish the medication (conmeds) taken by patients who also are exposed to a certain drug in a given time period. The query returns the number of patients taking the drug at least once. The input to the query is a value (or a comma-separated list of values) of a drug_concept_id and the time period. See  vocabulary queries for obtaining valid drug_concept_id values.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
SELECT concept_name AS drug_name,
	COUNT(1) AS persons
FROM (
	--Other drugs people are taking
	SELECT DISTINCT cohort.person_id,
		drug.drug_concept_id
	FROM (
		--person with specific drug in time frame
		SELECT DISTINCT person_id,
			drug_concept_id,
			from_date,
			to_date
		FROM @cdm.drug_exposure
		INNER JOIN (
			--date range
			SELECT DATEFROMPARTS(2008, 01, 01) AS from_date,
				DATEFROMPARTS(2008, 12, 31) AS to_date
			) dates ON drug_exposure_start_date BETWEEN from_date
				AND to_date
		WHERE drug_concept_id IN /*bortezomib, Thalidomide 50 mg capsules */ (1336825, 19047763)
		) cohort
	INNER JOIN @cdm.drug_exposure drug ON drug.person_id = cohort.person_id
		AND drug.drug_concept_id != cohort.drug_concept_id
		AND drug.drug_exposure_start_date >= start_date AND drug.drug_exposure_start_date <= to_date
	WHERE drug.drug_concept_id != 0 /* unmapped drug */
	) events
INNER JOIN @vocab.concept ON concept_id = drug_concept_id
GROUP BY concept_name
ORDER BY persons DESC;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug concept ids | 1336825, 19047763 | Yes | Bortezomib, Thalidomide 50 mg capsules |
| from_date | 01-jan-2008 | Yes |   |
| to_date | 31-dec-2009 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| drug_name | The name of the drug. |
| persons | count of patients taking the drug at least once |


## Example output record

| Field |  Value |
| --- | --- |
| drug_name | Dexamethasone 4 MG Oral Tablet |
| persons | 190 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
