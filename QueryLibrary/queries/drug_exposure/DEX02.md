<!---
Group:drug exposure
Name:DEX02 Counts of persons taking a drug, by age, gender, and year of exposure
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX02: Counts of persons taking a drug, by age, gender, and year of exposure

## Description
| This query is used to count the persons with exposure to a certain drug (drug_concept_id), grouped by age, gender, and year of exposure. The input to the query is a value (or a comma-separated list of values) of a drug_concept_id. See  vocabulary queries for obtaining valid drug_concept_id values. If the input is omitted, all drugs in the data table are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue. s

```sql
SELECT concept_name AS drug_name,
	year_of_exposure,
	age,
	gender,
	count(*) AS num_persons
FROM (
	SELECT DISTINCT drug.concept_name,
		de.person_id,
		gender.concept_name AS gender,
		YEAR(drug_exposure_start_date) AS year_of_exposure,
		YEAR(drug_exposure_start_date) - year_of_birth AS age
	FROM @cdm.drug_exposure de
	INNER JOIN @cdm.person p ON de.person_id = p.person_id
	INNER JOIN @vocab.concept drug ON drug.concept_id = drug_concept_id
	INNER JOIN @vocab.concept gender ON gender.concept_id = gender_concept_id
	WHERE drug_concept_id IN (40165254, 40165258)
	) EV
GROUP BY concept_name,
	gender,
	year_of_exposure,
	age
ORDER BY concept_name,
	year_of_exposure,
	age,
	gender;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 40165254, 40165258 | No | Crestor 20 and 40 mg tablets |

## Output

|  Field |  Description |
| --- | --- |
| drug_name | The name of the drug. |
| year_of_exposure | The year of exposure |
| age | The age of the person at the time of exposure |
| gender | The gender of the person. |
| num_persons | The patient count |

## Example output record

|  Field |  Content |
| --- | --- |
| drug_name |  Rosuvastatin calcium 40 MG Oral Tablet [Crestor] |
| year_of_exposure |  2010 |
| age |  69 |
| gender |  Male |
| num_persons |  15 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
