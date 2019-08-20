<!---
Group:condition occurrence combinations
Name:COC02 Determines length of course of therapy for a condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC02: Determines length of course of therapy for a condition

## Description

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT ingredient_name,
	ingredient_concept_id,
	count(*) AS num_patients,
	min(length_of_therapy) AS min_length_of_therapy,
	max(length_of_therapy) AS max_length_of_therapy,
	avg(length_of_therapy) AS average_length_of_therapy
FROM (
	SELECT condition.person_id,
		condition.condition_start_date,
		rx.drug_era_start_date,
		DATEDIFF(day, rx.drug_era_start_date, rx.drug_era_end_date) + 1 AS length_of_therapy,
		ingredient_name,
		ingredient_concept_id
	FROM (
		SELECT era.person_id,
			condition_era_start_date AS condition_start_date
		FROM @cdm.condition_era era
		INNER JOIN @cdm.observation_period AS obs ON obs.person_id = era.person_id
			AND condition_era_start_date >= DATEADD(day,180,observation_period_start_date)
				AND condition_era_start_date <= DATEADD(day,-180,observation_period_end_date)
		WHERE condition_concept_id IN (137829, 138723, 140065, 140681, 4031699, 4098027, 4098028, 4098145, 4098760, 4100998, 4101582, 4101583, 4120453, 4125496, 4125497, 4125498, 4125499, 4146086, 4146087, 4146088, 4148471, 4177177, 4184200, 4184758, 4186108, 4187773, 4188208, 4211348, 4211695, 4225810, 4228194, 4234973, 4298690, 4345236)
		) condition
	INNER JOIN @cdm.drug_era rx ON rx.person_id = condition.person_id
		AND rx.drug_era_start_date >= condition_start_date
			AND rx.drug_era_start_date <= DATEADD(day, 30,condition_start_date)
	INNER JOIN (
		SELECT DISTINCT ingredient.concept_id AS ingredient_concept_id,
			ingredient.concept_name AS ingredient_name
		FROM @vocab.concept_ancestor ancestor
		INNER JOIN @vocab.concept indication ON ancestor.ancestor_concept_id = indication.concept_id
		INNER JOIN @vocab.concept ingredient ON ingredient.concept_id = ancestor.descendant_concept_id
		WHERE lower(indication.concept_name) LIKE ('%anemia%')
			AND indication.vocabulary_id = 'Indication'
			AND ingredient.concept_class_id = 'Ingredient'
			AND indication.invalid_reason IS NULL /* not necessary as concept_ancestor stores only valid concepts */
			AND ingredient.invalid_reason IS NULL /* not necessary as concept_ancestor stores only valid concepts */
					) ingredients ON ingredient_concept_id = drug_concept_id
	) treatment
GROUP BY ingredient_name,
	ingredient_concept_id
ORDER BY num_patients DESC;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 500000201 | Yes | SNOMed codes for OMOP Aplastic Anemia 1 |

## Output

|  Field |  Description |
| --- | --- |
| ingredient_name |   |
| ingredient_concept_id |   |
| count |   |
| min_length_of_therapy |   |
| max_length_of_therapy |   |
| average_length_of_therapy |   |

## Example output record

|  Field |  Description |
| --- | --- |
| ingredient_name |   |
| ingredient_concept_id |   |
| count |   |
| min_length_of_therapy |   |
| max_length_of_therapy |   |
| average_length_of_therapy |   |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
