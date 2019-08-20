<!---
Group:condition occurrence combinations
Name:COC05 Mortality rate after initial diagnosis
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC05: Mortality rate after initial diagnosis

## Description


## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT COUNT(DISTINCT diagnosed.person_id) AS all_infarctions,
	SUM(CASE
			WHEN death.person_id IS NULL
				THEN 0
			ELSE 1
			END) AS death_from_infarction
FROM -- Initial diagnosis of Acute Myocardial Infarction
	(
	SELECT DISTINCT person_id,
		condition_era_start_date
	FROM /* diagnosis of Acute Myocardial Infarction, ranked by date, 6 month clean period with 1 year follow-up */
		(
		SELECT condition.person_id,
			condition.condition_era_start_date,
			SUM(1) OVER (
				PARTITION BY condition.person_id ORDER BY condition_era_start_date ROWS UNBOUNDED PRECEDING
				) AS ranking
		FROM @cdm.condition_era condition
		INNER JOIN --definition of Acute Myocardial Infarction 1
			(
			SELECT DISTINCT ca.descendant_concept_id AS concept_id
			FROM @vocab.concept concept1
			INNER JOIN @vocab.concept_relationship rel ON concept1.concept_id = rel.concept_id_1
			INNER JOIN @vocab.concept_ancestor ca ON ancestor_concept_id = concept_id_2
			WHERE rel.relationship_id = 'HOI - SNOMED'
				AND concept1.concept_name = 'OMOP Acute Myocardial Infarction 1'
				AND rel.invalid_reason IS NULL
			) conceptlist ON conceptlist.concept_id = condition_concept_id
		INNER JOIN @cdm.observation_period obs ON obs.person_id = condition.person_id
			AND condition_era_start_date >= DATEADD(day, 180, observation_period_start_date)
			AND condition_era_start_date <= DATEADD(day, - 360, observation_period_end_date)
		) ranked_diagnosis
	WHERE ranking = 1
	) diagnosed
LEFT JOIN @cdm.death /* death within a year */
	ON death.person_id = diagnosed.person_id
	AND death.death_date <= DATEADD(day, 360, condition_era_start_date);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | OMOP Acute Myocardial Infarction 1 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| all_infarctions | The number of persons with an infarction |
| death_from_infarction | The number of persons that died from an infarction |

## Example output record

|  Field |  Description |
| --- | --- |
| all_infarctions | 3629 |
| death_from_infarction | 638 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
