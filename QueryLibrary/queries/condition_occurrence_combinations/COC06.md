<!---
Group:condition occurrence combinations
Name:COC06 Time until death after initial diagnosis
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC06: Time until death after initial diagnosis

## Description


## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT COUNT(DISTINCT diagnosed.person_id) AS all_infarction_deaths,
	ROUND(min(DATEDIFF(day, diagnosed.condition_era_start_date, death.death_date)) / 365, 1) AS min_years,
	ROUND(max(DATEDIFF(day, diagnosed.condition_era_start_date, death.death_date)) / 365, 1) AS max_years,
	ROUND(avg(DATEDIFF(day, diagnosed.condition_era_start_date, death.death_date)) / 365, 1) AS avg_years
FROM -- Initial diagnosis of Acute Myocardial Infarction
	(
	SELECT DISTINCT person_id,
		condition_era_start_date
	FROM /* diagnosis of Acute Myocardial Infarction, ranked by date, 6 month clean*/
		(
		SELECT condition.person_id,
			condition.condition_era_start_date,
			rank() OVER (PARTITION BY condition.person_id ORDER BY condition_era_start_date) AS ranking
		FROM @cdm.condition_era condition
		INNER JOIN -- definition of Acute Myocardial Infarction 1
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
			AND condition.condition_era_start_date >= DATEADD(day, 180, obs.observation_period_start_date)
		) diagnosis_ranked
	WHERE ranking = 1
	) diagnosed
INNER JOIN @cdm.death ON death.person_id = diagnosed.person_id;
```
## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | OMOP Acute Myocardial Infarction 1 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| all_infarction_deaths | Number of persons that died from an infarction |
| min_years | The minimum number of years between first diagnosis and death |
| max_years | The maximum number of years between first diagnosis and death |
| avg_years | The average number of years between first diagnosis and death |

## Example output record

|  Field |  Description |
| --- | --- |
| all_infarction_deaths | 638 |
| min_years | 1 |
| max_years | 6 |
| avg_years | 2 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
