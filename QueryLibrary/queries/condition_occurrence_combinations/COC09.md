<!---
Group:condition occurrence combinations
Name:COC09 Condition that is regionally dependent
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC09: Condition that is regionally dependent

## Description

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT STATE,
	count(*) AS total_enroled,
	sum(lymed) AS lyme_cases,
	ROUND((sum(lymed) / count(*)) * 100, 2) AS percentages
FROM (
	SELECT person.person_id,
		state,
		ISNULL(lymed, 0) lymed
	FROM @cdm.person
	INNER JOIN @cdm.location ON person.location_id = location.location_id
	LEFT JOIN (
		SELECT DISTINCT person_id,
			1 AS lymed
		FROM @cdm.condition_era
		INNER JOIN @vocab.concept_relationship cr ON concept_id_1 = condition_concept_id
		INNER JOIN @vocab.concept c ON concept_id_2 = concept_id
		WHERE (
				(
					c.vocabulary_id = 'ICD9CM'
					AND c.concept_code = '088.81'
					)
				OR (
					c.vocabulary_id = 'ICD10CM'
					AND c.concept_code LIKE 'A69.2%'
					)
				)
			AND cr.invalid_reason IS NULL
			AND relationship_id = 'Mapped from'
		) lp ON lp.person_id = person.person_id
	) lyme_patients
GROUP BY STATE
ORDER BY 4 DESC;
```
## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| source_code | 088.81 | Yes | lyme disease |


## Output

|  Field |  Description |
| --- | --- |
| state | The state field as it appears in the source data. |
| total_enroled | Number of patients in the state |
| lyme_cases | Number of patients in the state with lyme disease |
| percentages | Percentage of patients in the state with lyme disease |

## Example output record

|  Field |  Description |
| --- | --- |
| state |   |
| total_enroled |   |
| lyme_cases |   |
| percentages |   |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
