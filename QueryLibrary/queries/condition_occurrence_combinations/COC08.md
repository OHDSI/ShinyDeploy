<!---
Group:condition occurrence combinations
Name:COC08 Patients with condition and some measurement criteria some number of days prior to or after initial condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC08: Patients with condition and some measurement criteria some number of days prior to or after initial condition

## Description

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT DISTINCT condition.person_id,
	measurement_date,
	condition_era_start_date
FROM @cdm.condition_era condition
INNER JOIN --  definition of Aplastic Anemia
	(
	SELECT DISTINCT ca.descendant_concept_id AS concept_id
	FROM @vocab.concept concept1
	INNER JOIN @vocab.concept_relationship rel ON concept1.concept_id = rel.concept_id_1
	INNER JOIN @vocab.concept_ancestor ca ON ancestor_concept_id = concept_id_2
	WHERE rel.relationship_id = 'HOI - SNOMED'
		AND concept1.concept_name = 'OMOP Aplastic Anemia 1'
		AND rel.invalid_reason IS NULL
	) conceptlist ON conceptlist.concept_id = condition_concept_id
INNER JOIN @cdm.measurement measurement ON measurement.person_id = condition.person_id
	AND measurement_date >= DATEADD(day, - 7, condition_era_start_date)
	AND measurement_date <= DATEADD(day, 7, condition_era_start_date)
WHERE measurement_concept_id IN /* leukocytes #/volume in blood */ (3000905, 3003282, 3010813)
	AND unit_concept_id = 8961 /* Thousand per cubic millimeter */
	AND value_as_number <= 3.5;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | OMOP Aplastic Anemia 1 | Yes |   |
| list of measurement_concept_id | 3000905, 3003282, 3010813 |   | Leukocytes #/volume in blood |

## Output

|  Field |  Description |
| --- | --- |
| person_id | The ID of the person |
| measurement_date | The date of the measurement criterium prior to the condition |
| condition_era_start_date | The start date for the condition era constructed from the individual instances of condition occurrences. It is the start date of the very first chronologically recorded instance of the condition. |

## Example output record

|  Field |  Description |
| --- | --- |
| person_id |   |
| measurement_date |   |
| condition_era_start_date |   |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
