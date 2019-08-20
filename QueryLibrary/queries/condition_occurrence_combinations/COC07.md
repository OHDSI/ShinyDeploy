<!---
Group:condition occurrence combinations
Name:COC07 Patients with condition in conjunction with a procedure some number of days prior to or after initial condition.
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC07: Patients with condition in conjunction with a procedure some number of days prior to or after initial condition.

## Description
Aplastic Anemia AND Occurrence of at least one diagnostic procedure code for bone marrow aspiration or biopsy within 60 days prior to the diagnostic code.


## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  


```sql
SELECT DISTINCT condition.condition_concept_id,
	condition.person_id,
	procedure_date,
	condition_era_start_date
FROM @cdm.procedure_occurrence pr
INNER JOIN @cdm.condition_era condition ON condition.person_id = pr.person_id
INNER JOIN (
	SELECT DISTINCT ca.descendant_concept_id AS concept_id
	FROM @vocab.concept concept1
	INNER JOIN @vocab.concept_relationship rel ON concept1.concept_id = rel.concept_id_1
	INNER JOIN @vocab.concept_ancestor ca ON ancestor_concept_id = concept_id_2
	WHERE rel.relationship_id = 'HOI - SNOMED'
		AND concept1.concept_name = 'OMOP Aplastic Anemia 1'
		AND rel.invalid_reason IS NULL
	) conceptlist ON conceptlist.concept_id = condition_concept_id
WHERE pr.procedure_concept_id IN ( 2002382, 2002403, 2108452, 2108453, 2212660, 2212662, 3045142, 3048879, 36359239, 37586183)
	AND procedure_date >= DATEADD(day, - 60, condition_era_start_date)
	AND procedure_date <= condition_era_start_date;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | OMOP Aplastic Anemia 1 | Yes |   |
| list of procedure_concept_id | 2002382, 2002403, 2108452, 2108453, 2212660, 2212662, 3045142 , 3048879, 36359239, 37586183 |   | Bone marrow aspiration or biopsy |

## Output

|  Field |  Description |
| --- | --- |
| condition_concept_id | The concept ID of a condition 60 days prior to bone marrow aspiration or biopsy |
| person_id | The peron ID |
| procedure_date | The date of the procedure |
| condition_era_start_date | The start date of the condition |

## Example output record

|  Field |  Description |
| --- | --- |
| person_id |   |
| procedure_date |   |
| condition_era_start_date |   |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
