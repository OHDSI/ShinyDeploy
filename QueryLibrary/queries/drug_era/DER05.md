<!---
Group:drug era
Name:DER05 For a given indication, what proportion of patients take each indicated treatment?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER05: For a given indication, what proportion of patients take each indicated treatment?

## Description
For a given indication, calculate the proportion of patients that take each indicated treatment.

## Query
```sql
SELECT tt.concept_id, tt.concept_name, 100*(tt.cntPersons*1.0/tt.total*1.0) AS proportion FROM (
SELECT c.concept_id, c.concept_name, t.cntPersons, sum(cntPersons) over() AS total
FROM @vocab.concept c,
(SELECT er.drug_concept_id, count(DISTINCT er.person_id) AS cntPersons
FROM  @vocab.concept_relationship cr,
         @vocab.concept_ancestor ca,
      @cdm.drug_era er
WHERE cr.concept_id_1 = ca.descendant_concept_id
  and er.drug_concept_id = ca.ancestor_concept_id
  and cr.concept_id_2 = 21001738--&era_id -- &Indication_id
  -- allow only indication relationships
  and cr.relationship_id IN ('Has FDA-appr ind', 'Has off-label ind', 'May treat', 'May prevent', 'CI by', 'Is off-label ind of', 'Is FDA-appr ind of', 'May be treated by')
GROUP BY er.drug_concept_id, cr.concept_id_2
) t
WHERE t.drug_concept_id = c.concept_id
) tt
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_id | 21001738 | Yes | Cold Symptoms |
| list of relationship_id | 'Has FDA-appr ind', 'Has off-label ind', 'May treat', 'May prevent', 'CI by', 'Is off-label ind of', 'Is FDA-appr ind of', 'May be treated by' | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| Concept_id | Unique identifier for drug concept |
| Concept_name | Standardized drug name |
| Proportion | Drug that proportion of patients take |

## Example output record

|  Field |  Value |
| --- | --- |
| Concept_id | 1126658 |
| Concept_name | Hydromorphone |
| Proportion | 0.63270536909000900 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
