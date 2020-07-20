<!---
Group:drug exposure
Name:DEX20 How many people taking a drug for a given indicaton actually have that disease in their record prior to exposure?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX20: How many people taking a drug for a given indication actually have that disease in their record prior to exposure?

## Description
Calculate the number of people taking a drug for a given indication that actually have that disease in their record prior to exposure.

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
WITH tb_treatment_start AS (
/* person and tuberculosis treatment start date */
SELECT de.person_id, MIN(de.drug_exposure_start_date) AS treatment_start
  FROM @vocab.concept c1
  JOIN @vocab.concept_ancestor ca
    ON ca.ancestor_concept_id = c1.concept_id
  JOIN @cdm.drug_exposure de
    ON ca.descendant_concept_id = de.drug_concept_id
  JOIN @vocab.concept c2
    ON de.drug_concept_id  = c2.concept_id
 WHERE c1.concept_name     = 'Tuberculosis'   
   AND c1.concept_class_id = 'Ind / CI'
   AND c2.domain_id        = 'Drug'
   AND c2.vocabulary_id    = 'RxNorm'
   AND c2.standard_concept = 'S'
 GROUP BY de.person_id
), tb_first_diagnosis AS (
SELECT co.person_id, MIN(co.condition_start_date) AS first_diagnosis  
  FROM @vocab.concept c
  JOIN @vocab.concept_ancestor ca
    ON ca.ancestor_concept_id = c.concept_id
  JOIN @cdm.condition_occurrence co
    ON co.condition_concept_id = ca.descendant_concept_id  
 WHERE c.concept_name = 'Tuberculosis'
 GROUP BY co.person_id
)
SELECT COUNT(*) AS treated
  FROM tb_treatment_start tss
  JOIN tb_first_diagnosis tfd
    ON tss.person_id = tfd.person_id
   AND tss.treatment_start >= tfd.first_diagnosis;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | Tuberculosis | Yes |   

## Output

|  Field |  Description |
| --- | --- |
| treated | The number of persons treated for the specified indication |

## Example output record

|  Field |  Description |
| --- | --- |
| treated | 49 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
