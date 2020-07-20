<!---
Group:drug exposure
Name:DEX19 How many people are taking a drug for a given indication?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX19: How many people are taking a drug for a given indication?

## Description
Calculate the number of people taking a drug for a given indication.

## Query
The following is a sample run of the query. The input parameters are highlighted in

```sql
/* indication and associated drug ids */
SELECT c1.concept_name AS indication, COUNT(DISTINCT de.person_id) AS count_value
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
 GROUP BY c1.concept_name;  
```
## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_name | Tuberculosis | Yes |

## Output

|  Field |  Description |
| --- | --- |
| Indication | The indication |
| count_value |  The number of persons getting medication for the indication |


## Example output record

|  Field |  Description |
| --- | --- |
| Indication | Tuberculosis |
| count_value |  12508 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
