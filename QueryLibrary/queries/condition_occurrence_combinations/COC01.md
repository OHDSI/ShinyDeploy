<!---
Group:condition occurrence combinations
Name:COC01 Determines first line of therapy for a condition
Author:Patrick Ryan
CDM Version: 5.3
-->

# COC01: Determines first line of therapy for a condition

## Description


## Query
The following is a sample run of the query. The input parameters are highlighted in  blue  

```sql
SELECT
  ingredient_name,
  ingredient_concept_id,
  count(*) AS num_patients
FROM /*Drugs started by people up to 30 days after Angioedema diagnosis */ (
  SELECT
    condition.person_id,
    condition_start_date,
    drug_era_start_date,
    ingredient_name,
    ingredient_concept_id
  FROM /* people with Angioedema with 180 clean period and 180 day follow-up */ (
    SELECT
      era.person_id,
      condition_era_start_date AS condition_start_date
    FROM @cdm.condition_era era
    JOIN @cdm.observation_period obs
      ON obs.person_id = era.person_id AND
         era.condition_era_start_date >= DATEADD(DAY,180,obs.observation_period_start_date) AND
         era.condition_era_start_date <= DATEADD(DAY, -180,obs.observation_period_end_date)
    WHERE
      era.condition_concept_id IN -- SNOMed codes for Angioedema  
      ( SELECT descendant_concept_id
        FROM @vocab.concept_ancestor
        WHERE ancestor_concept_id=432791)
  ) condition
  JOIN @cdm.drug_era rx /* Drug_era has drugs at ingredient level */
    ON rx.person_id = condition.person_id AND
       rx.drug_era_start_date >= condition.condition_start_date AND
       rx.drug_era_start_date <= DATEADD(day,30,condition.condition_start_date)
  JOIN /* Ingredients for indication Angioedema */ (
    SELECT
      ingredient.concept_id AS ingredient_concept_id ,
      ingredient.concept_name AS ingredient_name
    FROM @vocab.concept ingredient
    JOIN @vocab.concept_ancestor ca ON ca.descendant_concept_id = ingredient.concept_id
    WHERE
      ca.ancestor_concept_id = 21003378 /* indication for angioedema */ AND
      ingredient.concept_class_id ='Ingredient'
  ) ingredients ON ingredients.ingredient_concept_id = rx.drug_concept_id
) therapy
GROUP by ingredient_name, ingredient_concept_id
ORDER BY num_patients DESC;
```
## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of condition_concept_id | 432791  | Yes | Angioedema |
| ancestor_concept_id | 21003378 | Yes | Angioedema |


## Output

|  Field |  Description |
| --- | --- |
| ingredient_name |   |
| ingredient_concept_id |   |
| count |   |

## Example output record

|  Field |  Description |
| --- | --- |
| ingredient_name |   |
| ingredient_concept_id |   |
| count |   |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
