<!---
Group:drug
Name:D17 Find ingredients for an indication
Author:Patrick Ryan
CDM Version: 5.3
-->

# D17: Find ingredients for an indication

## Description
This query provides ingredients that are designated for a certain indication. Indications have to be given as FDB indicationsor NDF-RT indications. Indications can be identified using the generic query  G03](http://vocabqueries.omop.org/general-queries/g3), or, if at least one drug is known for this indication, query  [D04.

## Query
```sql
SELECT
  ingredient.concept_id   AS ingredient_concept_id,
  ingredient.concept_name AS ingredient_concept_name,
  ingredient.concept_code AS ingredient_concept_code
FROM @vocab.concept AS ingredient
  JOIN @vocab.concept_ancestor AS a
    on a.descendant_concept_id = ingredient.concept_id
WHERE
  a.ancestor_concept_id = 4345991 AND
  ingredient.domain_id = 'Drug' AND
  ingredient.standard_concept = 'S' AND
  (getdate() >= ingredient.valid_start_date) AND (getdate() <= ingredient.valid_end_date)
;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  4345991 |  Yes | FDB indication concept for 'Vomiting' |

## Output

| Field |  Description |
| --- | --- |
|  ingredient_concept_id |  Concept ID of the ingredient |
|  ingredient_concept_name |  Name of the ingredient |
|  ingredient_concept_code |  Concept code of the ingredient |

## Example output record

|  Field |  Value |
| --- | --- |
|  ingredient_concept_id |  733008 |
|  ingredient_concept_name |  Perphenazine |
|  ingredient_concept_code |  8076 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
