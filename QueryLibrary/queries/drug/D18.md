<!---
Group:drug
Name:D18 Find ingredients for an indication provided as condition concept
Author:Patrick Ryan
CDM Version: 5.3
-->

# D18: Find ingredients for an indication provided as condition concept

## Description
This query provides all ingredients that are indicated for a certain indication. Indications have to be provided as SNOMED-CT concept ID.

## Query
```sql
SELECT DISTINCT
  ingredient.concept_id as ingredient_concept_id,
  ingredient.concept_name as ingredient_concept_name,
  ingredient.concept_code as ingredient_concept_code
FROM
  @vocab.concept ingredient,
  @vocab.concept_ancestor snomed,
  @vocab.concept_ancestor ind,
  @vocab.concept_relationship r
WHERE
  snomed.ancestor_concept_id = 253954 AND
  snomed.descendant_concept_id = r.concept_id_1 AND
  concept_id_2 = ind.ancestor_concept_id AND
  -- r.relationship_id in (247, 248) AND
  --r.relationship_id in (
  --     'Ind/CI - SNOMED',
  --     'SNOMED - ind/CI') AND
  ind.descendant_concept_id = ingredient.concept_id AND
  --ingredient.concept_level = 2 AND
  --ingredient.vocabulary_id = 'RxNorm' AND
  (getdate() >= ingredient.valid_start_date) AND (getdate() <= ingredient.valid_end_date);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  253954 |  Yes | SNOMED-CT indication concept ID |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Ingredient_Concept_ID |  Concept ID of the ingredient |
|  Ingredient_Concept_Name |  Name of the ingredient |
|  Ingredient_Concept_Code |  Concept code of the ingredient |

## Example output record

| Field |  Value |
| --- | --- |
|  Ingredient_Concept_ID |  1790868 |
|  Ingredient_Concept_Name |  Amikacin |
|  Ingredient_Concept_Code |  641 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
