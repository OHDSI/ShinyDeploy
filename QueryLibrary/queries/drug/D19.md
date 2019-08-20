<!---
Group:drug
Name:D19 Find ingredients for an indication by indication type
Author:Patrick Ryan
CDM Version: 5.3
-->

# D19: Find ingredients for an indication by indication type

## Description
This query provides all ingredients that are indicated for a certain condition. In addition, it provides the type of indication: FDA-approved, off-label (both based on FDB indication classes) and may treat and may prevent (both based on NDF-RT). Indications have to be provided as FDB indicationsor NDF-RT.

## Query
```sql
SELECT DISTINCT
  ingredient.concept_id as ingredient_concept_id,
  ingredient.concept_name as ingredient_concept_name,
  ingredient.concept_code as ingredient_concept_code,
  rn.relationship_name as indication_type,
  indication_relation.relationship_id
FROM
  @vocab.concept_relationship indication_relation
INNER JOIN
  @vocab.concept_ancestor a ON a.ancestor_concept_id=indication_relation.concept_id_2
INNER JOIN
  @vocab.concept ingredient ON ingredient.concept_id=a.descendant_concept_id
INNER JOIN
  @vocab.relationship rn ON rn.relationship_id = indication_relation.relationship_id
WHERE
  indication_relation.concept_id_1 = 4345991 AND
  ingredient.domain_id = 'Drug' AND
  ingredient.standard_concept = 'S' AND
  indication_relation.relationship_id IN (
         'May treat',
         'May prevent',
         'May be treated by',
         'May be prevented by',
         'Has FDA-appr ind',
         'Has off-label ind',
         'Is FDA-appr ind of',
         'Is off-label ind of',
         'Inferred class of',
         'Has inferred class') AND
  (getdate() >= ingredient.valid_start_date) AND (getdate() <= ingredient.valid_end_date);
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  4345991 |  Yes | FDB indication concept for 'Vomiting' |

## Output

|  Field |  Description |
| --- | --- |
|  ingredient_concept_id |  Concept ID of the ingredient |
|  ingredient_concept_name |  Name of the ingredient |
|  ingredient_concept_code |  Concept code of the ingredient |
|  indication_type |  One of the FDB, NDF-RT or OMOP inferred indication types |
|  relationship_id |  Corresponding relationship ID to the Indication Type |

## Example output record

|  Field |  Value |
| --- | --- |
|  ingredient_concept_id |  733008 |
|  ingredient_concept_name |  Perphenazine |
|  ingredient_concept_code |  8076 |
|  indication_type |  Inferred ingredient of (OMOP) |
|  relationship_id |  281 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
