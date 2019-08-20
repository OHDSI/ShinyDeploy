<!---
Group:drug
Name:D06 Find branded drugs by ingredient
Author:Patrick Ryan
CDM Version: 5.3
-->

# D06: Find branded drugs by ingredient

## Description
This query is designed to extract all branded drugs that have a specified ingredient. The query accepts an ingredient concept ID as the input and returns all branded drugs that have the ingredient. It should be noted that the query returns both generics that have a single ingredient (i.e. the specified ingredient) and those that are combinations which include the specified ingredient. The query requires the ingredient concept ID as the input. A list of these ingredient concepts can be extracted by querying the CONCEPT table for concept class of 'Ingredient'.

## Query
```sql
SELECT        A.concept_id Ingredient_concept_id,
                A.concept_name Ingredient_concept_name,
                A.concept_code Ingredient_concept_code,
                A.concept_class_id Ingredient_concept_class,
                D.concept_id branded_drug_id,
                D.concept_name branded_drug_name,
                D.concept_code branded_drug_concept_code,
                D.concept_class_id branded_drug_concept_class
FROM        @vocab.concept_ancestor CA,
                @vocab.concept A,
                @vocab.concept D
WHERE
        CA.ancestor_concept_id = 966991
AND        CA.ancestor_concept_id = A.concept_id
AND        CA.descendant_concept_id = D.concept_id
AND        D.concept_class_id = 'Branded Drug'
AND        (getdate() >= A.valid_start_date)
AND        (getdate() <= A.valid_end_date)
AND        (getdate() >= D.valid_start_date)
AND        (getdate() <= D.valid_end_date)
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Ingredient Concept ID |  966991 |  Yes | Concept ID for 'Simethicone'. |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

| Field |  Description |
| --- | --- |
|  Ingredient_Concept_ID |  Concept ID of the ingredient entered as input |
|  Ingredient_name |  Name of the Ingredient |
|  Ingredient_Concept_code |  Concept code of the ingredient |
|  Ingredient_Concept_class |  Concept Class of the ingredient |
|  Branded_Drug_ID |  Concept ID of branded drug with the ingredient |
|  Branded_Drug_Name |  Name of branded drug concept with the ingredient |
|  Branded_Drug_Concept_Code |  Concept code of the branded drug with the ingredient |
|  Branded_Drug_Concept_Class |  Concept class of branded drug with the ingredient |

## Example output record

|  Field |  Value |
| --- | --- |
|  Ingredient_Concept_ID |  966991 |
|  Ingredient_name |  Simethicone |
|  Ingredient_Concept_code |  9796 |
|  Ingredient_Concept_class |  Ingredient |
|  Branded_Drug_ID |  19132733 |
|  Branded_Drug_Name |  Simethicone 66.7 MG/ML Oral Suspension [Mylicon] |
|  Branded_Drug_Concept_Code |  809376 |
|  Branded_Drug_Concept_Class |  Branded Drug |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
