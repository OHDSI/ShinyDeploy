<!---
Group:drug
Name:D03 Find ingredients of a drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# D03: Find ingredients of a drug

## Description
This query is designed to accept a drug concept (both clinical or branded) as input and return the list of ingredients that constitute them. Drug concept IDs can be obtained using query G03 or D02.

## Query
```sql
SELECT
        D.Concept_Id drug_concept_id,
        D.Concept_Name drug_name,
        D.Concept_Code drug_concept_code,
        D.Concept_Class_id drug_concept_class,
        A.Concept_Id ingredient_concept_id,
        A.Concept_Name ingredient_name,
        A.Concept_Code ingredient_concept_code,
        A.Concept_Class_id ingredient_concept_class
FROM
        @vocab.concept_ancestor CA,
        @vocab.concept A,
        @vocab.concept D
WHERE
        CA.descendant_concept_id = D.concept_id
        AND CA.ancestor_concept_id = A.concept_id
        AND LOWER(A.concept_class_id) = 'ingredient'
        AND getdate() >= A.VALID_START_DATE
        AND getdate() <= A.VALID_END_DATE
        AND getdate() >= D.VALID_START_DATE
        AND getdate() <= D.VALID_END_DATE
        AND CA.descendant_concept_id IN (939355, 19102189, 19033566)
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  List of drug Concept ID |  939355, 19102189, 19033566 |  Yes | Includes both clinical and branded drug concepts |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of drug (clinical/generic or branded) |
|  Drug_Name |  Name of drug |
|  Drug_Concept_Code |  Concept code of the drug |
|  Drug_Concept_Class |  Concept class of the drug |
|  Ingredient_Concept_ID |  Concept ID of the clinical ingredient |
|  Ingredient_Name |  Name of the clinical ingredient |
|  Ingredient_Concept_Code |  Concept code of the clinical ingredient |
|  Ingredient_Concept_Class |  Concept Class of the clinical ingredient |

## Example output record

| Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  19102189 |
|  Drug_Name |  Omeprazole 20 MG Enteric Coated Tablet |
|  Drug_Concept_Code |  402014 |
|  Drug_Concept_Class |  Clinical Drug |
|  Ingredient_Concept_ID |  923645 |
|  Ingredient_Name |  Omeprazole |
|  Ingredient_Concept_Code |  7646 |
|  Ingredient_Concept_Class |  Ingredient |



## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
