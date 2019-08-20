<!---
Group:drug
Name:D27 Find drugs of a brand
Author:Patrick Ryan
CDM Version: 5.3
-->

# D27: Find drugs of a brand

## Description
This query is designed to extract all clinical and branded drugs associated with a branded ingredient (or simply a brand name). Since the brand names are not part of the standard drug hierarchy in the OMOP vocabulary, the association between brand name and generic/branded drugs is made using a set of relationships.
The query requires a brand name concept ID as the input. Brand name concept IDs can be obtained by querying the Concept table for a concept class of 'Brand Name'.

## Query
```sql
SELECT  A.Concept_Id               drug_concept_id,
        A.Concept_Name             drug_name,
        A.Concept_Code             drug_concept_code,
        A.Concept_Class_id            drug_concept_class,
        D.Concept_Id               brand_concept_id,
        D.Concept_Name             brand_name,
        D.Concept_Code             brand_concept_code,
        D.Concept_Class_id            brand_concept_class
FROM   @vocab.concept_relationship  CR003,
       @vocab.concept               A,
       @vocab.concept_relationship  CR007,
       @vocab.concept_relationship  CR006,
       @vocab.concept               D
WHERE  CR003.relationship_ID   = 'Constitutes'
AND    CR003.concept_id_1      = A.concept_id
AND     lower(A.concept_class_id) = 'clinical drug'
AND    CR007.concept_id_2      = CR003.concept_id_2
AND    CR007.relationship_id   = 'Has tradename'
AND    CR007.concept_id_1      = CR006.concept_id_1
AND    CR006.relationship_id   = 'RxNorm has ing'
AND    CR006.concept_id_2      = D.concept_id
AND    lower(D.concept_class_id)  = 'brand name'
AND    D.concept_id            = 19011505
AND    (getdate() >= CR006.valid_start_date) AND (getdate() <= CR006.valid_end_date)
AND    (getdate() >= CR007.valid_start_date) AND (getdate() <= CR007.valid_end_date)
UNION ALL
SELECT  A.Concept_Id               drug_concept_id,
        A.Concept_Name             drug_name,
        A.Concept_Code             drug_concept_code,
        A.Concept_Class_id            drug_concept_class,
        D.Concept_Id               brand_concept_id,
        D.Concept_Name             brand_name,
        D.Concept_Code             brand_concept_code,
        D.Concept_Class_id            brand_concept_class
FROM   @vocab.concept              A,
       @vocab.concept_relationship  CR007,
       @vocab.concept_relationship  CR006,
       @vocab.concept               D
WHERE  lower(A.concept_class_id) = 'branded drug'
AND    CR007.concept_id_2     = A.concept_id
AND    CR007.relationship_ID  = 'Has tradename'
AND    CR007.concept_id_1     = CR006.concept_id_1
AND    CR006.relationship_ID  = 'RxNorm has ing'
AND    CR006.concept_id_2     = D.concept_id
AND    lower(D.concept_class_id) = 'brand name'
AND    D.concept_id           = 19011505
AND    (getdate() >= CR006.valid_start_date) AND (getdate() <= CR006.valid_end_date)
AND    (getdate() >= CR007.valid_start_date) AND (getdate() <= CR007.valid_end_date)
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Brand name Concept ID |  19011505 |  Yes | Concept ID for brand name 'Fosamax'.
Brand name concept IDs are listed in the CONCEPT table with a concept class of 'Brand name' |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of drug (clinical/generic or branded) |
|  Drug_Name |  Name of drug |
|  Drug_Concept_Code |  Concept code of the drug |
|  Drug_Concept_Class |  Concept class of drug |
|  Brand_Concept_ID |  Concept ID of the brand name entered as ingredient |
|  Brand_name |  Name of the brand |
|  Brand_Concept_code |  Concept code of the brand name |
|  Brand_Concept_class |  Concept Class of the brand name |

## Example output record

| Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  40173591 |
|  Drug_Name |  Alendronic acid 10 MG Oral Tablet [Fosamax] |
|  Drug_Concept_Code |  904421 |
|  Drug_Concept_Class |  Branded Drug |
|  Brand_Concept_ID |  19011505 |
|  Brand_name |  Fosamax |
|  Brand_Concept_code |  114265 |
|  Brand_Concept_class |  Brand Name |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
