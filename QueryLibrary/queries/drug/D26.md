<!---
Group:drug
Name:D26 Find the brand name of a drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# D26: Find the brand name of a drug

## Description
This query is designed to accept a drug concept (both clinical and branded) as input and return a the brand name (or branded ingredient) associated with it. The query is useful to check for a brand names associated with a clinical drug. Drug concepts can be obtained using queries  G03](http://vocabqueries.omop.org/general-queries/g3) or  [D02.

## Query
```sql
SELECT A.Concept_Id               drug_concept_id,
        A.Concept_Name            drug_name,
        A.Concept_Code            drug_concept_code,
        A.concept_class_id           drug_concept_class_id,
        D.Concept_Id              brand_concept_id,
        D.Concept_Name            brand_name,
        D.Concept_Code            brand_concept_code,
        D.concept_class_id           brand_concept_class_id
FROM   @vocab.concept_relationship  CR003,
       @vocab.concept               A,
       @vocab.concept_relationship  CR007,
       @vocab.concept_relationship  CR006,
       @vocab.concept                 D
WHERE  CR003.relationship_ID  = 'Has tradename'
AND    CR003.concept_id_1     = A.concept_id
AND    lower(A.concept_class_id) = 'clinical drug'
AND    CR007.concept_id_2     = CR003.concept_id_2
AND    CR007.relationship_ID  = 'Constitutes'
AND    CR007.concept_id_1     = CR006.concept_id_1
AND    CR006.relationship_ID  = 'RxNorm has ing'
AND    CR006.concept_id_2     = D.concept_id
AND    lower(D.concept_class_id) = 'branded name'
AND    A.concept_Id           = 939355
AND    (getdate() >= CR006.VALID_START_DATE) AND (getdate() <= CR006.VALID_END_DATE)
UNION ALL
SELECT A.Concept_Id               drug_concept_id,
       A.Concept_Name             drug_name,
       A.Concept_Code             drug_concept_code,
       A.concept_class_id            drug_concept_class_id,
       D.Concept_Id               brand_concept_id,
       D.Concept_Name             brand_name,
       D.Concept_Code             brand_concept_code,
       D.concept_class_id            brand_concept_class_id
FROM   @vocab.concept               A,
       @vocab.concept_relationship  CR007,
       @vocab.concept_relationship  CR006,
       @vocab.concept               D
WHERE  lower(A.concept_class_id) = 'branded drug'
AND    CR007.concept_id_2     = A.concept_id
AND    CR007.relationship_ID  = 'Constitutes'
AND    CR007.concept_id_1     = CR006.concept_id_1
AND    CR006.relationship_ID  = 'RxNorm has ing'
AND    CR006.concept_id_2     = D.concept_id
AND    lower(D.concept_class_id) = 'branded name'
AND    A.concept_Id           = 939355
AND    (getdate() >= CR006.VALID_START_DATE) AND (getdate() <= CR006.VALID_END_DATE)
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Drug Concept ID |  939355 |  Yes | Can be both clinical and branded drug concepts |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

| Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of drug (clinical/generic or branded) |
|  Drug_Name |  Name of drug |
|  Drug_Concept_Code |  Concept code of the drug |
|  Drug_Concept_Class |  Concept class of drug |
|  Brand_Concept_ID |  Concept ID of the brand name (or branded ingredient) |
|  Brand_name |  Name of the brand name |
|  Brand_Concept_code |  Concept code of the brand name |
|  Brand_Concept_class |  Concept Class of the brand name |

## Example output record

|  Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  19102189 |
|  Drug_Name |  Omeprazole 20 MG Enteric Coated Tablet |
|  Drug_Concept_Code |  402014 |
|  Drug_Concept_Class |  Clinical Drug |
|  Brand_Concept_ID |  19045785 |
|  Brand_name |  Prilosec |
|  Brand_Concept_code |  203345 |
|  Brand_Concept_class |  Brand Name |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
