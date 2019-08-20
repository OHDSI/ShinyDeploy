<!---
Group:drug
Name:D20 Find dose form of a drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# D20: Find dose form of a drug

## Description
This query accepts concept IDs for a drug product (clinical or branded drug or pack) and identifies the dose form.

The query relies on RxNorm concept relationship 'RxNorm has dose form' for this.

## Query
```sql
SELECT
        A.concept_id drug_concept_id,
         A.concept_name drug_name,
         A.concept_code drug_concept_code,
         D.concept_id dose_form_concept_id,
         D.concept_name dose_form_concept_name,
         D.concept_code dose_form_concept_code
FROM
        @vocab.concept_relationship CR,
         @vocab.concept A,
         @vocab.concept D
WHERE
        (getdate() >= CR.valid_start_date) AND (getdate() <= CR.valid_end_date)
        AND CR.concept_id_1 = A.concept_id
        AND CR.concept_id_2 = D.concept_id
        AND CR.concept_id_1 = 19060647
        AND CR.relationship_id = 'RxNorm has dose form'
        --AND CR.relationship_ID = 4
        --AND A.concept_class_id ='Clinical Drug'
        --AND A.vocabulary_id = 'RxNorm'
        ;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Drug Concept ID |  19060647 |  Yes | Must be a level 1 Clinical or Branded Drug or Pack |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

| Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of drug entered with specified dose form |
|  Drug_Name |  Name of drug with specified dose form |
|  Drug_Concept_Code |  Concept ID of the dose form |
|  Dose_Form_Concept_ID |  Concept ID of the dose form |
|  Dose_Form_Concept_name |  Name of the dose form |
|  Dose_Form_Concept_code |  Concept code of dose form |

## Example output record

| Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  19060647 |
|  Drug_Name |  Budesonide 0.2 MG/ACTUAT Inhalant Powder |
|  Drug_Concept_Code |  247047 |
|  Dose_Form_Concept_ID |  19082259 |
|  Dose_Form_Concept_name |  Inhalant Powder |
|  Dose_Form_Concept_code |  317000 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
