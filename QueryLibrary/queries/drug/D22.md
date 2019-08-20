<!---
Group:drug
Name:D22 Find drugs by class and dose form
Author:Patrick Ryan
CDM Version: 5.3
-->

# D22: Find drugs by class and dose form

## Description
This query is designed to return a list of drug concept IDs that belong to a drug class and are of a certain dose form. The query ties together:

- Concept ancestor data to link drug concepts to therapeutic class
- RxNorm concept relationship 'RxNorm has dose form'

The results are combined to present a list of drugs from a specific therapeutic class with a specific dose form.

## Query
```sql
SELECT C.concept_id drug_concept_id,
C.concept_name drug_concept_name,
C.concept_code drug_concept_code
FROM @vocab.concept C,
        @vocab.concept_ancestor CA,
        @vocab.concept_relationship CRF,
        @vocab.concept F
WHERE CA.ancestor_concept_id = 4318008
        AND C.concept_id = CA.descendant_concept_id
        AND C.domain_id = 'Drug'
        AND C.standard_concept = 'S'
        AND CRF.concept_id_1 = C.concept_id
        AND CRF.relationship_ID = 'RxNorm has dose form'
        AND CRF.concept_id_2 = F.concept_id
        AND CHARINDEX(LOWER(REPLACE(REPLACE(F.concept_name, ' ', ''), '-', '')), LOWER(REPLACE(REPLACE('Nasal spray' , ' ', ''), '-', ''))) > 0
        AND (getdate() >= CRF.valid_start_date) AND (getdate() <= CRF.valid_end_date)
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| Therapeutic class Concept ID |  4318008 |  Yes | Concept ID for mechanism of action "Corticosteroid Hormone Receptor Agonists". Valid drug classes can be obtained using query  D02. |
|  Dose Form String |  'Nasal spray' |  Yes | Dose form string. Valid dose forms can be obtained using query  D19. |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of drug with specified therapeutic class and dose form |
|  Drug_Name |  Name of drug with specified therapeutic class and dose form |
|  Drug_Concept_Code |  Source code of drug |

## Example output record

|  Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  904131 |
|  Drug_Name |  Triamcinolone 0.055 MG/ACTUAT Nasal Spray |
|  Drug_Concept_Code |  245785 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
