<!---
Group:drug
Name:D16 Find drugs for an indication by indication type
Author:Patrick Ryan
CDM Version: 5.3
-->

# D16: Find drugs for an indication by indication type

## Description
This query provides all drugs that are indicated for a certain condition. In addition, it provides the type of indication: FDA-approved, off-label (both based on FDB indication classes) and may treat and may prevent (both based on NDF-RT). Indications have to be provided as FDB indicationsor NDF-RT.

## Query
```sql
SELECT DISTINCT
 drug.concept_id      as drug_concept_id,
 drug.concept_name    as drug_concept_name,
 drug.concept_code    as drug_concept_code,
 rn.relationship_name as indication_type,
 indication_relation.relationship_id
FROM
  @vocab.concept_relationship indication_relation
INNER JOIN @vocab.concept_ancestor a
 ON a.ancestor_concept_id=indication_relation.concept_id_2
INNER JOIN @vocab.concept drug
 ON drug.concept_id=a.descendant_concept_id
INNER JOIN @vocab.relationship rn
 ON rn.relationship_id=indication_relation.relationship_id
WHERE indication_relation.concept_id_1 = 4345991
  AND drug.domain_id = 'Drug'
  AND drug.standard_concept = 'S'
  AND indication_relation.relationship_id in (
          'May treat',
        'Is off-label ind of',
        'Is FDA-appr ind of',
        'Inferred class of',
        'May be prevented by',
        'May prevent',
        'Has FDA-appr ind',
        'Has off-label ind',
        'May be treated by',
        'Has inferred class')
  AND (getdate() >= drug.valid_start_date) AND (getdate() <= drug.valid_end_date);
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  4345991 |  Yes | FDB indication concept for 'Vomiting' |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of the drug |
|  Drug_Concept_Name |  Name of the drug |
|  Drug_Concept_Code |  Concept code of the drug |
|  Indication_Type |  One of the FDB, NDF-RT or OMOP inferred indication types |
|  Relationship_id |  Corresponding relationship ID to the Indication Type |

## Example output record

|  Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  19019530 |
|  Drug_Concept_Name |  Perphenazine 4 MG Oral Tablet |
|  Drug_Concept_Code |  198077 |
|  Indication_Type |  Inferred ingredient of (OMOP) |
|  Relationship_id |  281 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
