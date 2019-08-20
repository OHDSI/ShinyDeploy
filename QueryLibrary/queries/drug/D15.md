<!---
Group:drug
Name:D15 Find drugs for an indication provided as condition concepts
Author:Patrick Ryan
CDM Version: 5.3
-->

# D15: Find drugs for an indication provided as condition concepts

## Description
This query provides all clinical/branded drugs that are indicated for a certain indication. Indications have to be provided as SNOMED-CT concept.

## Query
```sql
SELECT DISTINCT
  drug.concept_id as drug_concept_id,
  drug.concept_name as drug_concept_name,
  drug.concept_code as drug_concept_code
FROM
  @vocab.concept drug,
  @vocab.concept_ancestor snomed,
  @vocab.concept_ancestor ind,
  @vocab.concept_relationship r
WHERE
  snomed.ancestor_concept_id = 253954 AND
  snomed.descendant_concept_id = r.concept_id_1 AND
  concept_id_2 = ind.ancestor_concept_id AND
  -- r.relationship_id in (247, 248) AND
  --r.relationship_id in ('Ind/CI - SNOMED', 'SNOMED - ind/CI') AND
  ind.descendant_concept_id = drug.concept_id AND
  --drug.concept_level = 1 AND
  --drug.vocabulary_id = 'RxNorm' AND
  (getdate() >= drug.valid_start_date) AND (getdate() <= drug.valid_end_date);
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  253954 |  Yes | SNOMED-CT indication concept ID |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

| Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of the drug |
|  Drug_Concept_Name |  Name of the drug |
|  Drug_Concept_Code |  Concept code of the drug |

## Example output record

| Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  19073074 |
|  Drug_Concept_Name |  Aminosalicylic Acid 500 MG Oral Tablet |
|  Drug_Concept_Code |  308122 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
