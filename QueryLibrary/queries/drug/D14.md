<!---
Group:drug
Name:D14 Find drugs for an indication
Author:Patrick Ryan
CDM Version: 5.3
-->

# D14: Find drugs for an indication

## Description
This query provides all clinical or branded drugs that are indicated for a certain indication. Indications have to be given as FDB indicationsor NDF-RT indications. Indications can be identified using the generic query  G03](http://vocabqueries.omop.org/general-queries/g3), or, if at least one drug is known for this indication, query  [D04.

## Query
```sql
SELECT drug.concept_id   AS drug_concept_id,
       drug.concept_name AS drug_concept_name,
       drug.concept_code AS drug_concept_code
FROM @vocab.concept AS drug
  JOIN @vocab.concept_ancestor AS a
      ON a.descendant_concept_id = drug.concept_id
WHERE a.ancestor_concept_id = 1710446
  AND drug.standard_concept = 'S'
  AND drug.domain_id = 'Drug'
  AND getdate() >= drug.valid_start_date 
  AND getdate() <= drug.valid_end_date
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Indication Concept ID |  1710446 |  Yes | Cycloserine ingredient concept |

## Output

| Field |  Description |
| --- | --- |
|  drug_concept_id |  Concept ID of the drug |
|  drug_concept_name |  Name of the drug |
|  drug_concept_code |  Concept code of the drug |

## Example output record

|  Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  1710447 |
|  Drug_Concept_Name |  Cycloserine 250 MG Oral Capsule |
|  Drug_Concept_Code |  197551 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/drug_exposure
