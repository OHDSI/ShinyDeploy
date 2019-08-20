<!---
Group:drug
Name:D09 Find drugs by drug class
Author:Patrick Ryan
CDM Version: 5.3
-->

# D09: Find drugs by drug class

## Description
This query is designed to extract all drugs that belong to a therapeutic class. The query accepts a therapeutic class concept ID as the input and returns all drugs that are included under that class .
Therapeutic classes could be obtained using query  D02 and are derived from one of the following:

- Enhanced Therapeutic Classification (FDB ETC)
- Anatomical Therapeutic Chemical classification (WHO ATC)

– NDF-RT Mechanism of Action (MoA), Concept Class = 'Mechanism of Action'

– NDF-RT Physiologic effect (PE), Concept Class = 'Physiologic Effect'

– NDF-RT Chemical Structure, Concept Class = 'Chemical Structure'

- VA Class

## Query
```sql
SELECT  c.concept_id       AS drug_concept_id,
        c.concept_name     AS drug_concept_name,
        c.concept_class_id AS drug_concept_class,
        c.concept_code     AS drug_concept_code
FROM @vocab.concept AS c
  JOIN @vocab.concept_ancestor AS ca
    ON c.concept_id = ca.descendant_concept_id
WHERE ca.ancestor_concept_id = 21506108
      AND c.domain_id = 'Drug'
      AND c.standard_concept = 'S'
      AND getdate() >= c.valid_start_date
      AND getdate() <= c.valid_end_date
;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Therapeutic Class Concept ID |  21506108 |  Yes | Concept ID for 'ACE Inhibitors and ACE Inhibitor Combinations' |

## Output

| Field |  Description |
| --- | --- |
|  drug_concept_id |  Concept ID of drug included in therapeutic class |
|  drug_concept_name |  Name of drug concept included in therapeutic class |
|  drug_concept_class |  Concept class of drug concept included in therapeutic class |
|  drug_concept_code |  RxNorm source code of drug concept |

## Example output record

|  Field |  Value |
| --- | --- |
|  drug_concept_id |  1308221 |
|  drug_concept_name |  Lisinopril 40 MG Oral Tablet |
|  drug_concept_class |  Clinical Drug |
|  drug_concept_code |  197884 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
