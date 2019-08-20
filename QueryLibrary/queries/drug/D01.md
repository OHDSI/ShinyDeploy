<!---
Group:drug
Name:D01 Find drug concept by concept ID
Author:Patrick Ryan
CDM Version: 5.3
-->

# D01: Find drug concept by concept ID

## Description
This is the lookup for obtaining drug concept details associated with a concept identifier. This query is intended as a tool for quick reference for the name, class, level and source vocabulary details associated with a concept identifier.
This query is equivalent to  G01, but if the concept is not in the drug domain the query still returns the concept details with the Is_Drug_Concept_Flag field set to 'No'.

## Query
```sql
SELECT 
    c.concept_id AS drug_concept_id,
    c.concept_name AS drug_concept_name,
    c.concept_code AS drug_concept_code,
    c.concept_class_id AS drug_concept_class,
    c.standard_concept AS drug_standard_concept,
    c.vocabulary_id AS drug_concept_vocab_id,
    (CASE c.domain_id WHEN 'Drug' THEN 'Yes' ELSE 'No' END) AS is_drug_concept_flag
FROM @vocab.concept AS c
WHERE getdate() >= c.valid_start_date AND getdate() <= c.valid_end_date 
  AND c.concept_id = 1545999
;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Concept ID |  1545999 |  Yes | Concept Identifier from RxNorm for 'atorvastatin 20 MG Oral Tablet [Lipitor]' |

## Output

| Field |  Description |
| --- | --- |
|  drug_concept_id |  Concept Identifier entered as input |
|  drug_concept_name |  Name of the standard drug concept |
|  drug_concept_code |  Concept code of the standard drug concept in the source vocabulary |
|  drug_concept_class |  Concept class of standard drug concept |
|  drug_standard_concept |  Indicates whether this drug is standard ('S'), classification ('C') or non-standard (empty) |
|  drug_concept_vocab_id |  Vocabulary the standard drug concept is derived from as vocabulary ID |
|  is_drug_concept_flag |  Flag indicating whether the Concept ID belongs to a drug concept. 'Yes' if drug concept, 'No' if not a drug concept |

## Example output record

| Field |  Value |
| --- | --- |
|  drug_concept_id |  1545999 |
|  drug_concept_name |  atorvastatin 20 MG Oral Tablet [Lipitor] |
|  drug_concept_code |  617318 |
|  drug_concept_class |  Branded Drug |
|  drug_standard_concept |  S |
|  drug_concept_vocab_id |  RxNorm |
|  is_drug_concept_flag |  No |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/drug_exposure
