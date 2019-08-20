<!---
Group:drug
Name:D07 Find single ingredient drugs by ingredient
Author:Patrick Ryan
CDM Version: 5.3
-->

# D07: Find single ingredient drugs by ingredient

## Description
This query accepts accepts an ingredient concept ID and returns all drugs which contain only one ingredient specified in the query. 
This query is useful when studying drug outcomes for ingredients where the outcome or drug-drug interaction effect of other ingredients needs to be avoided.

## Query
```sql
SELECT
      c.concept_id       AS drug_concept_id,
      c.concept_name     AS drug_concept_name,
      c.concept_class_id AS drug_concept_class_id
FROM @vocab.concept c
INNER JOIN (
  SELECT drug.cid FROM (
    SELECT a.descendant_concept_id cid, count(*) cnt FROM @vocab.concept_ancestor a
    INNER JOIN (
      SELECT c.concept_id FROM @vocab.concept c, @vocab.concept_ancestor a
      WHERE a.ancestor_concept_id = 1000560
      AND a.descendant_concept_id = c.concept_id AND c.domain_id = 'Drug'
    ) cd ON cd.concept_id = a.descendant_concept_id
    INNER JOIN @vocab.concept c ON c.concept_id=a.ancestor_concept_id
        WHERE c.concept_class_id = 'Ingredient'
    GROUP BY a.descendant_concept_id
  ) drug WHERE drug.cnt = 1  -- contains only 1 ingredient
) onesie ON onesie.cid = c.concept_id
WHERE (getdate() >= valid_start_date) AND (getdate() <= valid_end_date)
;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Ingredient Concept ID |  1000560 |  Yes | Concept ID for ingredient 'Ondansetron' |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Drug_Concept_ID |  Concept ID of a drug |
|  Drug_Concept_Name |  Name of drug Concept |
|  Drug_Concept_Class |  Concept Code of drug |

## Example output record

| Field |  Value |
| --- | --- |
|  Drug_Concept_ID |  40227201 |
|  Drug_Concept_Name |  Ondansetron 0.16 MG/ML Injectable Solution |
|  Drug_Concept_Class |  Clinical Drug |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
