<!---
Group:drug exposure
Name:DEX11 Distribution of brands used for a given generic drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX11: Distribution of brands used for a given generic drug

## Description
This query provides the brands that are used for a generic drug. The input to the query is a value of a drug_concept_id. See vocabulary queries for obtaining valid drug_concept_id values.  Note that depending on the mapping available for the source_values in the drug_exposure table, branded drug information might only partially or not be provided.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
SELECT drug_name,
       brand_name,
       100.00*cn_3_02/SUM(cn_3_02) OVER() AS perc_brand_count
  FROM (
SELECT A.concept_name  AS drug_name,
       D.concept_name  AS brand_name,
       COUNT(*)        AS cn_3_02
  FROM @vocab.concept AS A
  JOIN @vocab.concept_relationship AS CR003
    ON CR003.concept_id_1        = A.concept_id
  JOIN @vocab.concept_relationship AS CR007
    ON CR007.concept_id_2        = CR003.concept_id_2
  JOIN @vocab.concept_relationship AS CR006
    ON CR007.concept_id_1        = CR006.concept_id_1
  JOIN @vocab.concept D
    ON CR006.concept_id_2        = D.concept_id
  LEFT JOIN @cdm.drug_exposure de
    ON de.drug_concept_id        = cr003.concept_id_2
 WHERE CR003.relationship_ID     = 'Has tradename'
   AND A.concept_class_id        = 'Clinical Drug'
   AND CR007.relationship_ID     = 'Constitutes'
   AND CR006.relationship_ID     = 'Has brand name'
   AND D.concept_class_id        = 'Brand Name'
   AND A.concept_id              = 35606533
 GROUP BY A.concept_name, D.concept_name
       ) TMP;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 35606533 | Yes | Methylprednisolone |

## Output

|  Field |  Description |
| --- | --- |
| drug_name | The name of the query drug |
| brand_name | The name of the brand |
| perc_brand_count | The market share for each brand |

## Example output record

|  Field |  Content |
| --- | --- |
| drug_name | Methylprednisolone 125 MG Injection |
| brand_name | A-MethaPred |
| perc_brand_count | 60.56 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
