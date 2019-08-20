<!---
Group:drug exposure
Name:DEX06 Counts of distinct drugs in the database
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX06: Counts of distinct drugs in the database

## Description
This query is used to determine the number of distinct drugs (drug_concept_id) from the RxNorm vocabulary.
See  vocabulary queries for obtaining valid drug_concept_id values.

## Query
The following is a sample run of the query.  

```sql
SELECT
  COUNT(DISTINCT drug_concept_id) AS number_drugs
FROM @cdm.drug_exposure
INNER JOIN @cdm.concept
ON concept_id = drug_concept_id
WHERE LOWER(domain_id)='drug'
      AND vocabulary_id='RxNorm'
      AND standard_concept='S';
```

## Input
 None.

## Output

|  Field |  Description |
| --- | --- |
| number_drugs | The count of distinct drug concepts. |

## Example output record

|  Field |  Description |
| --- | --- |
| number_drugs | 10889 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
