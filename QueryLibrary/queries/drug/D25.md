<!---
Group:drug
Name:D25 Find the generic drugs in a list of drugs
Author:Patrick Ryan
CDM Version: 5.3
-->

# D25: Find the generic drugs in a list of drugs

## Description
This query is designed to identify generic drug concepts among from a list of standard drug concept IDs. The query identifies branded drugs from the CONCEPT table based on a concept class setting of 'Clinical Drug'

## Query
```sql
SELECT C.concept_id       AS drug_concept_id,
       C.concept_name     AS drug_name,
       C.concept_code     AS drug_concept_code,
       C.concept_class_id AS drug_concept_class,
       C.standard_concept AS drug_standard_concept,
       C.vocabulary_id    AS drug_vocabulary_id
FROM @vocab.concept AS C
WHERE C.domain_id = 'Drug'
      AND C.concept_id IN (1396833, 19060643)
      AND C.concept_class_id = 'Clinical Drug'
      AND (getdate() >= C.valid_start_date) AND (getdate() <= C.valid_end_date)
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Drug Concept ID list |  1396833, 19060643 |  Yes | List of drug concept id's |

## Output

|  Field |  Description |
| --- | --- |
|  drug_concept_id |  concept ID of generic drug or pack |
|  drug_name |  Name of generic drug or pack |
|  drug_concept_code |  Concept code of generic drug or pack |
|  drug_concept_class |  Concept class of generic drug or pack |
|  drug_standard_concept |  Indicates whether this drug is standard ('S'), classification ('C') or non-standard (empty) |
|  drug_vocabulary_id |  Vocabulary the generic drug concept has been derived from, expressed as vocabulary ID |

## Example output record

|  Field |  Value |
| --- | --- |
|  drug_concept_id |  19060643 |
|  drug_name |  Budesonide 0.05 MG/ACTUAT Nasal Spray |
|  drug_concept_code |  247042 |
|  drug_concept_class |  Clinical Drug |
|  drug_standard_concept |  |
|  drug_vocabulary_id | RxNorm |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/drug_exposure
