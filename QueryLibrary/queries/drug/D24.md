<!---
Group:drug
Name:D24 Find the branded drugs in a list of drugs
Author:Patrick Ryan
CDM Version: 5.3
-->

# D24: Find the branded drugs in a list of drugs

## Description
This query is designed to identify branded drug concepts from a list of standard drug concept IDs. The query identifies branded drugs from the Concept table based on a concept class setting of 'Branded Drug'

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
      AND C.concept_id IN (1516830, 19046168)
      AND C.concept_class_id = 'Branded Drug'
      AND (getdate() >= C.valid_start_date) AND (getdate() <= C.valid_end_date)
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Drug Concept ID list |  1516830, 19046168 |  Yes | List of drug concept id's |

## Output

|  Field |  Description |
| --- | --- |
|  drug_concept_id |  Concept ID of branded drug or pack |
|  drug_name |  Name of branded drug or pack |
|  drug_concept_code |  Concept code of branded drug or pack |
|  drug_concept_class |  Concept class of branded drug or pack |
|  drug_standard_concept |  Indicates whether this drug is standard ('S'), classification ('C') or non-standard (empty) |
|  drug_vocabulary_id |  Vocabulary the branded drug concept has been derived from, expressed as vocabulary ID |

## Example output record

| Field |  Value |
| --- | --- |
|  drug_concept_id |  19046168 |
|  drug_name |  Triamcinolone 0.055 MG/ACTUAT Nasal Spray [Nasacort AQ] |
|  drug_concept_code |  211501 |
|  drug_concept_class |  Branded Drug |
|  drug_standard_concept |   |
|  drug_vocabulary_id |  RxNorm |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/drug_exposure
