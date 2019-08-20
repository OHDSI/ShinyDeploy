<!---
Group:drug
Name:D02 Find drug or class by keyword
Author:Patrick Ryan
CDM Version: 5.3
-->

# D02: Find drug or class by keyword

## Description
This query enables search of vocabulary entities in the drug domain by keyword. The query does a search of standard concepts names in the DRUG domain including the following:

- RxNorm standard drug concepts
- ETC, ATC therapeutic classes
- NDF-RT mechanism of action, physiological effect, chemical structure concepts
- Synonyms of drug concepts
- Mapped drug codes from NDC, GPI, Multum, Multilex

## Query
```sql
SELECT c.concept_id Entity_Concept_Id, c.concept_name Entity_Name, c.concept_code Entity_Code, 'Concept' Entity_Type, c.concept_class_id Entity_concept_class_id, c.vocabulary_id Entity_vocabulary_id
FROM @vocab.concept c
WHERE c.concept_class_id IS NOT NULL
AND c.vocabulary_id in ('NDFRT','RxNorm','Indication','ETC','ATC','VA Class','GCN_SEQNO')
AND LOWER(REPLACE(REPLACE(c.concept_name, ' ', ''), '-', '')) LIKE 'lipitor'
AND getdate() >= c.valid_start_date
AND getdate() <= c.valid_end_date

UNION ALL

SELECT c.concept_id Entity_Concept_Id, c.concept_name Entity_Name, c.concept_code Entity_Code, 'Mapped Code' Entity_Type,
c.concept_class_id Entity_concept_class_id, c.vocabulary_id Entity_vocabulary_id
FROM @vocab.concept_relationship cr JOIN @vocab.concept c ON c.concept_id = cr.concept_id_1
AND cr.relationship_id = 'Maps to'
AND c.vocabulary_id IN ('NDC', 'GPI', 'Multum', 'Multilex', 'VA Product', 'MeSH', 'SPL')
AND LOWER(REPLACE(REPLACE(c.concept_name, ' ', ''), '-', '')) LIKE 'lipitor'
AND getdate() >= c.valid_start_date
AND getdate() <= c.valid_end_date

UNION ALL

SELECT c.concept_id Entity_Concept_Id, s.concept_synonym_name Entity_Name, c.concept_code Entity_Code, 'Concept Synonym' Entity_Type, c.concept_class_id Entity_concept_class_id, c.vocabulary_id Entity_vocabulary_id
FROM @vocab.concept c, @cdm.concept_synonym s
WHERE S.concept_id = c.concept_id
AND c.vocabulary_id in ('NDFRT','RxNorm','Indication','ETC','ATC','VA Class','GCN_SEQNO')
AND c.concept_class_id IS NOT NULL
AND LOWER(REPLACE(REPLACE(s.concept_synonym_name, ' ', ''), '-', '')) LIKE 'lipitor'
AND getdate() >= c.valid_start_date
AND getdate() <= c.valid_end_date;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Keyword |  'Lipitor' |  Yes | Keyword should be placed in single quote |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Entity_Concept_ID |  Concept ID of entity with string match on name or synonym concept |
|  Entity_Name |  Concept name of entity with string match on name or synonym concept |
|  Entity_Code |  Concept code of entity with string match on name or synonym concept |
|  Entity_Type |  Type of entity with keyword match, includes one of the following:
- Concept
- Concept Synonym
- Mapped Code
 |
|  Entity_Concept_Class |  Concept class of entity with string match on name or synonym concept |
|  Entity_Vocabulary_ID |  Vocabulary the concept with string match is derived from as vocabulary ID |
|  Entity_Vocabulary_Name |  Name of the vocabulary the concept with string match is derived from as vocabulary code |

## Example output record

|  Field |  Value |
| --- | --- |
|  Entity_Concept_ID |  1545999 |
|  Entity_Name |  atorvastatin 20 MG Oral Tablet [Lipitor] |
|  Entity_Code |  617318 |
|  Entity_Type |  Concept |
|  Entity_Concept_Class |  Branded Drug |
|  Entity_Vocabulary_ID |  8 |
|  Entity_Vocabulary_Name |  RxNorm |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
