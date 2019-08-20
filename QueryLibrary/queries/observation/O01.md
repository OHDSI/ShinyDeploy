<!---
Group:observation
Name:O01 Find a Observation from a keyword
Author:Patrick Ryan
CDM Version: 5.3
-->

# O01: Find a Observation from a keyword

## Description
This query enables the search of LOINC and UCUM descriptions that are used in the observation domain of the vocabulary by keyword.
It does not require prior knowledge of where in the logic of the vocabularies the entity is situated.

## Query
The following is a sample run of the query to run a search of the Observation domain for keyword 'LDL'.

```sql
SELECT DISTINCT     
  C.concept_id             AS Entity_Concept_Id,
  C.concept_name           AS Entity_Name,
  C.concept_code           AS Entity_Code,
  'Concept'                AS Entity_Type,
  C.concept_class_id       AS Entity_concept_class_id,
  C.vocabulary_id          AS Entity_vocabulary_id
FROM @vocab.concept C
LEFT JOIN @vocab.concept_synonym S
ON C.concept_id = S.concept_id
WHERE  C.vocabulary_id IN ('LOINC', 'UCUM')
       AND C.concept_class_id IS NOT NULL
       AND C.standard_concept = 'S'
       -- regular expression containing the input pattern
       AND LOWER(C.concept_name) LIKE LOWER('%Ability to%')
       -- Retrieve only valid concepts
       AND getdate() >= C.valid_start_date AND my_date <= C.valid_end_date;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Keyword |  'LDL' |  Yes | Keyword search is case insensitive, and spaces and dashes are excluded from the search |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

Output field list

|  Field |  Description |
| --- | --- |
|  Entity_Concept_ID | Concept ID of entity with string match on name or synonym concept |
|  Entity_Name | Concept name of entity with string match on name or synonym concept |
|  Entity_Code | Concept code of entity with string match on name or synonym concept |
|  Entity_Type | Type of entity with keyword match (consistent with other keyword search queries elsewhere). Since procedure search is restricted to standard concepts and synonyms, the entity type is always set to 'Concept' |
|  Entity_concept_class_id | Concept class of entity with string match on name or synonym concept |
|  Entity_Vocabulary_ID | Vocabulary the concept with string match is derived from |

## Example output record

|  Field |  Value |
| --- | --- |
|  Entity_Concept_ID |  3035899 |
|  Entity_Name |  Cholesterol in LDL [Units/volume] in Serum or Plasma by Electrophoresis  |
|  Entity_Code |  12773-8 |
|  Entity_Type |  Concept |
|  Entity_concept_class_id |  Lab Test |
|  Entity_Vocabulary_ID |  LOINC |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
