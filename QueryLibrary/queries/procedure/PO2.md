<!---
Group:procedure
Name:P02 Find a procedure from a keyword.
Author:Patrick Ryan
CDM Version: 5.3
-->

# P02: Find a procedure from a keyword.

## Description
This query enables search of procedure domain of the vocabulary by keyword.
The query does a search of standard concepts names in the PROCEDURE domain (SNOMED-CT, ICD9, CPT, SNOMED Veterinary, OPCS4,
CDT ICD10PCS and HCPCS procedures) and their synonyms to return all related concepts.

This is a comprehensive query to find relevant terms in the vocabulary.
It does not require prior knowledge of where in the logic of the vocabularies the entity is situated.
To constrain, additional clauses can be added to the query. However, it is recommended to do a filtering after the result
set is produced to avoid syntactical mistakes.
The query only returns concepts that are part of the Standard Vocabulary, ie. they have concept level that is not 0.
If all concepts are needed, including the non-standard ones, the clause in the query restricting the concept level and
concept class can be commented out.

## Query

The following is a sample run of the query to run a search of the Procedure domain for keyword 'Fixation of fracture'.

```sql
SELECT DISTINCT
  C.concept_id            AS Entity_Concept_Id,
  C.concept_name          AS Entity_Name,
  C.concept_code          AS Entity_Code,
  'Concept'               AS Entity_Type,
  C.concept_class_id      AS Entity_concept_class_id,
  C.vocabulary_id         AS Entity_vocabulary_id
FROM @vocab.concept C
LEFT JOIN @vocab.concept_synonym S
ON C.concept_id = S.concept_id
WHERE C.vocabulary_id IN ('SNOMED','ICD9Proc','ICD10PCS','CPT4','CDT','HCPCS','SNOMED Veterinary','OPCS4')
      AND C.domain_id = 'Procedure'
      AND C.standard_concept = 'S'
      -- regular expression containing the input pattern
      AND LOWER(C.concept_name) LIKE LOWER('%Fixation of fracture%')
            OR LOWER(S.concept_synonym_name) LIKE LOWER('%Fixation of fracture%')
      -- Retrieve only valid concepts
      AND WHERE getdate() >= C.valid_start_date AND getdate() <= C.valid_end_date;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Keyword |  'artery bypass' |  Yes | Procedure keyword search |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

|  Field |  Description |
| --- | --- |
|  Entity_Concept_ID |  Concept ID of entity with string match on name or synonym concept |
|  Entity_Name |  Concept name of entity with string match on name or synonym concept |
|  Entity_Code |  Concept code of entity with string match on name or synonym concept |
|  Entity_Type |  Type of entity with keyword match (consistent with other keyword search queries elsewhere). Since procedure search is restricted to standard concepts and synonyms, the entity type is always set to 'Concept' |
|  Entity_Concept_Class_ID |  Concept class of entity with string match on name or synonym concept |
|  Entity_Vocabulary_ID |  Vocabulary the concept with string match is derived from as vocabulary ID |

## Example output record

| Field |  Value |
| --- | --- |
|  Entity_Concept_ID |  2107223 |
|  Entity_Name |  Coronary artery bypass, using venous graft(s) and arterial graft(s); two venous grafts (List separately in addition to code for primary procedure) |
|  Entity_Code |  33518 |
|  Entity_Type |  Concept |
|  Entity_Concept_Class_ID |  CPT-4 |
|  Entity_Vocabulary_ID |  CPT-4 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
