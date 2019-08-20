<!---
Group:drug
Name:D11 Find source codes by drug class
Author:Patrick Ryan
CDM Version: 5.3
-->

# D11: Find source codes by drug class

## Description
This query is designed to extract codes from a non-standard drug vocabulary that belong to a therapeutic class. The query accepts a therapeutic class concept ID and the vocabualry ID of the desired source vocabulary as input and returns all codes that are included under that class and that belong to a source vocabulary. This query could be used to derive e.g. all NDC codes that belong to a certain drug class.

## Query
```sql
SELECT  d.concept_code,
        d.vocabulary_id,
        v.vocabulary_name
 FROM @vocab.concept_ancestor ca
        JOIN @vocab.concept d on d.concept_id = ca.descendant_concept_id
        JOIN @vocab.concept a on a.concept_id = ca.ancestor_concept_id
        JOIN @vocab.vocabulary v on d.vocabulary_id = v.vocabulary_id
 WHERE  ca.ancestor_concept_id = 21506108
   AND  a.vocabulary_id = 'NDC'
   AND  d.domain_id = 'Drug'
   AND  (getdate() >= d.valid_start_date)
   AND  (getdate() <= d.valid_end_date);
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
|  Therapeutic Class Concept ID |  21506108 |  Yes | Concept ID for 'ACE Inhibitors and ACE Inhibitor Combinations' |
|  Source Vocabulary ID |  9 |  Yes | One of the above drug vocabulary ID's |
|  As of date |  Sysdate |  No | Valid record as of specific date. Current date – sysdate is a default |

## Output

| Field |  Description |
| --- | --- |
|  Source_Code |  Source code of drug in non-standard vocabulary (e.g. NDC code, FDA SPL number etc.) |
|  Source_Vocabulary_ID |  Vocabulary ID of source vocabulary |
|  Source_Vocabulary_Name |  Vocabulary name of source vocabulary |
|  Source_Code_Description |  Description of source code |

## Example output record

| Field |  Value |
| --- | --- |
|  Source_Code |  00003033805 |
|  Source_Vocabulary_ID |  9 |
|  Source_Vocabulary_Name |  NDC |
|  Source_Code_Description |  Captopril 25 MG / Hydrochlorothiazide 15 MG Oral Tablet |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
