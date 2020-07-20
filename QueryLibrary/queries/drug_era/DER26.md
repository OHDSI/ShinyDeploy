<!---
Group:drug era
Name:DER26 Counts of genders, stratified by drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER26: Counts of genders, stratified by drug

## Description
This query is used to count all genders (gender concept_id), stratified by drug (drug_concept_id). The input to the query is a value (or a comma-separated list of values) of a gender_concept_id and a drug_concept_id. If the input is ommitted, all existing value combinations are summarized.

## Query
```sql
SELECT p.gender_concept_id, t.drug_concept_id, count(1) AS count 
FROM @cdm.drug_era t 
JOIN @cdm.person p
    ON p.person_id = t.person_id
WHERE t.drug_concept_id IN (1300978, 1304643, 1549080)
    AND p.gender_concept_id IN (8507, 8532)
GROUP BY t.drug_concept_id, p.gender_concept_id
ORDER BY t.drug_concept_id, p.gender_concept_id;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of gender_concept_id | 8507, 8532 | Yes | 'Male', 'Female' |
| list of drug_concept_id | 1300978, 1304643, 1549080 | Yes | 'Megestrol', 'darbepoetin alfa', 'Estrogens, Conjugated (USP)'  |

## Output

|  Field |  Description |
| --- | --- |
| gender_concept_id | 8507 for male or 8532 for Female  |
| drug_concept_id | Concept id of the drug, foreign key to the concept table |
| count |  Number of drug era records |

## Example output record

|  Field |  Description |
| --- | --- |
| gender_concept_id | 8507 |
| drug_concept_id | 1300978 |
| count | 60 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/DRUG_ERA
