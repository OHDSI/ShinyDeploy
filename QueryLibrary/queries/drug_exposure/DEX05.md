<!---
Group:drug exposure
Name:DEX05 Counts of drug records for a particular drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX05: Counts of drug records for a particular drug

## Description
This query is used to count the drug exposure records for a certain drug (drug_concept_id).
The input to the query is a value (or a comma-separated list of values) of a drug_concept_id.
See  vocabulary queries for obtaining valid drug_concept_id values.
If the input is omitted, all drugs in the data table are summarized.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue. S


```sql
SELECT
  concept_name      AS drug_name,
  drug_concept_id,
  COUNT(*)          AS num_records
FROM @cdm.drug_exposure
INNER JOIN @vocab.concept
ON concept_id = drug_concept_id
WHERE LOWER(domain_id)='drug'
      AND vocabulary_id='RxNorm'
      -- Retrieve standard concepts
      AND standard_concept='S'
      -- List of input drug_concept_id
      AND drug_concept_id IN (40165254,40165258)
GROUP BY concept_name, drug_concept_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 40165254, 40165258 | No | Crestor 20 and 40 mg tablets |

## Output

|  Field |  Description |
| --- | --- |
| drug_name | An unambiguous, meaningful and descriptive name for the drug concept. |
| drug_concept_id | A foreign key that refers to a standard concept identifier in the vocabulary for the drug concept. |
| num_records | The number of drug exposure records |

## Example output record

|  Field |  Content |
| --- | --- |
| drug_name | Rosuvastatin calcium 20 MG Oral Tablet [Crestor] |
| drug_concept_id | 40165254 |
| num_records | 191244 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
