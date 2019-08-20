<!---
Group:drug exposure
Name:DEX13 Distribution of provider specialities prescribing a given drug
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX13: Distribution of provider specialities prescribing a given drug

## Description
| This query provides the provider specialties prescribing a given drug, and the frequencies for each provider prescribing the drug (drug exposure records). Note that many databases do not record the prescribing provider for drugs. See  vocabulary queries for obtaining valid drug_concept_id values.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
SELECT c.concept_name AS specialty,
       COUNT(*)       AS prescriptions_count
  FROM @cdm.drug_exposure drug
  JOIN @cdm.provider p
    ON p.provider_id = drug.provider_id
  JOIN @vocab.concept c
    ON c.concept_id  = p.specialty_concept_id
 WHERE c.vocabulary_id           = 'Specialty'
   AND drug.drug_concept_id      = 2213473  /* Influenza virus vaccine */
   AND c.standard_concept  = 'S'
 GROUP BY c.concept_name
 ORDER BY prescriptions_count desc;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 2213473 | Yes | Influenza virus vaccine |

## Output

|  Field |  Description |
| --- | --- |
| specialty | The concept name of the specialty concept |
| prescriptions_count | The count of drug exposure records providers from the specialties are listed as prescribing provider. |

## Example output record

|  Field |  Value |
| --- | --- |
| specialty |  Family Practice |
| prescriptions_count |  214825 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
