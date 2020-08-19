<!---
Group:drug era
Name:DER07 What is the average time between eras for a given ingredient? ex. steroids for RA
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER07: What is the average time between eras for a given ingredient? ex. steroids for RA

## Description
Calculate the average time between eras for a given ingredient.

## Query
```sql
SELECT avg(datediff(day, t.drug_era_end_date, t.next_era_start)) AS num_days
FROM (
         SELECT r.drug_era_end_date,
                lead(r.drug_era_start_date)
                OVER (PARTITION BY r.person_id, r.drug_concept_id ORDER BY r.drug_era_start_date) AS next_era_start
         FROM @cdm.drug_era r
         WHERE r.drug_concept_id = 1304643
     ) t
WHERE t.next_era_start IS NOT NULL
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 1304643 | Yes | darbepoetin alfa |

## Output

|  Field |  Description |
| --- | --- |
| Num_days |  Average number of days between drug eras |

## Example output record

|  Field |  Value |
| --- | --- |
| Num_days |  82 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
