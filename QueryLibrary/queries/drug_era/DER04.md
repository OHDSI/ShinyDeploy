<!---
Group:drug era
Name:DER04 What proportion of observation time is a person exposed to a given drug?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER04: What proportion of observation time is a person exposed to a given drug?

## Description
Calculate the proportion of a person's observation period that the person is exposed to a given drug. 

## Query
```sql
SELECT (CASE WHEN o.totalObs = 0 THEN 0 ELSE 100 * (e.totExposure * 1.0 / o.totalObs * 1.0) END) AS proportion
FROM (
         SELECT SUM(datediff(day, r.drug_era_start_date, r.drug_era_end_date)) AS totExposure,
                r.person_id
         FROM @cdm.drug_era r
         WHERE r.person_id = 9717995
           AND r.drug_concept_id = 1549080
         GROUP BY r.person_id
     ) e
JOIN
     (
         SELECT sum(datediff(day, p.observation_period_start_date, p.observation_period_end_date)) AS totalObs,
                p.person_id
         FROM @cdm.observation_period p
         GROUP BY p.person_id
     ) o
    ON o.person_id = e.person_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 1549080 | Yes | Estrogens, Conjugated (USP) |

## Output

|  Field |  Description |
| --- | --- |
| proportion | proportion of observation time is a person exposed to a given drug |

## Example output record

|  Field |  Value |
| --- | --- |
| proportion |  0.1 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
