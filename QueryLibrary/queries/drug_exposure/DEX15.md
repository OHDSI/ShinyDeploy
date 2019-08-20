<!---
Group:drug exposure
Name:DEX15 Number of persons taking a given drug having at least a 180 day period prior and a 365 day follow-up period
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX15: Number of persons taking a given drug having at least a 180 day period prior and a 365 day follow-up period

## Description

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.

```sql
WITH statins AS (
SELECT descendant_concept_id AS concept_id
  FROM @vocab.concept_ancestor
 WHERE ancestor_concept_id = 1539403
), statin_users AS (
SELECT de.person_id, MIN(de.drug_exposure_start_date) AS index_date
  FROM @cdm.drug_exposure de
  JOIN statins s
    ON de.drug_concept_id = s.concept_id
 GROUP BY de.person_id
)    
SELECT FLOOR(1.0*DATEDIFF(d,su.index_date,op.observation_period_end_date)/365) AS follow_up_years,
       COUNT(*) AS persons
       /* statin users with 180 clean period and at least 1 year follow up period */
  FROM statin_users su
  JOIN @cdm.observation_period op
    ON su.person_id  = op.person_id
 WHERE DATEADD(d,180,op.observation_period_start_date) < su.index_date
   AND op.observation_period_end_date                  > DATEADD(d,365,su.index_date)
 GROUP BY FLOOR(1.0*DATEDIFF(d,su.index_date,op.observation_period_end_date)/365)
 ORDER BY 1;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| concept_id | 1539403 | Yes | Statins |

## Output

|  Field |  Description |
| --- | --- |
| follow_up_years | The number of years of follow-up |
| persons | The number of persons with that number of follow-up years |

## Example output record

|  Field |  Description |
| --- | --- |
| follow_up_years |  |
| persons |  |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
