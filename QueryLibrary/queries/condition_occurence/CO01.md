<!---
Group:condition occurrence
Name:CO01 How long does a condition last.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO01: How long does a condition last.

## Description
Returns the distribution of a certain condition duration in days.

## Query
```sql
SELECT
  DATEDIFF(day,ca.condition_era_start_date, ca.condition_era_end_date) + 1 AS num_condition_duration_days,
  count(*)                                                                 AS condition_duration_freq_count
FROM @cdm.condition_era ca
INNER JOIN @vocab.concept c
ON ca.condition_concept_id = c.concept_id
WHERE c.concept_id = 200219
GROUP BY  c.concept_id,
          c.concept_name,
          DATEDIFF(day,condition_era_start_date, ca.condition_era_end_date)
ORDER BY 1;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id |   200219 |  Yes | Abdominal Pain |

## Output

|  Field |  Description |
| --- | --- |
| num_condition_duration_days | Number of days condition diagnozed |
| condition_duration_freq_count | The frequency of the condition |

## Example output record

|  Field |  Description |
| --- | --- |
| num_condition_duration_days |  9 |
| condition_duration_freq_count |  9077 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
