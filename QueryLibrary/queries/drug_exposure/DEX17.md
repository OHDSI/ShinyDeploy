<!---
Group:drug exposure
Name:DEX17 Why do people stop treatment?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX17: Why do people stop treatment?

## Description
| This query provides a list of stop treatment and their frequency.

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue. S

```sql
SELECT stop_reason, COUNT(*) AS reason_freq
  FROM @cdm.drug_exposure
 WHERE stop_reason IS NOT null
 GROUP BY stop_reason
 ORDER BY reason_freq DESC;
```

## Input
 None

## Output

|  Field |  Description |
| --- | --- |
| stop_reason | The reason the medication was stopped, where available. Reasons include regimen completed, changed, removed, etc. |
| reason_freq |  Frequency of stop reason |

## Example output record

|  Field |  Description |
| --- | --- |
| stop_reason |  Regimen Completed |
| reason_freq |  14712428 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
