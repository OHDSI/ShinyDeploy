<!---
Group:drug exposure
Name:DEX38 Counts of stop reasons
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX38: Counts of stop reasons

## Description
This query is used to count stop reasons (stop_reason) across all drug exposure records.
The input to the query is a value (or a comma-separated list of values) of a stop_reason.
If the input is omitted, all existing values are summarized.

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  COUNT(1) AS totExp,
  stop_reason
FROM @cdm.drug_exposure
-- Filter by input list of stop reasons
WHERE stop_reason IN ('INVALID')
GROUP BY stop_reason;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| stop_reason | 1 | Yes |   


## Output

|  Field |  Description |
| --- | --- |
| totExp | The number of individual drug exposure occurrences used to construct the drug era. |
| stop_reason | The reason the medication was stopped, where available. Reasons include regimen completed, changed, removed, etc. |

## Example output record

|  Field |  Description |
| --- | --- |
| totExp | 2003  |
| stop_reason |  Regimen completed |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
