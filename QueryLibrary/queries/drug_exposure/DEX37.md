<!---
Group:drug exposure
Name:DEX37 Counts of drug refills
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX37: Counts of drug refills

## Description
This query is used to count the drug refills (refills) across all drug exposure records.
The input to the query is a value (or a comma-separated list of values) of refills.
If the input is omitted, all existing values are summarized.

## Query

The following is a sample run of the query. The input parameters are highlighted in  blue

```sql
SELECT
  COUNT(1)  AS drug_exposure_count,
  refills   AS refills_count
FROM @cdm.drug_exposure
-- Filter by input list of refills
WHERE refills in (10, 20)
GROUP BY refills;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| refills count (list of numbers) | 10,20 | Yes |


## Output

|  Field |  Description |
| --- | --- |
| drug_exposure_count | The number of individual drug exposure occurrences used to construct the drug era. |
| Refills_Count | The number of refills after the initial prescription. The initial prescription is not counted, values start with 0. |


## Example output record

|  Field |  Description |
| --- | --- |
| drug_exposure_count |  70 |
| Refills_Count |  10 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
