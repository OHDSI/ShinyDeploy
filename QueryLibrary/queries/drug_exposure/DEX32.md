<!---
Group:drug exposure
Name:DEX32 Counts of drug exposure records per person
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX32: Counts of drug exposure records per person

## Description
This query is used to count the number of drug exposure records (drug_exposure_id) for all persons. The input to the query is a value (or a comma-separated list of values) for a number of records per person. If the input is omitted, all possible values are summarized.

## Query
The following is a sample run of the query.

```sql
SELECT
  person_id,
  count(*) AS exposure_count
FROM @cdm.drug_exposure
GROUP BY person_id
HAVING count(*) in (3,4)
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| count | 3, 4 |  Yes |   

## Output

|  Field |  Description |
| --- | --- |
| person_id | The ID of the person. |
| exposure_count | The number of drug exposures for the person. |


## Example output record

|  Field |  Description |
| --- | --- |
| person_id | 2026  |
| exposure_count |  4 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
