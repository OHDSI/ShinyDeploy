<!---
Group:drug exposure
Name:DEX24 Counts of days supply
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX24: Counts of days supply

## Description
This query is used to count days supply values across all drug exposure records.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue.  

```sql
SELECT days_supply, COUNT(*) AS cnt
  FROM @cdm.drug_exposure
 GROUP BY days_supply
 ORDER BY days_supply;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| days_supply | 2,3 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| days_supply | The number of days of supply of the medication as recorded in the original prescription or dispensing record. |
| cnt | Counts of records with the days_supply value |

## Example output record

|  Field |  Description |
| --- | --- |
| days_supply |  15 |
| cnt |  240179 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
