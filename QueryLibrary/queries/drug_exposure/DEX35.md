<!---
Group:drug exposure
Name:DEX35 Counts of drug quantity
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX35: Counts of drug quantity

## Description
This query is used to count the drug quantity (quantity) across all drug exposure records.
The input to the query is a value (or a comma-separated list of values) of a quantity.
If the input is omitted, all possible values are summarized.

## Query
The following is a sample run of the query.

```sql
SELECT
  COUNT(1) AS drug_quantity_count,
  quantity
FROM @cdm.drug_exposure
-- List of input numbers
WHERE quantity in (10, 20)
GROUP BY quantity;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| quantity (list of numbers) | 10,20 | Yes |  

## Output

|  Field |  Description |
| --- | --- |
| drug_quantity_count | The count of the drug quantity  |
| quantity | The quantity of drug as recorded in the original prescription or dispensing record|

## Example output record

|  Field |  Description |
| --- | --- |
| drug_quantity_count |  14 |
| quantity |  20 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
