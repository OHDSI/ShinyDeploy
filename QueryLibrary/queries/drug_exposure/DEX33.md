<!---
Group:drug exposure
Name:DEX33 Counts of drug exposure records stratified by observation month
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX33: Counts of drug exposure records stratified by observation month

## Description
This query is used to count the drug exposure records stratified by observation month. The input to the query is a value (or a comma-separated list of values) of a month. If the input is omitted, all possible values are summarized.

## Query
The following is a sample run of the query.

```sql
SELECT
  MONTH(drug_exposure_start_date) AS month_num,
  COUNT(*) AS                       exp_in_month_count
FROM @cdm.drug_exposure
WHERE MONTH(drug_exposure_start_date) IN (3, 5)
GROUP BY MONTH(drug_exposure_start_date)
ORDER BY month_num
;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of month numbers | 3, 5 |  Yes |  

## Output

|  Field |  Description |
| --- | --- |
| month_num | Month number (1-12) |
| exp_in_month_count | Number of drug exposures in the given month  |


## Example output record

|  Field |  Description |
| --- | --- |
| month_num | 3 |
| exp_in_month_count | 4278  |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
