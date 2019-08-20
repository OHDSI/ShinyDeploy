<!---
Group:drug era
Name:DER17 Counts of drug era records stratified by observation month
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER17: Counts of drug era records stratified by observation month

## Description
This query is used to count the drug era records stratified by observation month. The input to the query is a value (or a comma-separated list of values) of a month. If the input is ommitted, all possible values are summarized.

## Query
```sql
SELECT MONTH(er.drug_era_start_date) month_num, COUNT(1) as eras_in_month_count
FROM @cdm.drug_era er
WHERE MONTH(er.drug_era_start_date)
IN (3, 5)
GROUP BY MONTH(er.drug_era_start_date)
ORDER BY 1;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of month numbers | 3, 5 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| month_num | Month number (ex. 3 is March) |
| eras_in_month_count | Number of drug era count per month |

## Example output record

|  Field |  Description |
| --- | --- |
| month_num |  3 |
| eras_in_month_count | 19657680 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
