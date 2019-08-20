<!---
Group:drug exposure
Name:DEX08 Maximum number of distinct drugs per person over some time period
Author:Patrick Ryan
CDM Version: 5.3
-->

# DEX08: Maximum number of distinct drugs per person over some time period

## Description
| This query is to determine the maximum number of distinct drugs a patient is exposed to during a certain time period. If the time period is omitted, the entire time span of the database is considered. See  vocabulary queries for obtaining valid drug_concept_id values.

## Query
The following is a sample run of the query. The input parameters are highlighted in  blue. s

```sql
select max(drugs) as drugs_count from
(SELECT
COUNT( DISTINCT drug_concept_id) drugs
FROM @cdm.drug_exposure JOIN @cdm.person
ON drug_exposure.person_id = person.person_id
WHERE
drug_concept_id in (select distinct concept_id from @vocab.concept
                        WHERE domain_id='Drug' and standard_concept='S')
 AND drug_exposure_start_date >= DATEFROMPARTS(2017, 01, 01)
AND drug_exposure_start_date <= DATEFROMPARTS(2017, 12, 31)
GROUP BY drug_exposure.person_id) EV;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| date from | 01-Jan-2008 | Yes |   |
| date to | 31-Dec-2008 | Yes |   |


## Output

|  Field |  Description |
| --- | --- |
| drugs_count | The maximum number of distinct drugs a patient is exposed to during the time period |

## Example output record

|  Field |  Content |
| --- | --- |
| drugs_count | 141 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
