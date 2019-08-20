<!---
Group:drug era
Name:DER09 Counts of persons taking drugs
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER09: Counts of persons taking drugs

## Description
This query is used to count the persons with any number of eras with exposure to a certain drug (drug_concept_id) . The input to the query is a value (or a comma-separated list of values) of a drug_concept_id. If the input is omitted, all possible values are summarized.

## Query
```sql
select count(distinct r.person_id) as persons_count
from @cdm.drug_era r
where r.drug_concept_id in (1304643, 1549080);
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 1304643, 1549080 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| persons_count |  Count of persons with any number of eras with exposure to certain drug |

## Example output record

|  Field |  Value |
| --- | --- |
| persons_count |  1658496 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
