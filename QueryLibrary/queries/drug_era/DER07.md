<!---
Group:drug era
Name:DER07 What is the average time between eras for a given ingredient? ex. steroids for RA
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER07: What is the average time between eras for a given ingredient? ex. steroids for RA

## Description

## Query
```sql
select
        avg(datediff(day, t.drug_era_end_date, t.next_era_start)) as num_days
from
        (
                select
                        r.drug_era_end_date,
                        lead(r.drug_era_start_date) over(partition by r.person_id, r.drug_concept_id order by r.drug_era_start_date) as next_era_start
                from
                        @cdm.drug_era r
                where r.drug_concept_id = 1304643
        ) t
where
        t.next_era_start is not null
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_concept_id | 1304643 | Yes | darbepoetin alfa |

## Output

|  Field |  Description |
| --- | --- |
| Num_days |  Average number of days between drug eras |

## Example output record

|  Field |  Value |
| --- | --- |
| Num_days |  82 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
