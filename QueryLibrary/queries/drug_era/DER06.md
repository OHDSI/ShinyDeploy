<!---
Group:drug era
Name:DER06 For a given class, what proportion of patients take each treatment in the class?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER06: For a given class, what proportion of patients take each treatment in the class?

## Description
## Query
```sql
select        tt.concept_id,
                tt.concept_name,
                100*(tt.cntPersons*1.0/tt.total*1.0) as proportion_count
from
        (
        select        c.concept_id,
                        c.concept_name,
                        t.cntPersons,
                        sum(cntPersons) over() as total
        from        @vocab.concept c,
                        (
                        select        r.drug_concept_id,
                                        count(distinct r.person_id) as cntPersons
                        FROM        @vocab.concept_ancestor ca,
                                        @cdm.drug_era r
                        WHERE
                                ca.ancestor_concept_id        = 4324992
                        AND        r.drug_concept_id                = ca.descendant_concept_id
                        group by
                                r.drug_concept_id
                        ) t
        where
                t.drug_concept_id = c.concept_id
        ) tt;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| ancestor_concept_id | 4324992 | Yes | Antithrombins |

## Output

|  Field |  Description |
| --- | --- |
| Concept_id | Unique identifier for drug concept |
| Concept_name | Standardized drug name |
| Proportion_count | Proportion of patients take each treatment in the class |

## Example output record

|  Field |  Description |
| --- | --- |
| Concept_id | 1301025 |
| Concept_name | Enoxaparin |
| Proportion_count | 90.94584530269177500 |



## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
