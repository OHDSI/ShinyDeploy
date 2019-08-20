<!---
Group:drug era
Name:DER02 What is cost of ERA? - era -> exposure -> cost
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER02: What is cost of ERA? - era -> exposure -> cost

## Description
This query is used to count all gender values (gender_concept_id) for all exposed persons stratified by drug (drug_concept_id). The input to the query is a value (or a comma-separated list of values) of a gender_concept_id and drug_concept_id. If the input is omitted, all existing value combinations are summarized.

## Query
```sql
SELECT        sum(ISNULL(c.total_paid, 0)) as total_cost4era
FROM        @cdm.drug_exposure e,
            @cdm.cost c
WHERE
        exists
                (
                select        1
                from        @cdm.drug_era r,
                            @vocab.concept_ancestor m
                where
                        r.drug_era_id = 20--&era_id
                        and r.person_id = e.person_id
                        and r.drug_concept_id = m.ancestor_concept_id
                        and e.drug_concept_id = m.descendant_concept_id
                        and (e.drug_exposure_start_date >= r.drug_era_start_date) AND (e.drug_exposure_start_date <= r.drug_era_end_date)
                )
AND e. drug_exposure_id = c.cost_event_id
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_era_id | 20 | Yes |   |

## Output

| Field |  Description |
| --- | --- |
| Total_cost4era | Total cost for drug era |

## Example output record

| Field |  Description |
| --- | --- |
| Total_cost4era | 25.23 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
