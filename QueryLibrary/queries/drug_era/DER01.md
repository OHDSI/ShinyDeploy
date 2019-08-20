<!---
Group:drug era
Name:DER01 Which drug_exposure records belong to a drug_era?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER01: Which drug_exposure records belong to a drug_era?

## Description
This query is used to count all gender values (gender_concept_id) for all exposed persons stratified by drug (drug_concept_id). The input to the query is a value (or a comma-separated list of values) of a gender_concept_id and drug_concept_id. If the input is omitted, all existing value combinations are summarized.

## Query
```sql
select *
 from        @cdm.drug_exposure e
 where
         exists
                (
                select 1
                from
                        @cdm.drug_era r ,
                        @vocab.concept_ancestor m
                where
                        r.drug_era_id = 20
                and r.person_id = e.person_id
                and r.drug_concept_id = m.ancestor_concept_id
                and e.drug_concept_id = m.descendant_concept_id
                and (e.drug_exposure_start_date >= r.drug_era_start_date) AND (e.drug_exposure_start_date <= r.drug_era_end_date)
                )
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| drug_era_id | 20 | Yes |   |

## Output

| Field |  Description |
| --- | --- |
| drug_exposure_id | A system-generated unique identifier for each drug exposure. |
| person_id | A foreign key identifier to the person who is subjected to the drug during the drug era. The demographic details of that person are stored in the person table. |
| drug_exposure_start_date | The start date for the current instance of drug utilization. Valid entries include a start date of a prescription, the date a prescription was filled, or the date on which a drug administration procedure was recorded. |
| drug_exposure_end_date | The end date for the current instance of drug utilization. |

## Example output record

|  Field |  Description |
| --- | --- |
| drug_exposure_id | 3052353648 |
| person_id | 690809963 |
| drug_exposure_start_date | 2014-05-01 00:00:00 |
| drug_exposure_end _date | 2014-05-01 00:00:00 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
