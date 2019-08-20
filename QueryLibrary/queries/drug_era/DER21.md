<!---
Group:drug era
Name:DER21 Counts of drugs, stratified by year, age group and gender
Author:Patrick Ryan
CDM Version: 5.3
-->

# DER21: Counts of drugs, stratified by year, age group and gender

## Description
This query is used to count drugs (drug_concept_id) across all drug era records stratified by year, age group and gender (gender_concept_id). The age groups are calculated as 10 year age bands from the age of a person at the drug era start date. The input to the query is a value (or a comma-separated list of values) of a drug_concept_id , year, age_group (10 year age band) and gender_concept_id. If the input is omitted, all existing value combinations are summarized.

## Query
```sql
SELECT
  tt.drug_concept_id,
  count(1) as s_count,
  tt.age_band,
  tt.year_of_Era,
  tt.gender_concept_id
from (
  SELECT
    floor( (YEAR(t.drug_era_start_date ) - p.year_of_birth )/10 ) as age_band,
        YEAR(t.drug_era_start_date) as year_of_era,
        p.gender_concept_id,
        t.drug_concept_id
  FROM
    @cdm.drug_era t,
    @cdm.person p
  where
    t.person_id = p.person_id and
    t.drug_concept_id in (1300978, 1304643, 1549080)
) tt
where
  tt.age_band in(3,4) and
  tt.year_of_Era in( 2007, 2008)
group by
  tt.age_band,
  tt.year_of_Era,
  tt.gender_concept_id,
  tt.drug_concept_id
order by
  tt.age_band,
  tt.year_of_Era,
  tt.gender_concept_id,
  tt.drug_concept_id
;
```

## Input

| Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of concept_id | 1300978, 1304643, 1549080 | Yes |   |
| list of year_of_era | 2007, 2008 | Yes |   |

## Output

| Field |  Description |
| --- | --- |
| drug_concept_id | A foreign key that refers to a standard concept identifier in the vocabulary for the drug concept. |
| s_count | Count of drug group by age and gender |
| age_band | The number of individual drug exposure occurrences used to construct the drug era. |
| year_of_era | A foreign key to the predefined concept identifier in the vocabulary reflecting the type of drug exposure recorded. It indicates how the drug exposure was represented in the source data: as medication history, filled prescriptions, etc. |
| gender_concept_id | A foreign key that refers to a standard concept identifier in the vocabulary for the gender of the person. |

## Example output record

|  Field |  Description |
| --- | --- |
| drug_concept_id | 1304643 |
| s_count | 5 |
| age_band | 3 |
| year_of_era | 2007 |
| gender_concept_id | 8507 |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
