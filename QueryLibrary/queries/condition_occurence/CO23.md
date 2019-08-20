<!---
Group:condition occurrence
Name:CO23 Distribution of condition occurrence month/year, stratified by condition.
Author:Patrick Ryan
CDM Version: 5.3
-->

# CO23: Distribution of condition occurrence month/year, stratified by condition.

## Description
This query is used to summary statistics of the condition month/year start dates across all condition occurrence records, stratified by condition (condition_concept_id).  The input to the query is a value  of a condition_concept_id.

## Query
```sql
SELECT condition_concept_id,
       concept_name,
       condition_month_year,
       count(*)                   AS occurrences_count
  FROM
      (SELECT condition_concept_id,
              concept_name,
              
              CONCAT(MONTH(condition_start_date), '-', YEAR(condition_start_date))  AS condition_month_year,
              MONTH(condition_start_date)                                           AS m1
        FROM @cdm.condition_occurrence condition
        INNER JOIN @vocab.concept concept
        ON condition.condition_concept_id = concept.concept_id AND condition.condition_concept_id=192279
      ) AS m1
GROUP BY condition_concept_id,
         concept_name,
         condition_month_year,
         m1
ORDER BY m1;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| condition_concept_id | 192279 | Yes | Condition concept identifier for 'Diabetic Nephropathy' |

## Output

|  Field |  Description |
| --- | --- |
| condition_concept_id | Concept identifier for condition |
| condition_name | Meaningful and descriptive name for the concept. |
| condition_month_year | The month/year when the instance of the condition is recorded. |
| occurrences_count |  Number of condition occurrences |

## Example output record

| Field |  Description |
| --- | --- |
| condition_concept_id |  192279 |
| condition_name |  Diabetic nephropathy |
| condition_month_year |  05-2004 |
| occurrences_count |  348 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
