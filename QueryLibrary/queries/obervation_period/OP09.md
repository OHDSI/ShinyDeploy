<!---
Group:observation period
Name:OP09 Observation period records per person
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP09: Observation period records per person

## Description
List all people (person_id) who has specific number of observations. The input to the query is a value (or a comma-separated list of values) for a record count.

## Query
```sql
SELECT p.person_id, count(1) observation_period_count
FROM @cdm.observation_period p
GROUP BY p.person_id
having count(1) = 3;
```

## Input

None

## Output

| Field |  Description |
| --- | --- |
| person_id | Person identifier |
| observation_period_count | Number of periods |

## Example output record

|  Field |  Value |
| --- | --- |
| person_id |  826002 |
| observation_period_count |  3 |


## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
