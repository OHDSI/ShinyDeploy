<!---
Group:drug cost
Name:DRC03 What is out-of-pocket cost for a given drug?
Author:Patrick Ryan
CDM Version: 5.3
-->

# DRC03: What is out-of-pocket cost for a given drug?

## Description
Determine the out-of-pocket cost out of a list of drugs.

## Query
```sql
SELECT d.drug_concept_id, avg(c.paid_by_patient - c.paid_patient_copay) AS avg_out_pocket_cost
FROM @cdm.cost c
JOIN @cdm.drug_exposure d
  ON d.drug_exposure_id = c.cost_event_id
WHERE d.drug_exposure_id = c.cost_event_id
  AND (c.paid_by_patient - c.paid_patient_copay) > 0
  AND d.drug_concept_id
    IN (906805, 1517070, 19010522)
GROUP BY d.drug_concept_id;
```

## Input

|  Parameter |  Example |  Mandatory |  Notes |
| --- | --- | --- | --- |
| list of drug_concept_id | 906805, 1517070, 19010522 | Yes |   |

## Output

|  Field |  Description |
| --- | --- |
| drug_concept_id | The drug concept ID |
| avg_out_pocket_cost | The average amount paid by the person as a share of the expenses, excluding the copay. |

## Example output record

|   |  |
| --- | --- |
| Field |  Description |
| drug_concept_id | 19010522  |
| avg_out_pocket_cost | 40.32  |

## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/COST
