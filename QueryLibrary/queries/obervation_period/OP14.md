<!---
Group:observation period
Name:OP14 Distribution of age, stratified by gender
Author:Patrick Ryan
CDM Version: 5.3
-->

# OP14: Distribution of age, stratified by gender

## Description
This query is used to provide summary statistics for the age across all observation records stratified by gender (gender_concept_id): the mean, the standard deviation, the minimum, the 25th percentile, the median, the 75th percentile, the maximum and the number of missing values. The age value is defined by the earliest observation date. Age is summarized for all existing gender_concept_id values.

## Query
```sql
WITH t AS 
  ( SELECT 
      p.person_id, 
      ISNULL( concept_name, 'MISSING' ) AS gender,
      YEAR(first_observation_date ) - year_of_birth AS age
    FROM -- person, first observation period date
      ( SELECT 
          person_id,
          MIN( observation_period_start_date ) AS first_observation_date
        FROM @cdm.observation_period
        GROUP BY person_id
      ) AS p
    INNER JOIN @cdm.person 
    ON p.person_id = person.person_id
    LEFT OUTER JOIN @vocab.concept 
    ON person.gender_concept_id = concept.concept_id
    WHERE year_of_birth IS NOT NULL
  )

SELECT
  ordered_data.gender,
  COUNT(*)                                                               AS num_people,
  MIN( age )                                                             AS min_age,
  MAX( age )                                                             AS max_age,
  ROUND( avg( age ), 2 )                                                 AS avg_age,
  ROUND( STDEV( age ), 1 )                                               AS STDEV_age,
  MIN(CASE WHEN order_nr < .25 * population_size THEN 9999 ELSE age END) AS percentile_25,
  MIN(CASE WHEN order_nr < .50 * population_size THEN 9999 ELSE age END) AS median,
  MIN(CASE WHEN order_nr < .75 * population_size THEN 9999 ELSE age END) AS percentile_75
FROM 
 ( SELECT 
    gender                                                               AS gender,
    age                                                                  AS age,
    ROW_NUMBER() OVER (PARTITION BY gender ORDER BY age)                 AS  order_nr
  FROM t
) AS ordered_data
INNER JOIN 
 ( SELECT gender,
    COUNT(*) AS population_size
   FROM t
   GROUP BY gender
) AS population_sizes
 ON ordered_data.gender = population_sizes.gender
GROUP BY ordered_data.gender;
```

## Input

None

## Output

|  Field |  Description |
| --- | --- |
|  gender |  Gender concept name |
|  num_people |  Number of people with specific gender |
|  min_age |  Minimum age across observation of people with specific gender |
|  max_age |  Maximum age across observation of people with specific gender |
|  avg_age |  Average age across observation of people with specific gender |
|  STDEV_age |  Standard deviation of age across observation within specific gender |
|  percentile_25 |  25th percentile age across observation within specific gender |
|  median |  Median age across observation within specific gender |
|  percentile_75 |  75th percentile age across observation within specific gender |

## Example output record

|  Field |  Description |
| --- | --- |
|  gender |  MALE |
|  num_people |  1607472 |
|  min_age |  0 |
|  max_age |  103 |
|  avg_age |  40.78 |
|  STDEV_age |  18.60 |
|  percentile_25 |  29 |
|  median |  45 |
|  percentile_75 |  55 |



## Documentation
https://github.com/OHDSI/CommonDataModel/wiki/
