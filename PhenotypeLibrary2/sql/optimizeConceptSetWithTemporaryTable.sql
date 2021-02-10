{DEFAULT @conceptSetConceptIdsExcluded = 0 } 
{DEFAULT @conceptSetConceptIdsDescendantsExcluded = 0 } 
{DEFAULT @conceptSetConceptIdsNotExcluded = 0 } 
{DEFAULT @conceptSetConceptIdsDescendantsNotExcluded = 0 }


IF OBJECT_ID('tempdb..#not_excluded', 'U') IS NOT NULL
	DROP TABLE #not_excluded;
-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y or N"
SELECT DISTINCT concept_id,
	standard_concept,
	invalid_reason
INTO #not_excluded
FROM @vocabulary_database_schema.concept
WHERE concept_id IN (@conceptSetConceptIdsNotExcluded);



IF OBJECT_ID('tempdb..#not_excluded_desc', 'U') IS NOT NULL
	DROP TABLE #not_excluded_desc;
-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y"
SELECT DISTINCT ancestor_concept_id,
	descendant_concept_id concept_id
INTO #not_excluded_desc
FROM @vocabulary_database_schema.concept_ancestor
WHERE ancestor_concept_id IN (@conceptSetConceptIdsDescendantsNotExcluded)
	AND ancestor_concept_id != descendant_concept_id;-- Exclude the selected ancestor itself	



IF OBJECT_ID('tempdb..#not_excl_non_std', 'U') IS NOT NULL
	DROP TABLE #not_excl_non_std;
-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y"
SELECT cr.concept_id_1 concept_id,
	cr.concept_id_2 concept_id_non_standard
INTO #not_excl_non_std
FROM @vocabulary_database_schema.concept_relationship cr
INNER JOIN #not_excluded ON concept_id = concept_id_1
	AND relationship_id = 'Maps to'
WHERE standard_concept IS NULL
	AND cr.invalid_reason IS NULL;



IF OBJECT_ID('tempdb..#excluded', 'U') IS NOT NULL
	DROP TABLE #excluded;
-- Concepts that are part of the concept set definition that are "EXCLUDED = Y, DECENDANTS = Y or N"
SELECT DISTINCT concept_id,
	standard_concept,
	invalid_reason
INTO #excluded
FROM @vocabulary_database_schema.concept
WHERE concept_id IN (@conceptSetConceptIdsExcluded);



IF OBJECT_ID('tempdb..#excluded_desc', 'U') IS NOT NULL
	DROP TABLE #excluded_desc;
-- Concepts that are part of the concept set definition that are "EXCLUDED = Y, DECENDANTS = Y"
SELECT DISTINCT ancestor_concept_id,
	descendant_concept_id concept_id
INTO #excluded_desc
FROM @vocabulary_database_schema.concept_ancestor
WHERE ancestor_concept_id IN (@conceptSetConceptIdsDescendantsExcluded)
	AND ancestor_concept_id != descendant_concept_id;



-- Exclude the selected ancestor itself
IF OBJECT_ID('tempdb..#concepts_included', 'U') IS NOT NULL
	DROP TABLE #concepts_included;
SELECT a.concept_id original_concept_id,
	a.invalid_reason,
	c1.concept_name original_concept_name,
	b.ancestor_concept_id,
	c3.concept_name ancestor_concept_name,
	d.concept_id mapped_concept_id,
	c4.concept_name mapped_concept_name,
	ISNULL(b.concept_id, d.concept_id) subsumed_concept_id,
	ISNULL(c2.concept_name, c4.concept_name) subsumed_concept_name
INTO #concepts_included
FROM #not_excluded a
LEFT JOIN #not_excluded_desc b ON a.concept_id = b.concept_id
LEFT JOIN #not_excl_non_std d ON a.concept_id = d.concept_id
LEFT JOIN @vocabulary_database_schema.concept c1 ON a.concept_id = c1.concept_id
LEFT JOIN @vocabulary_database_schema.concept c2 ON b.concept_id = c2.concept_id
LEFT JOIN @vocabulary_database_schema.concept c3 ON b.ancestor_concept_id = c3.concept_id
LEFT JOIN @vocabulary_database_schema.concept c4 ON d.concept_id = c4.concept_id;



IF OBJECT_ID('tempdb..#concepts_excluded', 'U') IS NOT NULL
	DROP TABLE #concepts_excluded;
SELECT a.concept_id original_concept_id,
	a.invalid_reason,
	c1.concept_name original_concept_name,
	b.ancestor_concept_id,
	c3.concept_name ancestor_concept_name,
	cast(NULL AS INT) mapped_concept_id,
	cast(NULL AS VARCHAR(255)) mapped_concept_name,
	b.concept_id subsumed_concept_id,
	c2.concept_name subsumed_concept_name
INTO #concepts_excluded
FROM #excluded a
LEFT JOIN #excluded_desc b ON a.concept_id = b.concept_id
LEFT JOIN @vocabulary_database_schema.concept c1 ON a.concept_id = c1.concept_id
LEFT JOIN @vocabulary_database_schema.concept c2 ON b.concept_id = c2.concept_id
LEFT JOIN @vocabulary_database_schema.concept c3 ON b.ancestor_concept_id = c3.concept_id;



IF OBJECT_ID('tempdb..#concepts_optimized', 'U') IS NOT NULL
	DROP TABLE #concepts_optimized;
SELECT concept_id,
  concept_name,
  invalid_reason,
  excluded,
  removed
INTO #concepts_optimized
FROM (
	SELECT original_concept_id concept_id,
		original_concept_name concept_name,
		invalid_reason,
		cast(0 AS INT) excluded,
		cast(0 AS INT) removed
	FROM #concepts_included
	WHERE subsumed_concept_id IS NULL
	UNION
	SELECT original_concept_id concept_id,
		original_concept_name concept_name,
		invalid_reason,
		cast(1 AS INT) excluded,
		cast(0 AS INT) removed
	FROM #concepts_excluded
	WHERE subsumed_concept_id IS NULL
	) opt;
	
	

IF OBJECT_ID('tempdb..#concepts_removed', 'U') IS NOT NULL
	DROP TABLE #concepts_removed;
SELECT concept_id,
  concept_name,
  invalid_reason,
  excluded,
  removed
INTO #concepts_removed
FROM (
	SELECT DISTINCT subsumed_concept_id concept_id,
		subsumed_concept_name concept_name,
		invalid_reason,
		cast(0 AS INT) excluded,
		cast(1 AS INT) removed
	FROM #concepts_included
	WHERE subsumed_concept_id IS NOT NULL
	UNION
	SELECT DISTINCT subsumed_concept_id concept_id,
		subsumed_concept_name concept_name,
		invalid_reason,
		cast(1 AS INT) excluded,
		cast(1 AS INT) removed
	FROM #concepts_excluded
	WHERE subsumed_concept_id IS NOT NULL
	) rmv;



IF OBJECT_ID('tempdb..#optimized_set', 'U') IS NOT NULL
	DROP TABLE #optimized_set;
SELECT *
INTO #optimized_set
FROM (
	SELECT concept_id,
		concept_name,
		invalid_reason,
		excluded,
		removed
	FROM #concepts_optimized
	UNION
	SELECT concept_id,
		concept_name,
		invalid_reason,
		excluded,
		removed
	FROM #concepts_removed
	) f;



IF OBJECT_ID('tempdb..#not_excluded', 'U') IS NOT NULL
	DROP TABLE #not_excluded;

IF OBJECT_ID('tempdb..#not_excluded_desc', 'U') IS NOT NULL
	DROP TABLE #not_excluded_desc;

IF OBJECT_ID('tempdb..#not_excl_non_std', 'U') IS NOT NULL
	DROP TABLE #not_excl_non_std;

IF OBJECT_ID('tempdb..#excluded', 'U') IS NOT NULL
	DROP TABLE #excluded;

IF OBJECT_ID('tempdb..#excluded_desc', 'U') IS NOT NULL
	DROP TABLE #excluded_desc;

IF OBJECT_ID('tempdb..#concepts_included', 'U') IS NOT NULL
	DROP TABLE #concepts_included;

IF OBJECT_ID('tempdb..#concepts_excluded', 'U') IS NOT NULL
	DROP TABLE #concepts_excluded;