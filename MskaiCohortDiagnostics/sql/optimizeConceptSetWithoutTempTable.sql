{DEFAULT @conceptSetConceptIdsExcluded = 0}
{DEFAULT @conceptSetConceptIdsDescendantsExcluded = 0}
{DEFAULT @conceptSetConceptIdsNotExcluded = 0}
{DEFAULT @conceptSetConceptIdsDescendantsNotExcluded = 0}

WITH conceptSetConceptsNotExcluded
AS (
	-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y or N"
	SELECT DISTINCT concept_id,
		standard_concept,
		invalid_reason
	FROM @vocabulary_database_schema.concept
	WHERE concept_id IN (@conceptSetConceptIdsNotExcluded)
	),
conceptSetConceptsDescendantsNotExcluded
AS (
	-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y"
	SELECT DISTINCT ancestor_concept_id,
		descendant_concept_id concept_id
	FROM @vocabulary_database_schema.concept_ancestor
	WHERE ancestor_concept_id IN (@conceptSetConceptIdsDescendantsNotExcluded)
		AND ancestor_concept_id != descendant_concept_id -- Exclude the selected ancestor itself
	),
conceptSetConceptsNonStandardMappedNotExcluded
AS (
	SELECT cr.concept_id_1 concept_id,
		cr.concept_id_2 concept_id_non_standard
	FROM @vocabulary_database_schema.concept_relationship cr
	INNER JOIN conceptSetConceptsNotExcluded ON concept_id = concept_id_1
		AND relationship_id = 'Maps to'
	WHERE standard_concept IS NULL
		AND cr.invalid_reason IS NULL
	), 
conceptSetConceptsExcluded
AS (
	-- Concepts that are part of the concept set definition that are "EXCLUDED = Y, DECENDANTS = Y or N"
	SELECT DISTINCT concept_id,
		standard_concept,
		invalid_reason
	FROM @vocabulary_database_schema.concept
	WHERE concept_id IN (@conceptSetConceptIdsExcluded)
	),
conceptSetConceptsDescendantsExcluded
AS (
	-- Concepts that are part of the concept set definition that are "EXCLUDED = Y, DECENDANTS = Y"
	SELECT DISTINCT ancestor_concept_id,
		descendant_concept_id concept_id
	FROM @vocabulary_database_schema.concept_ancestor
	WHERE ancestor_concept_id IN (@conceptSetConceptIdsDescendantsExcluded)
		AND ancestor_concept_id != descendant_concept_id -- Exclude the selected ancestor itself
	),
conceptSetsIncluded
AS (
	SELECT a.concept_id original_concept_id,
	    a.invalid_reason,
		c1.concept_name original_concept_name,
		b.ancestor_concept_id,
		c3.concept_name ancestor_concept_name,
		d.concept_id mapped_concept_id,
		c4.concept_name mapped_concept_name,
		ISNULL(b.concept_id, d.concept_id) subsumed_concept_id,
		ISNULL(c2.concept_name, c4.concept_name) subsumed_concept_name
	FROM conceptSetConceptsNotExcluded a
	LEFT JOIN conceptSetConceptsDescendantsNotExcluded b ON a.concept_id = b.concept_id
	LEFT JOIN conceptSetConceptsNonStandardMappedNotExcluded d ON a.concept_id = d.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c1 ON a.concept_id = c1.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c2 ON b.concept_id = c2.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c3 ON b.ancestor_concept_id = c3.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c4 ON d.concept_id = c4.concept_id
	),
conceptSetsExcluded
AS (
	SELECT a.concept_id original_concept_id,
	    a.invalid_reason,
		c1.concept_name original_concept_name,
		b.ancestor_concept_id,
		c3.concept_name ancestor_concept_name,
		cast(NULL as int) mapped_concept_id,
		cast(NULL as VARCHAR(255)) mapped_concept_name,
		b.concept_id subsumed_concept_id,
		c2.concept_name subsumed_concept_name
	FROM conceptSetConceptsExcluded a
	LEFT JOIN conceptSetConceptsDescendantsExcluded b ON a.concept_id = b.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c1 ON a.concept_id = c1.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c2 ON b.concept_id = c2.concept_id
	LEFT JOIN @vocabulary_database_schema.concept c3 ON b.ancestor_concept_id = c3.concept_id
	),
conceptSetOptimized
AS (
	SELECT original_concept_id concept_id,
		original_concept_name concept_name,
	    invalid_reason,
		cast(0 as int) excluded,
		cast(0 as int) removed
	FROM conceptSetsIncluded
	WHERE subsumed_concept_id IS NULL	
	union	
	SELECT original_concept_id concept_id,
		original_concept_name concept_name,
	    invalid_reason,
		cast(1 as int) excluded,
		cast(0 as int) removed
	FROM conceptSetsExcluded
	WHERE subsumed_concept_id IS NULL
	),
conceptSetRemoved
AS (
	SELECT DISTINCT subsumed_concept_id concept_id,
		subsumed_concept_name concept_name,
	    invalid_reason,
		cast(0 as int) excluded,
		cast(1 as int) removed
	FROM conceptSetsIncluded
	WHERE subsumed_concept_id IS NOT NULL
	union
	SELECT DISTINCT subsumed_concept_id concept_id,
		subsumed_concept_name concept_name,
	    invalid_reason,
		cast(1 as int) excluded,
		cast(1 as int) removed
	FROM conceptSetsExcluded
	WHERE subsumed_concept_id IS NOT NULL
	)
SELECT *
FROM conceptSetOptimized
UNION
SELECT *
FROM conceptSetRemoved;