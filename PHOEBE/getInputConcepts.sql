WITH 
ss AS (
	SELECT DISTINCT u.*
	FROM @target_database_schema.universe u -- in case standard_concept changed over time
	JOIN @vocabulary_database_schema.concept_synonym cs
		ON cs.concept_id = u.concept_id
	WHERE u.standard_concept = 'S'
		AND LOWER(u.concept_name) ~* '@source_string'
{@source_domain == ""} ? {
		AND u.domain_id IN ('Condition', 'Procedure', 'Drug', 'Measurement', 'Observation')
} : {
		AND LOWER(u.domain_id) = LOWER('@source_domain')
}
	
	UNION
	
	SELECT DISTINCT u.*
	FROM @target_database_schema.universe u -- in case standard_concept changed over time
	JOIN @vocabulary_database_schema.concept_synonym cs
		ON cs.concept_id = u.concept_id
	WHERE u.standard_concept = 'S'
		AND LOWER(u.concept_name) ~* '@source_string'
{@source_domain == ""} ? {
		AND u.domain_id IN ('Condition', 'Procedure', 'Drug', 'Measurement', 'Observation')
} : {
		AND LOWER(u.domain_id) = LOWER('@source_domain')
}
	)
SELECT *
FROM ss
WHERE NOT EXISTS (
		SELECT 1
		FROM @vocabulary_database_schema.concept_ancestor ca
		JOIN ss s2
			ON ca.descendant_concept_id = s2.concept_id
				AND ca.min_levels_of_separation > 0
				AND concept_id = ca.ancestor_concept_id
		)
ORDER BY drc DESC 
{@source_domain != ""} ? {LIMIT 1};
