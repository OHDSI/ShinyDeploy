SELECT ANCESTOR_CONCEPT_ID,
  CONCEPT_ID,
	CONCEPT_NAME,
	ISNULL(STANDARD_CONCEPT, 'N') STANDARD_CONCEPT,
	ISNULL(INVALID_REASON, 'V') INVALID_REASON,
	CONCEPT_CODE,
	CONCEPT_CLASS_ID,
	DOMAIN_ID,
	VOCABULARY_ID,
	'Descendant' RELATIONSHIP_NAME,
	MIN_LEVELS_OF_SEPARATION RELATIONSHIP_DISTANCE
FROM @vocabulary_database_schema.concept_ancestor ca
JOIN @vocabulary_database_schema.concept c ON c.concept_id = ca.descendant_concept_id
WHERE ancestor_concept_id IN (@conceptsIdsToGetDescendants);