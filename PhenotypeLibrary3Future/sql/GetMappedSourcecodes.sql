--get all source codes that map to the list of concepts provided
SELECT cr.CONCEPT_ID_2 AS STANDARD_CONCEPT_ID,
  CONCEPT_ID,
	CONCEPT_NAME,
	ISNULL(STANDARD_CONCEPT, 'N') STANDARD_CONCEPT,
	ISNULL(c.INVALID_REASON, 'V') INVALID_REASON,
	CONCEPT_CODE,
	CONCEPT_CLASS_ID,
	DOMAIN_ID,
	VOCABULARY_ID
FROM @vocabulary_database_schema.concept_relationship cr
JOIN @vocabulary_database_schema.concept c ON c.concept_id = cr.concept_id_1
WHERE cr.concept_id_2 IN (@conceptsIdsToGetMapped)
	AND cr.INVALID_REASON IS NULL
	AND relationship_id IN ('Maps to');