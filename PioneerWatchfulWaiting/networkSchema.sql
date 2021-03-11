CREATE TABLE charybdis.time_window
(
    time_window_id integer NOT NULL,
    time_window_name character varying COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT time_window_pkey PRIMARY KEY (time_window_id)
)

INSERT INTO charybdis.time_window (time_window_id, time_window_name) SELECT 1, '-365d to -1d';
INSERT INTO charybdis.time_window (time_window_id, time_window_name) SELECT 2, '-30d to -1d';
INSERT INTO charybdis.time_window (time_window_id, time_window_name) SELECT 3, '0d to 0d';
INSERT INTO charybdis.time_window (time_window_id, time_window_name) SELECT 4, '0d to 30d';

DROP TABLE charybdis.cohort_xref;
CREATE TABLE charybdis.cohort_xref (
	cohort_id varchar(100),
	target_id varchar(100),
	target_name varchar(4000),
	strata_id varchar(100),
	strata_name varchar(4000),
	cohort_type varchar(4000)
)
;

-- INSERT DATA FROM SPREADSHEET

DROP TABLE charybdis.cohort;
CREATE TABLE charybdis.cohort (
	cohort_id BIGINT,
	target_id BIGINT,
	target_name varchar(4000),
	strata_id BIGINT,
	strata_name varchar(4000),
	cohort_type varchar(4000)
)
;

INSERT INTO charybdis.cohort (
	cohort_id,
	target_id,
	target_name,
	strata_id,
	strata_name,
	cohort_type
)
SELECT 
	CAST(cohort_id AS BIGINT) cohort_id,
	CAST(target_id AS BIGINT) target_id,
	target_name,
	CAST(strata_id AS BIGINT) strata_id,
	strata_name,
	cohort_type
FROM charybdis.cohort_xref
;

TRUNCATE TABLE charybdis.cohort_xref;
DROP TABLE charybdis.cohort_xref;


-- Clean up covariates
SELECT COUNT(distinct covariate_id) FROM charybdis.covariate_mark
UNION ALL
SELECT COUNT(distinct covariate_id) FROM charybdis.covariate;

-- Handle the unique values first
select c.covariate_id, c.covariate_name, c.covariate_analysis_id
into charybdis.covariate_clean
from charybdis.covariate c
INNER JOIN (
	SELECT covariate_id from charybdis.covariate group by covariate_id having count(*) = 1
) d ON c.covariate_id = d.covariate_id

-- Next handle the non-unique cohort covariates
INSERT INTO charybdis.covariate_clean (covariate_id, covariate_name, covariate_analysis_id)
SELECT c.covariate_id, c.covariate_name, c.covariate_analysis_id
FROM charybdis.covariate c
LEFT JOIN charybdis.covariate_clean cc ON cc.covariate_id = c.covariate_id
where cc.covariate_id is null
  and c.covariate_name ilike 'cohort%'
  and c.covariate_analysis_id = 10000
order by c.covariate_id, c.covariate_name
;

-- Next handle the non-unique drug/condition covariates
INSERT INTO charybdis.covariate_clean (covariate_id, covariate_name, covariate_analysis_id)
SELECT c.covariate_id, c.covariate_name, c.covariate_analysis_id
FROM charybdis.covariate c
LEFT JOIN charybdis.covariate_clean cc ON cc.covariate_id = c.covariate_id
LEFT JOIN charybdis.covariate_mark cm ON cm.covariate_id = c.covariate_id
where cc.covariate_id is null
  and cm.mark = 'ASCII'
order by c.covariate_id, c.covariate_name
;

-- Verify that all covariates are now in the "clean" table
SELECT COUNT(*)
FROM (
	SELECT DISTINCT covariate_id FROM charybdis.covariate
) AS A
LEFT JOIN 
(
	SELECT DISTINCT covariate_id FROM charybdis.covariate_mark
) B ON A.covariate_id = B.covariate_id
WHERE B.covariate_id IS NULL
;

-- MAKE THE charybdis.covariate_clean the charybdis.covariate table
TRUNCATE TABLE charybdis.covariate;
DROP TABLE charybdis.covariate;

SELECT DISTINCT covariate_id, covariate_name, covariate_analysis_id, cast(right(cast(covariate_id as varchar), 1) as int) time_window_id
INTO charybdis.covariate
FROM charybdis.covariate_clean
;



TRUNCATE TABLE charybdis.covariate_mark;
DROP TABLE charybdis.covariate_mark;
TRUNCATE TABLE charybdis.covariate_clean;
DROP TABLE charybdis.covariate_clean;


SELECT c.covariate_id, c.covariate_name, c.covariate_analysis_id
FROM charybdis.covariate c
LEFT JOIN charybdis.covariate_clean cc ON cc.covariate_id = c.covariate_id
LEFT JOIN charybdis.covariate_mark cm ON cm.covariate_id = c.covariate_id
where cc.covariate_id is null
  and cm.mark = 'ASCII'
order by c.covariate_id, c.covariate_name


ALTER TABLE charybdis.cohort ADD PRIMARY KEY (cohort_id);
CREATE INDEX idx_cohort_target_id ON charybdis.cohort (target_id);
CREATE INDEX idx_cohort_target_name ON charybdis.cohort (target_name);
CREATE INDEX idx_cohort_strata_id ON charybdis.cohort (strata_id);
CREATE INDEX idx_cohort_strata_name ON charybdis.cohort (strata_name);

ALTER TABLE charybdis.cohort_count ADD PRIMARY KEY (cohort_id, database_id);
ALTER TABLE charybdis.covariate ADD PRIMARY KEY (covariate_id, covariate_name);
CREATE INDEX idx_covariate_time_window_id ON charybdis.covariate (time_window_id);

ALTER TABLE charybdis.covariate_value ADD PRIMARY KEY (cohort_id, covariate_id, database_id);
CREATE INDEX idx_cv_cohort_id ON charybdis.covariate_value (cohort_id);
CREATE INDEX idx_cv_covariate_id ON charybdis.covariate_value (covariate_id);
CREATE INDEX idx_cv_database_id ON charybdis.covariate_value (database_id);

ALTER TABLE charybdis.database ADD PRIMARY KEY (database_id);