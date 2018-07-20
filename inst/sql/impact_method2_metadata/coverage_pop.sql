-- Parameters:
-- 1: the touchstone id (including version, e.g., 201710gavi-1)
-- 2: year_min: vaccination activity calander year from
-- 3: year_max: vaccination activity calander year to
----------------------------------------------------------------------------------
SELECT coverage_fvps.*, un_population.value AS population
----------------------------------------------------------------------------------
FROM
----------------------------------------------------------------------------------
---- bridge table 1
---- coverage_fvps: coverage and fvps disaggregated by country/year/age/gender
(SELECT DISTINCT vaccine, activity_type,
coverage.country, year, gavi_support,
gender.name as gender, (age_from + i - 1) as age, target, coverage
FROM coverage_set
JOIN coverage
  ON coverage.coverage_set = coverage_set.id
JOIN gender
  ON gender.id = coverage.gender
JOIN num
  ON num.i <= (age_to - age_from + 1)
WHERE touchstone = $1
AND gavi_support_level IN ('with', 'bestminus')
%1s
  AND vaccine NOT IN ('none', 'DTP3')
  AND year BETWEEN $2 AND $3) As coverage_fvps
----------------------------------------------------------------------------------
LEFT JOIN
----------------------------------------------------------------------------------
---- bridge table 2
---- un_population: unwpp population by country/year/age/gender
(SELECT country, year, age_from as age, gender.name AS gender, value
FROM demographic_statistic
JOIN touchstone_demographic_dataset
  ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
JOIN demographic_statistic_type
  ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
JOIN gender
  ON gender.id = demographic_statistic.gender
WHERE touchstone_demographic_dataset.touchstone = $4
%2s
  AND demographic_statistic_type.code = 'int_pop'
  AND year BETWEEN $2 AND $3) AS un_population
----------------------------------------------------------------------------------
  ON (coverage_fvps.country = un_population.country
 AND coverage_fvps.year = un_population.year
 AND coverage_fvps.gender = un_population.gender
 AND coverage_fvps.age = un_population.age)
----------------------------------------------------------------------------------
ORDER BY vaccine, activity_type, coverage_fvps.country, year, age, gender
