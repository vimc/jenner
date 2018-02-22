SELECT tab1.*,
tab2.value AS population 
FROM
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
AND vaccine NOT IN ('none', 'PCV', 'DTP3') 
AND year BETWEEN $2 AND $3) As tab1
LEFT JOIN
(SELECT country, year, age_from as age, gender.name AS gender, value
FROM demographic_statistic
JOIN touchstone_demographic_dataset
ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
JOIN demographic_statistic_type
ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
JOIN gender 
ON gender.id = demographic_statistic.gender
WHERE touchstone_demographic_dataset.touchstone = $1
AND demographic_statistic_type.code = 'int_pop'
AND year BETWEEN $2 AND $3) AS tab2
ON (tab1.country = tab2.country
AND tab1.year = tab2.year
AND tab1.gender = tab2.gender
AND tab1.age = tab2.age)
ORDER BY vaccine, activity_type, tab1.country, year, age, gender
