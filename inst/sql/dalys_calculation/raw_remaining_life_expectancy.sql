--- raw life table
SELECT max(touchstone.version) AS touchstone_version, country, year, age_from, gender, value
FROM demographic_statistic
JOIN touchstone_demographic_dataset
ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
JOIN touchstone
ON touchstone.id = touchstone_demographic_dataset.touchstone
JOIN demographic_statistic_type
ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
JOIN gender
ON gender.id = demographic_statistic.gender
WHERE demographic_statistic_type.code = 'life_ex'
AND touchstone.touchstone_name = $1
AND gender.code = 'both'
GROUP BY country, year, age_from,gender,value
