--- raw life table
SELECT max(touchstone.version) AS touchstone_version, country, year, age_from, gender, value
FROM demographic_statistic
JOIN touchstone_demographic_dataset
ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
JOIN touchstone
ON touchstone.id = touchstone_demographic_dataset.touchstone
WHERE demographic_statistic.demographic_statistic_type = 11
AND touchstone.touchstone_name = $1
AND gender = 1
GROUP BY country, year, age_from,gender,value
