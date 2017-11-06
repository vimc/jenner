SELECT demographic_statistic.demographic_source,
       demographic_statistic.country,
       demographic_statistic.year,
       demographic_statistic.value
  FROM demographic_source
  JOIN demographic_statistic
    ON demographic_statistic.demographic_source = demographic_source.id
  JOIN gender
    ON gender.id = demographic_statistic.gender
  JOIN demographic_statistic_type
    ON demographic_statistic_type.id =
         demographic_statistic.demographic_statistic_type
 WHERE gender.code = 'both'
   AND demographic_statistic_type.code = 'tot_pop'
--   AND year BETWEEN 2001 AND 2016
--   AND country IN ('KEN', 'ZWE')
 ORDER BY demographic_source, country, year
