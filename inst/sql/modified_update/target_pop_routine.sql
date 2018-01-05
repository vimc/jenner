SELECT demographic_statistic.demographic_source,
       demographic_statistic.age_from,
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
   AND demographic_statistic_type.code = 'int_pop'
-- This will need expanding if we need further detail but this will
-- hopefully do for now (and will make the queries a little easier to
-- deal with)
   AND age_from IN (0, 2, 9)
--   AND year BETWEEN 2001 AND 2016
--   AND country IN ('KEN', 'ZWE')
 ORDER BY demographic_source, age_from, country, year
