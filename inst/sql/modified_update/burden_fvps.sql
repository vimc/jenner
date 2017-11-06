SELECT burden_estimate_set,
       burden_estimate.country,
       burden_estimate.year,
       value
  FROM burden_estimate
  JOIN burden_outcome
    ON burden_outcome.id = burden_estimate.burden_outcome
 WHERE burden_outcome.code = 'fvps'
--   AND year BETWEEN 2001 AND 2016
--   AND country IN ('KEN', 'ZWE')
ORDER BY burden_estimate_set, country, year
