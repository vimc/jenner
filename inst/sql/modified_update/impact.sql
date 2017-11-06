SELECT impact_estimate_set,
       impact_outcome,
       impact_estimate.country,
       impact_estimate.year,
       value
  FROM impact_estimate
  JOIN impact_estimate_set
    ON impact_estimate_set.id = impact_estimate.impact_estimate_set
  JOIN impact_estimate_recipe
    ON impact_estimate_recipe.id = impact_estimate_set.impact_estimate_recipe
--  WHERE year BETWEEN 2001 AND 2016
--    AND country IN ('KEN', 'ZWE')
 ORDER BY impact_estimate_set, impact_outcome, country, year
