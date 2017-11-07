SELECT coverage_set,
       coverage.country,
       coverage.year,
       coverage,
       target
  FROM coverage
 ORDER BY coverage_set, country, year
