SELECT coverage_set,
       coverage.country,
       coverage.year,
       coverage,
       gavi_support,
       target
  FROM coverage
 ORDER BY coverage_set, country, year
