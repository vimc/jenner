SELECT coverage_old.touchstone,
coverage_old.vaccine,
coverage_old.activity_type,
coverage_old.id AS coverage_set_id_old,
coverage_new.id AS coverage_set_id_new
FROM (SELECT * FROM coverage_set
      WHERE touchstone != '{{{touchstone_new}}}') AS coverage_old
LEFT JOIN (SELECT coverage_set.* FROM coverage_set
JOIN touchstone ON touchstone.id = coverage_set.touchstone
           WHERE touchstone = '{{{touchstone_new}}}'
           AND (coverage_set.gavi_support_level = 'with'
             OR coverage_set.gavi_support_level = 'bestminus') )AS coverage_new
ON coverage_old.vaccine = coverage_new.vaccine AND
coverage_old.activity_type = coverage_new.activity_type
