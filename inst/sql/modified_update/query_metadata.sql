SELECT touchstone.touchstone_name,
       touchstone.id AS touchstone,
       impact_estimate_recipe.id AS impact_estimate_recipe,
       impact_estimate_recipe.disease,
       impact_estimate_recipe.vaccine,
       impact_estimate_recipe.impact_outcome,
       impact_estimate_recipe.support_type,
       impact_estimate_recipe.activity_type,
       impact_estimate_recipe.focal_ingredient,
       --
       impact_estimate_set.id AS impact_estimate_set,
       impact_estimate_set.focal_coverage_set AS coverage_set,
       --
       burden_estimate_set.id AS burden_estimate_set,
       burden_estimate_set.responsibility,
       --
       coverage_set.name AS coverage_set_name,
       model.id AS model,
       model.modelling_group,
       touchstone_demographic_source.demographic_source,
       vaccine_routine_age.age AS vaccine_routine_age,
       v_migrate_coverage.coverage_set_id_new AS coverage_set_new
  FROM impact_estimate_recipe
  JOIN impact_estimate_set
    ON impact_estimate_set.id =
       impact_estimate_recipe.current_impact_estimate_set
  JOIN burden_estimate_set
    ON burden_estimate_set.id = impact_estimate_set.focal_burden_estimate_set
  JOIN model_version
    ON model_version.id = burden_estimate_set.model_version
  JOIN model
    ON model.id = model_version.model
  JOIN touchstone
    ON touchstone.id = impact_estimate_set.recipe_touchstone
  JOIN touchstone_demographic_source
    ON touchstone_demographic_source.touchstone = touchstone.id
  JOIN coverage_set
    ON coverage_set.id = impact_estimate_set.focal_coverage_set
  JOIN v_migrate_coverage
    ON v_migrate_coverage.coverage_set_id_old = coverage_set.id
  LEFT JOIN vaccine_routine_age
    ON vaccine_routine_age.vaccine = coverage_set.vaccine
  WHERE impact_outcome IN ('deaths_averted', 'cases_averted', 'fvps_added')
  ORDER BY impact_estimate_recipe.id
