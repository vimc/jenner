SELECT 
responsibility_set.id AS responsibility_set_id,
responsibility.id AS responsibility_id,
responsibility.current_burden_estimate_set AS burden_estimate_set_id,
responsibility.current_stochastic_burden_estimate_set AS stochastic_burden_estimate_set_id,
coverage_set.id AS coverage_set_id,
responsibility_set.touchstone,
responsibility_set.modelling_group,scenario_description.disease,
scenario_description.id AS scenario,
coverage_set.name AS coverage_set

FROM responsibility
JOIN responsibility_set
ON responsibility_set.id = responsibility_set
JOIN scenario
ON scenario.id = responsibility.scenario
JOIN scenario_description
ON scenario_description.id = scenario.scenario_description
JOIN touchstone
ON touchstone.id = scenario.touchstone
JOIN scenario_coverage_set
ON scenario_coverage_set.scenario = scenario.id
JOIN coverage_set
ON coverage_set.id = scenario_coverage_set.coverage_set
