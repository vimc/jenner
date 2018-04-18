--- responsibilities
--- I might need to change a way of generating responsibilities, because of the is_open thing.
SELECT
responsibility.current_burden_estimate_set AS burden_estimate_set_id,
responsibility.current_stochastic_burden_estimate_set AS stochastic_burden_estimate_set_id,
scenario_description.disease,
responsibility_set.modelling_group,
responsibility_set.touchstone,
max(touchstone.version),
scenario_description.id AS scenario

FROM responsibility
JOIN responsibility_set
ON responsibility_set.id = responsibility_set
JOIN scenario
ON scenario.id = responsibility.scenario
JOIN scenario_description
ON scenario_description.id = scenario.scenario_description
JOIN touchstone
ON touchstone.id = scenario.touchstone
WHERE is_open
AND touchstone.touchstone_name = $1
GROUP BY responsibility.current_burden_estimate_set, responsibility.current_stochastic_burden_estimate_set,
scenario_description.disease, responsibility_set.modelling_group, responsibility_set.touchstone, scenario_description.id
