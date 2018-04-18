--- Join dalys parameter and burden estimates
SELECT burden_estimate_set, country, year, age, burden_outcome, code, value as burden, sub_condition, adjusted_weight, duration
FROM
(SELECT * FROM dalys_parameters
  WHERE burden_estimate_set_id = $1 ) AS parameters
---
LEFT JOIN
---
(SELECT burden_estimate_set, burden_estimate.country, year, age, burden_outcome, value
   FROM burden_estimate
  WHERE burden_estimate_set = $1
    AND year BETWEEN $2 AND $3 ) AS burdens
---
ON burdens.burden_estimate_set = parameters.burden_estimate_set_id
AND burdens.burden_outcome = parameters.burden_outcome_id
