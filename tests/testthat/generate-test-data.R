test_data <- function (con, con_test, modelling_group = "PSU-Ferrari", vaccine_focal = "MCV1", vaccine_base = "none", year_min = 2000, year_max = 2030) {

  countries <- jenner::sql_in(c("PAK", "IND", "NGA", "ETH"))
  sql_meta <- c("SELECT meta2.* from
                (SELECT touchstone.touchstone_name, max(touchstone.version) as version, modelling_group, scenario.scenario_description
                FROM responsibility
                JOIN responsibility_set
                ON responsibility_set.id = responsibility.responsibility_set
                JOIN scenario
                ON scenario.id = responsibility.scenario
                JOIN scenario_description on scenario_description.id = scenario.scenario_description
                JOIN touchstone
                ON touchstone.id = responsibility_set.touchstone
                WHERE touchstone_name = '201710gavi'
                GROUP BY touchstone.touchstone_name, modelling_group, scenario.scenario_description) as meta

                join touchstone
                on touchstone.touchstone_name = meta.touchstone_name
                and touchstone.version = meta.version
                JOIN ( select responsibility_set.touchstone, modelling_group, scenario_description, vaccine, activity_type,
                responsibility.id as responsibility_id, scenario.id as sceanrio_id, current_burden_estimate_set
                FROM responsibility
                JOIN responsibility_set
                ON responsibility_set.id = responsibility.responsibility_set
                JOIN scenario
                ON scenario.id = responsibility.scenario
                join coverage_set on coverage_set.id = scenario.focal_coverage_set
                where current_burden_estimate_set IS NOT NULL
                ) as meta2
                ON meta2.touchstone = touchstone.id
                and meta2.modelling_group = meta.modelling_group
                and meta2.scenario_description = meta.scenario_description
                WHERE meta2.modelling_group = '%1s'
                AND meta2.vaccine IN %2s")

  sql_meta <- sprintf(sql_meta, modelling_group, sql_in(c(vaccine_focal, vaccine_base)))
  meta = DBI::dbGetQuery(con, sql_meta)
  if(nrow(meta) == 0L) stop("Unknown model-vaccine combination.")
  i <- duplicated(data.frame(meta$scenario, meta$modelling_group))
  if (any(i)) {stop("duplication in meta not expected")}

  ids <- meta$current_burden_estimate_set[!is.na(meta$current_burden_estimate_set)]
  ### we use all recipes and pine countries as test data
  ### year range is 2000 - 2030 (fvps) and 2000-2100 (burden) for campaign
  ### year range is 2020 - 2030 (fvps and burden) for routine
  ## when calculate impact:
  ## do routine only - because campaign impact uses all coverage and all impact, too big for a test
  sql_burden <- paste(sprintf("SELECT * FROM burden_estimate
                              WHERE burden_estimate_set
                              IN %s", sql_in(ids, text_item =FALSE)),
                      sprintf("AND country IN %s", countries),
                      sprintf("AND year BETWEEN $1 AND $2")
  )
  burden_lite <- DBI::dbGetQuery(con, sql_burden, list(year_min, year_max))
  input_lite <- jenner::fix_coverage_fvps(con, year_min = year_min, year_max = year_max, pine = TRUE, write_table = FALSE)
  input_lite <- input_lite[input_lite$vaccine == vaccine_focal, ]
  vaccine_routine_age <- DBI::dbReadTable(con, "vaccine_routine_age")
  DBI::dbWriteTable(con_test, "burden_estimate", burden_lite, overwrite = TRUE)
  DBI::dbWriteTable(con_test, "temporary_coverage_fvps", input_lite, overwrite = TRUE)
  DBI::dbWriteTable(con_test, "vaccine_routine_age", vaccine_routine_age, overwrite = TRUE)
}
