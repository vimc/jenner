context("impact_method2")

test_that("impact_calculation, method1", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname=":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })

  ## build test data
  message("Can only test routine at the moment.")
  modelling_group <- "Harvard-Sweet"
  vaccine_focal <- "HPV"
  vaccine_base <- "HPV"
  year_min <- 2020
  year_max <- 2030
  method <- "method1"

  meta <- prepare_recipe(con, recipe = "impact.csv") # the .csv is needed at least for now, I am afraid.
  meta2 <- meta[meta$modelling_group == modelling_group &
                meta$vaccine == vaccine_focal &
                meta$activity_type == "routine", ]

  import_test_data_impact_method2(con = con, con_test = con_test, modelling_group = modelling_group, vaccine_focal = vaccine_focal, vaccine_base = vaccine_base, year_min = year_min, year_max = year_max)

  burden <- DBI::dbReadTable(con_test, "burden_estimate")
  ## mannually calculate impact
  tot_impact <- merge(meta2, burden, by.x = c("burden_estimate_set_id", "burden_outcome_id"), by.y = c("burden_estimate_set", "burden_outcome"))
  tot_impact$value <- tot_impact$value * tot_impact$coef
  tot_impact <- aggregate(value ~ index + country + year + age, data = tot_impact, sum, na.rm=TRUE)
  ## run function
  dat <- impact_calculation(con = con_test, meta = meta2, year_min = year_min, year_max = year_max, method = method)
  a <- dat$impact_full
  a <- a[order(a$index, a$country, a$year, a$age), ]
  b <- tot_impact
  b <- b[order(b$index, b$country, b$year, b$age), ]
  expect_equal(a$impact, b$value)
})

test_that("impact_calculation, method2", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname=":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })

  ## build test data
  message("Can only test routine at the moment.")
  modelling_group <- "Harvard-Sweet"
  vaccine_focal <- "HPV"
  vaccine_base <- "HPV"
  year_min <- 2020
  year_max <- 2030
  method <- "method2"

  meta <- prepare_recipe(con, recipe = "impact.csv") # the .csv is needed at least for now, I am afraid.
  meta2 <- meta[meta$modelling_group == modelling_group & meta$vaccine == vaccine_focal & meta$activity_type == "routine", ]

  import_test_data_impact_method2(con = con, con_test = con_test, modelling_group = modelling_group, vaccine_focal = vaccine_focal, vaccine_base = vaccine_base, year_min = year_min, year_max = year_max)
  burden <- DBI::dbReadTable(con_test, "burden_estimate")
  fvps <- DBI::dbReadTable(con_test, "temporary_coverage_fvps")
  fvps <- fvps[fvps$activity_type == "routine", ]

  ## define shape and run the function
  cohort_min <- year_min
  cohort_max <- year_max
  if( vaccine_focal == "MCV2") {
    cohort_min <- year_min - 2
    cohort_max <- year_max - 2
  }
  if( vaccine_focal == "HPV") {
    cohort_min <- year_min - 9
    cohort_max <- year_max - 9
  }
  dat <- impact_calculation(con = con_test, meta = meta2, year_min = year_min, year_max = year_max, method = "method2")
  ## mannual impact calculation
  tot_impact <- merge(meta2, burden, by.x = c("burden_estimate_set_id", "burden_outcome_id"), by.y = c("burden_estimate_set", "burden_outcome"))
  tot_impact$cohort <- tot_impact$year - tot_impact$age
  tot_impact <- tot_impact[tot_impact$cohort %in% cohort_min:cohort_max, ]
  tot_impact$value <- tot_impact$value * tot_impact$coef
  tot_impact <- aggregate(value ~ index + country, data = tot_impact, sum, na.rm=TRUE)
  tot_fvps <- aggregate(fvps ~ country, data = fvps, sum, na.rm=TRUE)
  tot_rate <- merge(tot_impact, tot_fvps, by = "country")
  tot_rate$tot_rate <- tot_rate$value / tot_rate$fvps
  tot_rate <- tot_rate[c("index", "country", "tot_rate")]
  impact <- merge(fvps, tot_rate, by = c("country"))
  impact$impact <- impact$tot_rate * impact$fvps
  impact$cohort <- impact$year - impact$age

  a <- dat$impact_full
  a <- a[a$support_type == "total", ]
  a <- a[order(a$index, a$country, a$year, a$age), ]

  b <- impact[impact$cohort %in% cohort_min:cohort_max, ]
  b <- b[order(b$index, b$country, b$year, b$age), ]
  expect_equal(a$impact, b$impact)
})
