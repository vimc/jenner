context("dalys_calculation")

test_that("duration_weighting", {
  period <- c("year", "day", "month")
  w <- c(1., 1./365, 1./12)
  expect_equal(duration_weighting(period), w)
})

test_that("dalys_calculation", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })

  # parameters - we are testing against Measles - Jit
  touchstone_name <- "201710gavi"
  touchstone <- "201710gavi-5" # at this moment, touchstone is manually filled
  modelling_group <- "PSU-Ferrari"
  year_min <- 2015
  year_max <- 2020
  dalys_parameters <- create_dalys_parameters(con, touchstone_name, vimc_dalys_only = TRUE)
  dalys_parameters <- dalys_parameters[dalys_parameters$modelling_group == modelling_group, ]
  burden_estimate_sets <- sql_in(unique(dalys_parameters$burden_estimate_set_id), text_item = FALSE)
  burden_outcomes <- sql_in(unique(dalys_parameters$burden_outcome_id), text_item = FALSE)

  ##manual calculation
  life_table <- create_dalys_life_table(con, touchstone_name, year_min, year_max)

  dat0 <- DBI::dbGetQuery(con, paste(sprintf("SELECT burden_estimate_set, country.id AS country, year, burden_outcome, value, age FROM burden_estimate
                                      JOIN country ON country.nid = burden_estimate.country
                                      WHERE burden_estimate_set IN %s", burden_estimate_sets),
                                     sprintf("AND burden_outcome IN %s", burden_outcomes),
                                     "AND year between $1 AND $2"), list(year_min, year_max))
  v <- merge(dat0, dalys_parameters, by.x = c("burden_estimate_set", "burden_outcome"),
             by.y=c("burden_estimate_set_id", "burden_outcome_id"))
  v$.code <- paste(v$country, v$year, v$age, sep = "-")
  v <- merge(v, life_table, by = c(".code"))
  v$adj_duration <- v$remainning_life_exp
  i <- v$duration < v$remainning_life_exp
  v$adj_duration[i] <- v$duration[i]
  v$dalys <- v$value * v$adjusted_weight * v$adj_duration
  dat0 <- aggregate(dalys ~ burden_estimate_set + country + year + age, data = v, sum, na.rm = TRUE)
  dat0 <- dat0[order(dat0$burden_estimate_set, dat0$country, dat0$year, dat0$age),]
  ## run function
  dat <- jenner::calculate_dalys(con, touchstone_name, year_min, year_max, modelling_group = modelling_group)
  dat <- dat[order(dat$country, dat$year, dat$age),]
  expect_equal (dat$burden_estimate_set, dat$value, dat0$dalys)
  #saveRDS(dat, "jenner-test-data/dalys_calculation/PSU-Ferrari.rds")
  #expect_known_value(dat, "jenner-test-data/dalys_calculation/PSU-Ferrari.rds",
  #                   update = FALSE, tolerance = 1e-6)
})

test_that("dalys stochastic data matches", {
  con <- test_montagu_readonly_connection()
  create_dalys_parameters(con, "201710gavi", TRUE)

  burden_estimate_set_id <- 735
  burden_outcomes <- "(1,6)"
  year_min <- 2000
  year_max <- 2030

  data <- DBI::dbGetQuery(con, "SELECT * from burden_estimate WHERE burden_estimate_set = 735")
  countries <- DBI::dbGetQuery(con, "SELECT * FROM country")
  data$country <- countries$id[match(data$country, countries$nid)]
  names(data)[names(data)=='value'] <- "burden"

  vdb <- get_dalys_data_db(con, burden_estimate_set_id, burden_outcomes,
                                   year_min, year_max)

  vdf <- get_dalys_data_stochastic(con, data, burden_estimate_set_id,
                                   burden_outcomes, year_min, year_max)

  vdb <- vdb[order(vdb$country, vdb$year, vdb$age, vdb$burden_outcome),]
  vdf <- vdb[order(vdb$country, vdb$year, vdb$age, vdb$burden_outcome),]
  expect_identical(vdb, vdf)

})

