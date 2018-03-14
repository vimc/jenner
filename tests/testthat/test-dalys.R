context("dalys_calculation")

test_that("duration_weighting", {
  period <- c("year", "day", "month")
  w <- c(1., 1./365, 1./12)
  expect_equal(duration_weighting(period), w)
})

test_that("dalys_calculation", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname=":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })

  # parameters - we are testing against Measles - Jit
  dalys_src <- read.csv("dalys_parameters.csv", stringsAsFactors = FALSE)
  touchstone_name <- "201710gavi"
  year_min <- 2020
  year_max <- 2020
  stopifnot(unique(dalys_src$modelling_group) == "LSHTM-Jit")
  # responsibility 328-MCV1 332-No vac
  # burden_estimate 276-MCV1 275-No vac
  ##mannual calculation
  a <- DBI::dbGetQuery(con, "SELECT ")
  ## run function
  dat <- jenner::calculate_dalys(con, dalys_src, touchstone_name, year_min, year_max)

  saveRDS(a, "jenner-test-data/dalys_calculation/Jit-Measles.rds")
  expect_known_value(a, "jenner-test-data/dalys/Jit-Measles.rds",
                     update = FALSE)
})
