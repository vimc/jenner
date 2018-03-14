context("dalys_calculation")

test_that("duration_weighting", {
  period <- c("year", "day", "month")
  w <- c(1., 1./365, 1./12)
  expect_equal(duration_weighting(period), w)
})

test_that("dalys_calculation", {

  dalys_src <- read.csv("dalys_parameters.csv", stringsAsFactors = FALSE)
  ##mannual calculation
  a <- NULL
  ## run function
  dat <- jenner::calculate_dalys(con, dalys_src, touchstone_name, year_min, year_max,  vimc_dalys_only = TRUE)

  saveRDS(a, "jenner-test-data/dalys_calculation/Jit-Measles.rds")
  expect_known_value(a, "jenner-test-data/dalys/Jit-Measles.rds",
                     update = FALSE)
})
