context("dalys_calculation")

test_that("duration_weighting", {
  period <- c("year", "day", "month")
  w <- c(1., 1./365, 1./12)
  expect_equal(duration_weighting(period), w)
})
