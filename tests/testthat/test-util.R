context("util")

test_that("sql_in, integer", {
  expect_equal(sql_in(1, FALSE), "(1)")
  expect_equal(sql_in(c(1, 10), FALSE), "(1, 10)")
})

test_that("sql_in, text", {
  expect_equal(sql_in("a"), "('a')")
  expect_equal(sql_in(c("a", "b")), "('a', 'b')")
})

test_that("squote", {
  expect_equal(squote(character(0)), character(0))
  expect_equal(squote("a"), "'a'")
  expect_equal(squote(c("a", "b")), c("'a'", "'b'"))
})
