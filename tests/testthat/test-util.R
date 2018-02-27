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

test_that("dquote", {
  expect_equal(dquote(character(0)), character(0))
  expect_equal(dquote("a"), '"a"')
  expect_equal(dquote(c("a", "b")), c('"a"', '"b"'))
})

test_that("merge_in", {
  dat <- data_frame(.code = 1:3, id=1:3, b = c("a", "b", "c"))
  d <- data_frame(.code = 1:3, B = c("A", "B", "C"))
  cols <- "B"
  dat2 <- data_frame(id = 1:3, b = c("a", "b", "c"), B = c("A", "B", "C"))
  expect_equal(merge_in(dat, d, cols, .code = ".code"), dat2)
})
