context("database")

test_that("database_location resolves sensibly", {
  expect_equal(database_location("foo"), "foo")
  withr::with_envvar(
    list(MONTAGU_DB_HOST = NA_character_),
    expect_error(database_location(NULL),
                 "'MONTAGU_DB_HOST' is unset"))
  withr::with_envvar(
    list(MONTAGU_DB_HOST = "science"),
    expect_equal(database_location(NULL), "science"))
  withr::with_envvar(
    list(MONTAGU_DB_HOST = "production"),
    expect_equal(database_location(NULL), "production"))
  withr::with_envvar(
    list(MONTAGU_DB_HOST = "fi--didevimc01"),
    expect_equal(database_location(NULL), "production"))
})
