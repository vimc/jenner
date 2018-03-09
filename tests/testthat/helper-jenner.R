skip_if_no_montagu_password <- function() {
  pw <- Sys.getenv("MONTAGU_PASSWORD")
  if (nzchar(pw)) {
    return()
  }
  testthat::skip("Environment variable 'MONTAGU_PASSWORD' is not defined")
}


test_montagu_readonly_connection <- function() {
  skip_if_no_montagu_password()

  host <- Sys.getenv("MONTAGU_HOST", "support.montagu.dide.ic.ac.uk")
  port <- as.integer(Sys.getenv("MONTAGU_PORT", 5432))
  user <- "readonly"
  password <- Sys.getenv("MONTAGU_PASSWORD", "changeme")
  con <- tryCatch(
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = "montagu",
                   host = host,
                   port = port,
                   password = password,
                   user = user),
    error = function(e)
      testthat::skip("Connection to montagu was not possible"))

  con
}
