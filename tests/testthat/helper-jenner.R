skip_if_no_montagu_password <- function() {
  pw <- Sys.getenv("MONTAGU_PASSWORD")
  if (nzchar(pw)) {
    return()
  }
  testthat::skip("Environment variable 'MONTAGU_PASSWORD' is not defined")
}
