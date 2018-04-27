##' Connect to database
##' @title Connect to database
##'
##' @param location One of "science", "production", "uat" or
##'   "localhost".  Be \emph{very} careful if using production.  If
##'   \code{NULL}, then the location is set by the
##'   \code{MONTAGU_DB_HOST} environment variable.  For running in
##'   import scripts you can arrange to set that to the value of the
##'   shell command \code{hostname} (e.g. \code{export
##'   MONTAGU_DB_HOST=$(hostname)}).
##'
##' @param user Username to connect as
##' @param local_port Port (when running locally)
##' @param local_password_group  Password group (when running locally)
##' @export
database_connection <- function(location = "science", user = "readonly",
                                local_port = NULL,
                                local_password_group = "science") {
  location <- database_location(location)
  if (location == "science") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 5432
    group <- "science"
  } else if (location == "uat") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 15432
    group <- NULL
  } else if (location == "production") {
    host <- "production.montagu.dide.ic.ac.uk"
    port <- 5432
    group <- "production"
  } else if (location == "localhost") {
    host <- "localhost"
    port <- local_port %||% 5432
    group <- local_password_group
  } else {
    stop("Unknown location ", location)
  }
  if (is.null(group)) {
    password <- if (user == "vimc") "changeme" else user
  } else {
    password <- vault_read(sprintf("/secret/database/%s/users/%s", group, user),
                           "password")
  }
  ret <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "montagu",
                        host = host,
                        port = port,
                        password = password,
                        user = user)
  attr(ret, "location") <- location
  ret
}


database_location <- function(location) {
  if (is.null(location)) {
    hostname <- Sys.getenv("MONTAGU_DB_HOST", NA_character_)
    if (is.na(hostname)) {
      stop("'MONTAGU_DB_HOST' is unset", call. = FALSE)
    }
    location <- if (hostname == "fi--didevimc01") "production" else hostname
  }
  location
}


append_table <- function(con, table, data, ...) {
  DBI::dbWriteTable(con, table, data, append = TRUE, row.names = FALSE)
}
