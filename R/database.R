##' Connect to database
##' @title Connect to database
##'
##' @param location One of "science", "production", "uat" or
##'   "localhost".  Be \emph{very} careful if using production
##'
##' @param user Username to connect as
##' @param local_port Port (when running locally)
##' @param local_password_group  Password group (when running locally)
##' @export
database_connection <- function(location = "science", user = "readonly",
                                local_host = NULL, local_port = NULL,
                                local_password_group = "science") {
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
    password <- vault_read(sprintf("/secret/%s/database/users/%s", group, user),
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

append_table <- function(con, table, data, ...) {
  DBI::dbWriteTable(con, table, data, append = TRUE, row.names = FALSE)
}
