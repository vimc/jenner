database_connection <- function(location = "science", user = "readonly",
                                port = NULL) {
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
    port <- port %||% 5432
    group <- "science"
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
