database_connection <- function(location = "science", user = "readonly",
                                port = NULL) {
  if (location == "science") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 5432
  } else if (location == "uat") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 15432
  } else if (location == "production") {
    host <- "production.montagu.dide.ic.ac.uk"
    port <- 5432
  } else if (location == "localhost") {
    host <- "localhost"
    port <- port %||% 5432
  } else {
    stop("Unknown location ", location)
  }
  if (location == "uat") {
    password <- "changeme"
  } else {
    password <- vault_read(sprintf("/secret/database/users/%s", user),
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
