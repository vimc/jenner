database_connection <- function(instance = "science", user = "readonly",
                                port = NULL) {
  if (instance == "science") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 5432
  } else if (instance == "uat") {
    host <- "support.montagu.dide.ic.ac.uk"
    port <- 15432
  } else if (instance == "production") {
    host <- "production.montagu.dide.ic.ac.uk"
    port <- 5432
  } else if (instance == "localhost") {
    host <- "localhost"
    port <- port %||% 5432
  } else {
    stop("Unknown instance ", instance)
  }
  if (instance == "uat") {
    password <- "changeme"
  } else {
    password <- vault_read(sprintf("/secret/database/users/%s", user),
                           "password")
  }
  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "montagu",
                 host = host,
                 port = port,
                 password = password,
                 user = user)
}
