vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

read_file <- function(filename) {
  paste(readLines(filename), collapse = "\n")
}

read_csv <- function(...) {
  read.csv(..., stringsAsFactors = FALSE)
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

insert_values_into <- function(con, table, d, key = NULL,
                               text_key = FALSE, id = NULL) {
  id <- id %||% (if (length(key) == 1L) key else "id")
  stopifnot(length(id) == 1L)
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    sql <- c(sprintf("INSERT INTO %s", table),
             sprintf("  (%s)", paste(names(x), collapse = ", ")),
             "VALUES",
             sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", ")),
             sprintf("RETURNING %s", id))
    sql <- paste(sql, collapse = "\n")
    if (is.null(key)) {
      DBI::dbGetQuery(con, sql, x)[[id]]
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE", id, table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"), x[key])[[id]]
      if (length(ret) == 0L) {
        ret <- DBI::dbGetQuery(con, sql, x)[[id]]
      }
      ret
    }
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  tmp <- lapply(seq_len(nrow(d)), insert1)
  vapply(tmp, identity, if (text_key) character(1) else integer(1))
}

with_transaction <- function(con, transaction, dry_run, expr) {
  if (transaction || dry_run) {
    DBI::dbBegin(con)
    on.exit(DBI::dbRollback(con))
  }
  res <- force(expr)
  if (dry_run) {
    DBI::dbRollback(con)
  } else if (transaction) {
    DBI::dbCommit(con)
  }
  on.exit()
  res
}

## Deal with data quality issues
is_blank <- function(x) {
  is.na(x) | x == 0
}

rbind_simple <- function(x) {
  nms <- names(x[[1]])
  ok <- vapply(x[-1], function(el) setequal(names(el), nms), logical(1))
  if (!all(ok)) {
    stop("Names do not agree")
  }
  cols <- lapply(nms, function(nm)
    unlist(lapply(x, "[[", nm), use.names = FALSE))
  names(cols) <- nms
  as.data.frame(cols, stringsAsFactors = FALSE)
}
