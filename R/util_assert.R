assert_has_columns <- function(x, columns, name = deparse(substitute(x))) {
  msg <- setdiff(columns, names(x))
  if (length(msg) > 0L) {
    stop(sprintf("Missing columns from '%s': %s",
                 name, paste(msg, collapse = ", ")))
  }
}

assert_has_files <- function(x, files) {
  msg <- setdiff(files,x)
  if (length(msg) > 0L) {
    stop(sprintf("Missing file '%s' from: %s.",
                 msg, paste(files, collapse = ", ")))
  }
}

