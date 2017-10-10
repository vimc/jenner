cache <- new.env(parent = emptyenv())

vault_connect <- function() {
  if (is.null(cache$vault)) {
    cache$vault <- vaultr::vault_client("github")
  } else if (is.null(cache$vault$token)) {
    cache$vault$auth("github")
  }
}

vault_read <- function(key, field) {
  vault_connect()
  cache$vault$read(key, field)
}
