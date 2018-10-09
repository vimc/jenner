cache <- new.env(parent = emptyenv())

MONTAGU_VAULT <- "https://support.montagu.dide.ic.ac.uk:8200"

vault_connect <- function() {
  if (is.null(cache$vault)) {
    cache$vault <- vaultr::vault_client("github", addr = MONTAGU_VAULT)
  } else if (is.null(cache$vault$token)) {
    cache$vault$auth("github")
  }
}

vault_read <- function(key, field) {
  vault_connect()
  cache$vault$read(key, field)
}
