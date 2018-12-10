vault_read <- function(key, field) {
  vault <- vaultr::vault_client(login = "github")
  vault$read(key, field)
}
