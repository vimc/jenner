##' Set the active touchstone
##' @title Set the active touchstone
##' @param con Database connection (will require write access to the database)
##' @param touchstone_id Touchstone id to set as "open"
##' @param dry_run Don't commit the transaction - just test if it would work
##' @export
admin_set_active_touchstone <- function(con, touchstone_id, dry_run = TRUE) {
  sql_close <- "UPDATE touchstone SET status = 'finished' WHERE status = 'open'"
  sql_open <- "UPDATE touchstone SET status = 'open' WHERE id = '$1'"
  with_transaction(con, TRUE, dry_run, {
    DBI::dbExecute(con, sql_close)
    DBI::dbExecute(con, sql_open, touchstone_id)
  })
}
