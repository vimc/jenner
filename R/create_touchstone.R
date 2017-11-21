##' Create and import a new touchstone.
##'
##' @title Create a new touchstone
##'
##' @param con Database connection.  You will need to be the
##'   \code{vimc} or \code{import} user (not \code{readonly}) to run
##'   this function.
##'
##' @param dat A \code{data.frame} of coverage data to import.
##'
##' @param demography_from Touchstone id to import
##'   demographic statistics from.  This must currently be given, but
##'   in future we'll allow this to be imported from a csv
##'
##' @param path_meta Path that we look for various metadata files.
##'   Eventually we'll document what they look like.
##'
##' @param transaction
##' @param dry_run
##' @export
create_touchstone <- function(con, dat, demography_from = NULL,
                              path_meta = "meta",
                              transaction = TRUE, dry_run = TRUE) {
  ## First, try to create the new touchstone - this will fail if it
  ## already exists, aborting the transaction that this function will
  ## run within.
  touchstone <- read_touchstone_metadata(path_meta)
  if (is.null(demography_from)) {
    stop("demography_from must currently be provided")
  }
  if (transaction) {
    DBI::dbBegin(con)
    on.exit(DBI::dbRollback(con))
  }
  insert_values_into(con, "touchstone_name", touchstone$touchstone_name,
                     key = "id", text_key = TRUE)
  append_table(con, "touchstone", touchstone$touchstone)

  touchstone_id <- touchstone$touchstone$id

  meta_cols <- c("vaccine", "activity_type", "gavi_support_level")

  activity <- touchstone$activities
  dat_meta <- merge(activity, unique(dat[meta_cols]), by = meta_cols,
                    all = TRUE)
  dat_meta$name <- paste0(
    dat_meta$disease, ": ",
    paste(dat_meta$vaccine, dat_meta$gavi_support_level,
          dat_meta$activity_type, sep = ", "))

  cov_set <- unique(dat_meta[c("name", meta_cols)])
  cov_set$touchstone <- touchstone_id

  ## This must be pushed into the database before the next bits will work
  append_table(con, "coverage_set", cov_set)

  ## import scenario - need to match focal_coverage_set - by largest
  ## order (import scenario_description beforehand when applicable)
  meta_cols <- c("touchstone", "scenario_description", "focal_coverage_set")
  sql <- paste0("SELECT touchstone, id AS coverage_set, name from coverage_set WHERE touchstone = $1")
  db_cov_set <- DBI::dbGetQuery(con, sql, touchstone_id)
  scenario <- merge(dat_meta, db_cov_set, by = "name", all = TRUE)
  scenario <- scenario[scenario$is_focal, ]
  scenario$focal_coverage_set <- scenario$coverage_set
  scenario <- unique(scenario[meta_cols])

  append_table(con, "scenario", scenario)

  ## import scenario_coverage_set - the order thing
  meta <- c("scenario", "coverage_set", "order")
  sql <- "SELECT id AS scenario, scenario_description FROM scenario WHERE touchstone = $1"
  db_scenario <- DBI::dbGetQuery(con, sql, touchstone_id)
  bridge <- unique(dat_meta[c("scenario_description", "name", "order")])
  sce_cov_set <- merge(bridge, db_scenario, by = "scenario_description")
  sce_cov_set <- merge(sce_cov_set, db_cov_set, by = "name")
  sce_cov_set <- sce_cov_set[meta]
  append_table(con, "scenario_coverage_set", sce_cov_set)

  ## import responsibility_set
  meta <- c("modelling_group", "touhstone", "status")
  resp_set <- data_frame(modelling_group = unique(dat_meta$modelling_group),
                         touchstone = touchstone_id,
                         status = "approved")
  append_table(con, "responsibility_set", resp_set)

  ## import responsibility - leave current_burden_estimate_set empty
  meta <- c("responsibility_set", "scenario", "current_burden_estimate_set")
  sql <- "SELECT id AS responsibility_set, modelling_group FROM responsibility_set WHERE touchstone = $1"
  db_resp_set <- DBI::dbGetQuery(con, sql, touchstone_id)
  bridge <- merge(db_resp_set, unique(dat_meta[c("modelling_group", "scenario_description")]), by = "modelling_group")
  resp <- merge(bridge, db_scenario, by = "scenario_description")
  resp$current_burden_estimate_set <- NA
  resp <- resp[, meta]
  append_table(con, "responsibility", resp)

  ## import the coverage
  meta <- c("coverage_set", "year", "country", "age_from", "age_to",
            "age_range_verbatim", "coverage", "target", "gavi_support",
            "gender")
  meta_2 <- c("vaccine", "activity_type", "gavi_support_level")
  bridge <- unique(dat_meta[, c("name", meta_2)])
  cov <- merge(bridge, db_cov_set, by = "name")
  ## you see more rows, take it easy, it's caused by duplication of
  ## DTP3 and MCV1 for LiST model
  cov <- merge(dat, cov, by = meta_2, all.x = TRUE)
  cov <- cov[, meta]
  append_table(con, "coverage", cov)

  ## point demographic import to new touchstone - code from Wes
  tdd <- DBI::dbGetQuery(con, paste(
                                "SELECT * from touchstone_demographic_dataset",
                                " WHERE touchstone=$1"), demography_from)
  tdd$touchstone <- touchstone_id
  tdd$id <- NULL
  append_table(con, "touchstone_demographic_dataset", tdd)

  tc <- DBI::dbGetQuery(con, paste(
                               "SELECT * from touchstone_country",
                               " WHERE touchstone=$1"), demography_from)
  tc$touchstone <- touchstone_id
  tc$id <- NULL
  append_table(con, "touchstone_country", tc)

  if (transaction && !dry_run) {
    DBI::dbCommit(con)
    on.exit()
  }
  message("import complete")
}

read_touchstone_metadata <- function(path_meta) {
  read_csv2 <- function(filename, columns = NULL, rows = NULL, ...) {
    if (!file.exists(filename)) {
      stop(sprintf("Expected filename '%s' to exist, but is missing", filename))
    }
    d <- read_csv(filename, ...)
    if (!is.null(columns)) {
      if (!setequal(columns, names(d))) {
        stop(sprintf("Incorrect columns in %s:\n\tExpected: %s\n\tRecieved: %s",
                     filename,
                     paste(expected, collapse = "\n"),
                     paste(recieved, collapse = "\n")))
        }
    }
    if (!is.null(rows)) {
      if (nrow(d) != rows) {
        stop(sprintf("Expected %d rows in %s but recieved %d",
                     rows, filename, nrow(d)))
      }
    }
    d
  }

  touchstone_name <- read_csv2(file.path(path_meta, "touchstone_name.csv"),
                               c("id", "description", "comment"), 1L)
  touchstone <- read_csv2(file.path(path_meta, "touchstone.csv"),
                          c("version", "description", "status", "comment"), 1L)
  activities_cols <- c("modelling_group", "scenario_description", "disease",
                       "vaccine", "gavi_support_level", "activity_type",
                       "order", "is_focal")
  activities <- read_csv2(file.path(path_meta, "activities.csv"),
                          activities_cols)

  touchstone$touchstone_name <- touchstone_name$id
  touchstone$id <- sprintf("%s-%s", touchstone_name$id, touchstone$version)

  list(touchstone_name = touchstone_name,
       touchstone = touchstone,
       activities = activities)
}

##' Project coverage
##' @title Project coverage
##' @param dat Data with columns...
##'
##' @param year_project_from This is the year that the projections start
##'
##' @param year_from,year_to Range of the actual coverage data that
##'   that you want.
##'
##' @export
project_coverage <- function(dat, year_project_from,
                             year_from = 1980, year_to = 2100) {
  assert_has_columns(dat, c("country", "vaccine", "activity_type", "gavi73"))

  meta <- unique(dat[c("country", "vaccine", "activity_type", "gavi73")])

  data_out <- rep(list(NA), nrow(meta))

  for (i in seq_len(nrow(meta))){
    j <- dat$country == meta$country[i] &
      dat$vaccine == meta$vaccine[i] &
      dat$activity_type == meta$activity_type[i] &
      dat$gavi73 == meta$gavi73[i]
    data_in <- dat[j, ]

    x <- project_coverage1(data_in, year_from, year_project_from, year_to, meta[i, ])

    data_out[[i]] <- project_coverage1(data_in, year_from, year_project_from,
                                       year_to, meta[i, ])
  }
  dat <- do.call("rbind", data_out)
  dat
}

project_coverage1 <- function(dat, year_from, year_proj_from, year_to, meta) {
  stopifnot(nrow(meta) == 1L,
            year_from <= year_proj_from,
            year_proj_from <= year_to)
  meta <- list(meta)

  proj_cov <- function(datai, year_proj){
    i <- datai$year %in% year_proj
    y <- datai$year == min(year_proj) - 1
    if(datai$coverage[y] > 0. ){
      datai$coverage[i] <- datai$coverage[y] + 0.01 * seq_along(year_proj)
    } else{
      datai$coverage[i] <- 0.
    }
    datai
  }
  cap_cov <- function(datai, rule, rule_3){
    tmp <- choose_less(rule, rule_3)
    i <- datai$coverage > tmp
    datai$coverage[i] <- tmp
    datai
  }
  choose_less <- function(x, y){
    if(x >= y) x else y
  }

  rule_1 <- 0.9
  rule_2 <- 0.95
  rule <- if (meta[[1]]$vaccine == "MCV1") rule_2 else rule_1

  year <- year_from:year_to
  tmp <- rep(meta, length(year))
  tmp <- do.call("rbind", tmp)
  tmp$year <- year
  tmp <- merge(tmp, dat, by = names(tmp), all = TRUE) # campaign data ends here

  if (meta[[1]]$activity_type == "routine") {
    year <- year_proj_from:year_to
    tmp <- proj_cov(tmp, year)
    rule_3 <- tmp$coverage[tmp$year == year_proj_from - 1]
    tmp <- cap_cov(tmp, rule, rule_3)
  }

  tmp
}
