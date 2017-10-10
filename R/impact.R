import_impact_estimate_recipes <- function(con, path,
                                           transaction = TRUE,
                                           dry_run = FALSE) {
  impact <- read_csv(path)
  cols <- c("touchstone", "modelling_group", "disease", "vaccine",
            "impact_outcome", "activity_type", "support_type", "name",
            "scenario", "script", "comment", "focal_ingredient",
            "focal_coverage_set")
  if (!setequal(names(impact), cols)) {
    ## TODO: information about the columns here
    stop("unexpected columns in impact csv")
  }

  group <- paste0(impact$touchstone, impact$disease, impact$modelling_group,
                  sep = "\r")
  message("Importing impact estimate calculations")
  if (transaction || dry_run) {
    DBI::dbBegin(con)
    on.exit(DBI::dbRollback(con))
  }
  dat <- lapply(split(impact, group), import_impact_estimate_recipes1,
                con = con)
  if (dry_run) {
    DBI::dbRollback(con)
  } else if (transaction) {
    DBI::dbCommit(con)
  }
  on.exit()
  impact$impact_estimate_recipe <- unlist(dat, use.names = FALSE)
  invisible(impact)
}

import_impact_estimate_recipes1 <- function(con, impact) {
  touchstone <- impact$touchstone[[1L]]
  disease <- impact$disease[[1L]]
  modelling_group <- impact$modelling_group[[1L]]
  message(sprintf(" - %s / %s / %s", touchstone, disease, modelling_group))

  ## Then we look up what is needed:
  tmp <- strsplit(impact$scenario, "\\s*,\\s*")
  n <- lengths(tmp)
  tmp <- unlist(tmp)
  i <- rep(seq_along(n), n)
  re <- "^([^:]+):([^:]+):([^:]+)$"
  stopifnot(all(grepl(re, tmp)))
  components <- data_frame(scenario = sub(re, "\\1", tmp),
                           burden_outcome = sub(re, "\\2", tmp),
                           name = sub(re, "\\3", tmp))

  sql <- c("SELECT id FROM responsibility_set",
           "WHERE touchstone = $1 AND modelling_group = $2")
  responsibility_set <- DBI::dbGetQuery(con, paste(sql, collapse = "\n"),
                                        list(touchstone, modelling_group))$id
  stopifnot(length(responsibility_set) == 1L)

  sql <- c("SELECT id FROM impact_estimate_recipe",
           sprintf(" WHERE responsibility_set IN (%s)",
                   paste(responsibility_set, collapse = ", ")))
  prev <- DBI::dbGetQuery(con, paste(sql, collapse = "\n"))
  if (nrow(prev) > 0L) {
    message("   ...already imported")
    return(invisible(prev$id))
  }

  d <- data.frame(version = 1L,
                  name = impact$name,
                  responsibility_set = responsibility_set,
                  disease = impact$disease,
                  vaccine = impact$vaccine,
                  script = impact$script,
                  impact_outcome = impact$impact_outcome,
                  activity_type = impact$activity_type,
                  support_type = impact$support_type,
                  comment = as.character(impact$comment))
  impact_estimate_recipe <-
    insert_values_into(con, "impact_estimate_recipe", d)

  ## Then we need to do a *massive* join to pull all this together:
  sql <- c(
    "SELECT",
    "  responsibility.id AS responsibility_id,",
    "  scenario_description.id AS scenario_description_id",
    "FROM responsibility",
    "  JOIN responsibility_set",
    "    ON responsibility.responsibility_set = responsibility_set.id",
    "  JOIN scenario",
    "    ON responsibility.scenario = scenario.id",
    "  JOIN scenario_description",
    "    ON scenario.scenario_description = scenario_description.id",
    "WHERE",
    "  responsibility_set.touchstone = $1",
    "  AND responsibility_set.modelling_group = $2",
    "  AND scenario_description.disease = $3")
  d <- DBI::dbGetQuery(con, paste(sql, collapse = " "),
                       list(touchstone, modelling_group, disease))

  responsibility_id <- d$responsibility_id[
    match(components$scenario, d$scenario_description_id)]
  if (any(is.na(responsibility_id))) {
    ## This can fail for all sorts of reasons but the nasty one that I
    ## have seen so far is that if the impact estimate references the
    ## wrong touchstone version it will appear that nothing is there!
    stop("Failed to identify responsibilities within this touchstone")
  }
  burden_outcomes <- DBI::dbReadTable(con, "burden_outcome")
  burden_outcome_id <-
    burden_outcomes$id[match(components$burden_outcome, burden_outcomes$code)]

  d <- data.frame(responsibility = responsibility_id,
                  impact_estimate_recipe = impact_estimate_recipe[i],
                  burden_outcome = burden_outcome_id,
                  name = components$name)
  impact_estimate_ingredient_id <-
    insert_values_into(con, "impact_estimate_ingredient", d)

  set_focal_ingredient <- function(id) {
    i <- d$impact_estimate_recipe == id
    j <- impact_estimate_recipe == id
    focal_id <- impact_estimate_ingredient_id[i][
      components$scenario[i] == impact$focal_ingredient[j]]
    sql <- paste("UPDATE impact_estimate_recipe",
                 "   SET focal_ingredient = $2",
                 " WHERE id = $1",
                 sep = "\n")
    DBI::dbExecute(con, sql, list(id, focal_id))
  }
  for (id in impact_estimate_recipe) {
    set_focal_ingredient(id)
  }

  invisible(impact_estimate_recipe)
}
