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
  dat <- with_transaction(con, transaction, dry_run, {
    lapply(split(impact, group), import_impact_estimate_recipes1,
           con = con)
  })
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

impact_estimate_data <- function(con, impact_estimate_recipe_id) {
  sql <- c(
    "SELECT",
    "  impact_estimate_ingredient.id",
    "    AS impact_estimate_ingredient,",
    "  impact_estimate_ingredient.impact_estimate_recipe",
    "    AS impact_estimate_recipe,",
    "  burden_estimate_set.id",
    "    AS burden_estimate_set,",
    ## We need to know these for the translation
    "  impact_estimate_ingredient.name,",
    "  impact_estimate_ingredient.burden_outcome,",
    ## This needs expanding as version information
    "  burden_estimate_set.uploaded_on",
    ## Then a nasty join to get the *current set of responsibilities*
    "FROM impact_estimate_ingredient",
    "  JOIN responsibility",
    "    ON impact_estimate_ingredient.responsibility = responsibility.id",
    "  JOIN burden_estimate_set",
    "    ON responsibility.current_burden_estimate_set = ",
    "       burden_estimate_set.id",
    "WHERE impact_estimate_ingredient.impact_estimate_recipe = $1")
  cols <- DBI::dbGetQuery(con, paste(sql, collapse = " "),
                          impact_estimate_recipe_id)

  ## Similar query to work out what the focal bits are:
  sql <- paste("SELECT impact_estimate_recipe.id AS impact_estimate_recipe,",
               "       responsibility.current_burden_estimate_set AS",
               "         focal_burden_estimate_set,",
               "       scenario.focal_coverage_set",
               "  FROM impact_estimate_recipe",
               "  JOIN impact_estimate_ingredient",
               "    ON impact_estimate_ingredient.id =",
               "       impact_estimate_recipe.focal_ingredient",
               "  JOIN responsibility",
               "    ON responsibility.id =",
               "       impact_estimate_ingredient.responsibility",
               "  JOIN scenario",
               "    ON scenario.id = responsibility.scenario",
               " WHERE impact_estimate_recipe = $1",
               sep = "\n")
  info <- DBI::dbGetQuery(con, sql, impact_estimate_recipe_id)
  stopifnot(nrow(info) == 1L)

  ## Data for the calculation:
  rename <- paste(sprintf("value%d AS %s", seq_along(cols$name), cols$name),
                  collapse = ", ")
  n <- nrow(cols)
  args <- paste0("$", seq_len(n + 1), collapse = ", ")
  sql <- sprintf(
    "SELECT country, year, age, %s from select_burden_data%d(%s)",
    rename, n, args)
  pars <- as.list(c(cols$burden_outcome[[1L]], cols$burden_estimate_set))
  data <- DBI::dbGetQuery(con, sql, pars)

  list(cols = cols, info = info, data = data)
}

impact_estimate_compute1 <- function(con, impact_estimate_recipe_id) {
  sql <- c("SELECT",
           "    impact_estimate_recipe.*,",
           "    responsibility_set.touchstone,",
           "    responsibility_set.modelling_group",
           "  FROM impact_estimate_recipe",
           "  JOIN responsibility_set",
           "    ON responsibility_set.id =",
           "         impact_estimate_recipe.responsibility_set",
           " WHERE impact_estimate_recipe.id = $1")
  info <- DBI::dbGetQuery(con, paste(sql, collapse = "\n"),
                          impact_estimate_recipe_id)
  message(sprintf(" - %s / %s / %s / %s",
                  info$touchstone, info$modelling_group, info$disease,
                  info$name))

  res <- impact_estimate_data(con, impact_estimate_recipe_id)
  browser()
  dat <- res$data
  cols <- res$cols
  value <- eval(parse(text = info$script), dat, .GlobalEnv)

  if (length(value) != nrow(dat)) {
    stop(sprintf("Expected %d values but recieved %d", nrow(res), length(value)))
  }
  res$value <- value

  ## impact_estimate_set
  d <- data.frame(
    impact_estimate_recipe = impact_estimate_recipe_id,
    recipe_touchstone = info$touchstone,
    ## TODO: this needs to be configurable to run the
    ## modified update
    coverage_touchstone = info$touchstone,
    focal_coverage_set = res$info$focal_coverage_set,
    focal_burden_estimate_set = res$info$focal_burden_estimate_set)
  impact_estimate_set <- insert_values_into(con, "impact_estimate_set", d)

  ## impact_estimate_set_ingredient
  d <- data.frame(impact_estimate_set = impact_estimate_set,
                  impact_estimate_ingredient = cols$impact_estimate_ingredient,
                  burden_estimate_set = cols$burden_estimate_set)
  DBI::dbWriteTable(con, "impact_estimate_set_ingredient", d, append = TRUE)

  ## impact_estimate
  d <- data.frame(impact_estimate_set = impact_estimate_set,
                  year = dat$year,
                  age = dat$age,
                  country = dat$country,
                  value = res$value)
  DBI::dbWriteTable(con, "impact_estimate", d, append = TRUE)

  sql <- paste("UPDATE impact_estimate_recipe",
               "   SET current_impact_estimate_set = $2",
               " WHERE id = $1",
               paste = "\n")
  DBI::dbExecute(con, sql, list(impact_estimate_recipe_id, impact_estimate_set))

  invisible(res)
}

impact_estimate_compute <- function(con, impact_estimate_recipe_ids,
                                    transaction = TRUE, dry_run = FALSE) {
  message(sprintf("Computing %d impact estimate sets",
                  length(impact_estimate_recipe_ids)))
  with_transaction(con, transaction, dry_run, {
    for (id in impact_estimate_recipe_ids) {
      impact_estimate_compute1(con, id)
    }
  })
}
