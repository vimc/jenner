##' Calculate dalys
##'
##' @title DALYs calculation
##'
##' @param con You can be \code{readonly} user to run this function.
##' But if you need to import dalys for Ferrari, Li and LiST (201710gavi), you can use \code{import} user.
##' @param touchstone_name touchstone for which dalys are calcualted
##' @param year_min minimal year
##' @param year_max maximum year
##' @param vimc_dalys_only set to be TRUE if we are only interested in Ferrari, Li and LiST (201710gavi)
##' @param modelling_group This parameter makes the calcualtion more flexible, specify a vector of modelling_group(s) that you are interested in.
##' @param stochastic_data If this is not NULL, then instead of querying the database for the burden
##' estimate set, use the specified stochastic_data, which must be a data frame containing the columns
##' "burden_estimate_set", "country", "year", "age", "burden_outcome" and "burden". The burden_estimate_set
##' should refer to the central burden estimate set for that group, which daly parameters related to.
##' "country" is the 3-character representation. "year" and "age" are trivial; "burden_outcome" is the
##' integer code for the burden, for each line, and "burden" is the data value.
##'
##' @export
calculate_dalys <- function(con, touchstone_name, year_min = 2000, year_max = 2030,
                            vimc_dalys_only = TRUE, modelling_group = NULL,
                            stochastic_data = NULL) {

  ## [make temporary dalys parameter table]
  dalys_parameters <- create_dalys_parameters(con, touchstone_name, vimc_dalys_only)
  if (!is.null(modelling_group)) {
    i <- modelling_group %in% dalys_parameters$modelling_group
    if (any(!i)) {
      stop("unknown modelling group")
    } else {
      dalys_parameters <- dalys_parameters[dalys_parameters$modelling_group %in% modelling_group, ]
    }
  }
  ## [make remaining life expectancy table] - this will take some time
  life_table <- create_dalys_life_table(con, touchstone_name, year_min, year_max)

  sets <- unique(dalys_parameters$burden_estimate_set_id)
  dalys_out <- lapply(sets, function(i) calculate_dalys1(con, life_table, i,
                                                         sql_in(unique(dalys_parameters$burden_outcome_id[dalys_parameters$burden_estimate_set_id == i]), text_item = FALSE),
                                                         year_min, year_max, stochastic_data))
  # output
  dat <- do.call(rbind, dalys_out)
  dat$burden_outcome <- DBI::dbGetQuery(con, "SELECT id FROM burden_outcome WHERE code = 'dalys'")$id
  cols <- c("burden_estimate_set", "country", "year", "burden_outcome", "value", "age")
  dat[cols]
}

create_dalys_parameters <- function(con, touchstone_name = "201710gavi", vimc_dalys_only) {
  dalys_src <- read_csv(system.file("dalys_parameters.csv", package = "jenner", mustWork = TRUE))
  ## subsetting those for which VIMC has to calculate DALYs - vimc_dalys = TRUE
  if (vimc_dalys_only) {
    dalys_src <- dalys_src[dalys_src$vimc_dalys, ]
  }
  ## transform average duration into years; N.B. duration will be compared with remaining life-expectancy
  ## 1000 years of duration is funny, but helpful
  dalys_src$duration <-  dalys_src$average_duration * duration_weighting(dalys_src$period)

  ## adjusted weight = proportion * diability_weight; this saves calculation later
  dalys_src$adjusted_weight <- dalys_src$proportion * dalys_src$disability_weight

  ## find relevant responsibilities
  sql <- read_sql(file_name = "dalys_calculation/responsibility.sql")
  recipe <- DBI::dbGetQuery(con, sql, touchstone_name)

  dalys_src <- merge(dalys_src, recipe, by = c("disease", "modelling_group"), all.x = TRUE)

  dalys_src <- dalys_src[order(dalys_src$burden_estimate_set_id), ]
  cols <- c("disease", "modelling_group", "scenario", "burden_estimate_set_id","burden_outcome_id", "code", "sub_condition", "duration", "adjusted_weight")
  ## make a temporary table - dalys_parameters. This stores model-specific dalys parameters.
  DBI::dbWriteTable(con, "dalys_parameters", dalys_src[cols], temporary = TRUE, overwrite = TRUE, row.names = FALSE)
  dalys_src[cols]
}

create_dalys_life_table <- function(con, touchstone_name = "201710gavi", year_min = 2000, year_max = 2030) {
  message("Creating touchstone specific life table - remaining life expectancy by country, year and age")
  sql <- read_sql(file_name = "dalys_calculation/raw_remaining_life_expectancy.sql")

  life_ex <- DBI::dbGetQuery(con, sql, touchstone_name)
  cols <- c("country", "gender", "year", "age", "value")

  # interpolate years
  l <- split(life_ex, list(life_ex$country, life_ex$age_from, life_ex$gender))
  life_ex2 <- do.call(rbind, lapply(l, function(i) interpolate_year(i, year_min, year_max)))

  # interpolate age
  # keep age 0
  life_ex0 <- life_ex2[life_ex2$age == 0,]
  # after age 0, remaining life expectancy is monotonic
  life_ex3 <- life_ex2[life_ex2$age != 0,]
  l <- split(life_ex3, list(life_ex3$country, life_ex3$year, life_ex3$gender))
  life_ex4 <- do.call(rbind, lapply(l, function(i) interpolate_age(i)))
  # bind age 0 and other ages
  dat <- rbind(life_ex0[cols],life_ex4[cols])
  # output
  dat$.code <- paste(dat$country, dat$year, dat$age, sep = "-")
  dat$remainning_life_exp <- dat$value
  dat <- dat[c(".code", "gender", "remainning_life_exp")]
  dat
}

interpolate_year <- function(dat, year_min = 2000, year_max = 2101) {
  ## expand life table regarding year - from 5year interval to 1year
  years <- year_min:year_max
  f <- stats::splinefun(x = dat$year, y = dat$value, method = "natural") #natural spline
  interp <- data_frame(year = years)
  interp$value <- f(years)
  interp$country <- dat$country[[1]]
  interp$gender <- dat$gender[[1]]
  interp$age <- dat$age_from[[1]]
  interp
}

interpolate_age <- function(dat) {
  ## expand life table regarding age - from 5year interval to 1year
  ages <- 1:100
  age_from2 <- c(2.5, seq(7,97,5), 110)
  f <- stats::splinefun(x = age_from2, y = dat$value, method = "monoH.FC") #monotone Hermite spline - population of any given cohort decreases strictly from birth to all-gone
  interp <- data_frame(age = ages)
  interp$value <- f(ages)
  interp$country <- dat$country[[1]]
  interp$gender <- dat$gender[[1]]
  interp$year <- dat$year[[1]]
  interp
}


get_dalys_data_db <- function(con, burden_estimate_set_id,
                              burden_outcomes, year_min, year_max) {

  message(sprintf("calculating dalys for burden_estimate_set %s",
                  burden_estimate_set_id))

  # burden_estimates

  sql <- paste(
    "SELECT tab1.*, tab2.* FROM",
    "  (  SELECT burden_estimate_set, country.id AS country, ",
    "            year, age, ",
    "            burden_outcome, value as burden",
    "       FROM burden_estimate",
    "       JOIN country ON country.nid = burden_estimate.country",
    "      WHERE burden_estimate_set = $1",
    sprintf("AND burden_outcome IN %s", burden_outcomes),
    "        AND year BETWEEN $2 AND $3",
    "  ) as tab1",
    "LEFT JOIN",
    "  (  SELECT * FROM dalys_parameters",
    "      WHERE burden_estimate_set_id = $1) as tab2",
    "         ON tab1.burden_estimate_set = tab2.burden_estimate_set_id",
    "        AND tab1.burden_outcome = tab2.burden_outcome_id "
  )

  v <- DBI::dbGetQuery(con, sql, list(burden_estimate_set_id, year_min, year_max))
  message("finish reading from db")
  v
}

get_dalys_data_stochastic <- function(con, data, burden_estimate_surrogate,
                                      burden_outcomes, year_min, year_max) {

  # Convert chars "(a,b)" to vector of a, b

  burden_outcomes <- gsub("\\(", "", burden_outcomes)
  burden_outcomes <- gsub("\\)", "", burden_outcomes)
  burden_outcomes <- as.integer(unlist(strsplit(burden_outcomes, ",")))

  # Check the input data frame a bit

  expected_cols <- c("burden_estimate_set", "country", "year",
                      "age", "burden_outcome", "burden")
  for (e in expected_cols) {
    if (!e %in% names(data)) {
      stop(sprintf("Error - missing column '%s' in data", e))
    }
  }

  # Filter input by outcomes and years

  v <- data[data$burden_estimate_set == burden_estimate_surrogate &
            data$burden_outcome %in% burden_outcomes &
            data$year >= year_min &
            data$year <= year_max, ]

  # Fetch dalys parameters for the "central estimate set" which we're
  # told the stochastic data relates to.

  dp <- DBI::dbGetQuery(con, "SELECT * FROM dalys_parameters
                               WHERE burden_estimate_set_id = $1",
                                          burden_estimate_surrogate)

  # Left Join - for each line in dp, column-bind all the burdens with
  # matching outcome to it. Then rbind everything together.

  new_data <- NULL
  for (dpi in seq_len(nrow(dp))) {
    new_data <- rbind(new_data,
      cbind(v[v$burden_outcome == dp$burden_outcome_id[dpi],],
            dp[dpi, ]))
  }

  new_data
}

calculate_dalys1 <- function(con, life_table, burden_estimate_set_id,
                             burden_outcomes, year_min, year_max,
                             stochastic_data = NULL) {
  bes <- burden_estimate_set_id
  result <- NULL
  if (!is.null(stochastic_data)) {
    bes <- unique(stochastic_data$burden_estimate_set)

  }
  if (burden_estimate_set_id %in% bes) {
    message(sprintf("calculating dalys for burden_estimate_set %s",
                    burden_estimate_set_id))

    if (is.null(stochastic_data)) {
      v <- get_dalys_data_db(con, burden_estimate_set_id, burden_outcomes,
                             year_min, year_max)
    } else {

      v <- get_dalys_data_stochastic(con, stochastic_data, burden_estimate_set_id,
                                     burden_outcomes, year_min, year_max)
    }

    # match dalys parameters
    v$.code <- paste(v$country, v$year, v$age, sep = "-")
    v <- merge(v, life_table, by = ".code", all.x = TRUE)
    # make condition-sepcific adjusted duration
    v$adjusted_duration <- v$duration
    j <- v$remainning_life_exp < v$duration
    v$adjusted_duration[j] <- v$remainning_life_exp[j]
    # calculate dalys
    v$value <- v$burden * v$adjusted_weight * v$adjusted_duration
    #aggregate to return output in the same structure as burden_estiamte
    result <- stats::aggregate(value ~ burden_estimate_set + country + year + age, data = v, sum, na.rm = TRUE)
  }
  result
}

duration_weighting <- function(period) {
  i <- period %in% c("year", "month", "day")
  if (any(is.na(i))) stop("Unknown period label. Please correct the 'period' column in 'dalys_burden_outcome.csv'.")
  w <- seq_along(period)
  y <- period == "year"
  m <- period == "month"
  d <- period == "day"
  w[y] <- 1.
  w[m] <- 1./12
  w[d] <- 1./365
  w
}
