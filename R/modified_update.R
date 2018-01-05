##' Do a modified update
##' @title Do a modified update
##'
##' @param con Database connection
##'
##' @param touchstone_name_mod Name of the new modified touchstone
##'   that we are creating
##'
##' @param touchstone_use Name of the touchstone that we are basing
##'   this off of
##'
##' @export
modified_update_calculate <- function(con, touchstone_name_mod, touchstone_use) {
  year_min <- 2001
  year_max <- 2030

  sql <- paste("SELECT *",
               "  FROM touchstone",
               " WHERE touchstone_name = $1",
               sep = "\n")
  touchstone_mod <- DBI::dbGetQuery(con, sql, touchstone_name_mod)
    i <- touchstone_mod$id != '201510gavi-42' & touchstone_mod$id != '201710gavi-4'
    touchstone_mod <- touchstone_mod[which.max(touchstone_mod$version[i]), ]
  meta <- mu_prepare(con, touchstone_mod$id)
  meta <- mu_impact_metadata(meta, touchstone_use)

  meta$touchstone_mod <-
    as.list(touchstone_mod[c("id", "touchstone_name", "version")])
  meta$touchstone_use <- touchstone_use

  meta$group$touchstone_src <- meta$group$touchstone
  touchstone_src_native <- sub("-[0-9]{6}.*", "", meta$group$touchstone_name)
  meta$group$touchstone_name_dest <-
    sprintf("%s-%s", touchstone_src_native, touchstone_mod$touchstone_name)
  meta$group$vaccine_delivery <- sprintf(
    "[%s] %s",
    ifelse(meta$group$activity_type == "routine", "Rout", "SIA"),
    meta$group$vaccine)

  ## This is quite slow to run but we can just pull it once:
  fmt <- "SELECT * FROM v_target_pop_%s WHERE YEAR BETWEEN %d AND %d"
  pop <- list(
    total = DBI::dbGetQuery(con, sprintf(fmt, "total", year_min, year_max)),
    routine = DBI::dbGetQuery(con, sprintf(fmt, "routine", year_min, year_max)))

  
  index <- meta$group$index
  data <- lapply(index, function(i) mu_build_data(con, i, meta, pop))
  
  data <- rbind_simple(data)
  data <- mu_fix_sdf7_psu(meta, data)
  ## Perform the update itself the update:
  data$deaths_averted_new <- mu_scale("deaths_averted", data)
  data$cases_averted_new <- mu_scale("cases_averted", data)
  meta$data <- data
  meta
}

##' Modified update summary output
##' @title Modified update summary output
##'
##' @param con Database connection
##'
##' @param res A modified update object (returned from
##'   \code{modified_update_calculate})
##'
##' @param path_meta Path to the metadata directory.  In this
##'   directory the following files must exist:
##'   \code{gavi_country_data.csv}, \code{tr_touchstone.csv},
##'   and \code{years_output.csv}.  There is no
##'   validation done on these files and providing the wrong thing
##'   will break in interesting and unknown ways.
##'
##' @export
modified_update_summary_output <- function(con, res, path_meta) {
  translate_vals <- function(x, tr, to = "gavi", from = "montagu") {
    i <- match(x, tr[[from]])
    j <- !is.na(i)
    x[j] <- tr[[to]][i[j]]
    x
  }

  ## TODO: At the moment the metadata handling is very crude; the
  ## inputs are not properly documented or anything.
  meta_files <- c("gavi_country_data.csv", "tr_touchstone.csv",
                  "years_output.csv")
  stopifnot(all(file.exists(file.path(path_meta, meta_files))))


  ## Start on the meta
  group <- res$group

  ## TODO: we need to filter this to only include the "gavi focal model"
  ## for each disease.  This will do for now:
  focal <- DBI::dbReadTable(con, "gavi_focal_model")
  focal$.code <- paste(focal$touchstone, focal$disease, focal$model, sep = "\n")
  group$is_focal <-
    paste(group$touchstone, group$disease, group$model, sep = "\n") %in%
  focal$.code

  tr_touchstone <- read_csv("meta/tr_touchstone.csv")

  group_out <- data_frame(
    vaccine = group$vaccine,
    ## New columns
    disease = group$disease,
    is_focal = group$is_focal,
    activity_type = group$activity_type,
    support_type = group$support_type,
    modelling_group = group$modelling_group,
    model = group$model,
    touchstone = group$touchstone_name,
    vaccine_delivery = group$vaccine_delivery)

  group_out$touchstone_short <-
    translate_vals(group$touchstone_name, tr_touchstone, "montagu_short")

  ## The metadata that we have on countries differs from what the shiny
  ## app has (e.g WHO region of EMR vs EMRO for AFG), so we'll use this:
  ## cmp <- read.csv("../gavi-shiny-app/data_20170117.csv",
  ##                 stringsAsFactors = FALSE)
  ## cols <- c("ISO", "Country", "Continent", "Region", "Global", "WHO_region",
  ##           "WB_2013", "Gavi_PII_Cofin_Status")
  ## country <- unique(cmp[cols])
  ## write.csv(country, "meta/shiny/gavi_country_data.csv", row.names = FALSE)
  country <- read_csv(file.path(path_meta, "gavi_country_data.csv"))
  country$global <- NULL

  ## Which leaves us with the rest:
  ##
  ## TODO: this should be target_pop_estimated_new which needs to come
  ## from coverage_target_new
  cols <- c(country = "country",
            year = "year",
            coverage = "coverage_old",
            population = "target_pop_given",
            population_estimated = "target_pop_estimated",
            deaths_averted_rate = "deaths_averted_rate",
            rate_type = "deaths_averted_rate_type",
            deaths_averted = "deaths_averted",
            fvps = "fvps",
            cases_averted = "cases_averted",
            cases_averted_rate = "cases_averted_rate")
  cols_update <- c(coverage = "coverage_new",
                   deaths_averted = "deaths_averted_new",
                   cases_averted = "cases_averted_new",
                   fvps = "fvps_new",
                   population = "target_pop_given_new")

  dat <- res$data[cols]
  names(dat) <- names(cols)
  tmp <- country[match(dat$country, country$country),
                 setdiff(names(country), "country")]
  dat <- cbind(dat, tmp)
  rownames(dat) <- NULL

  dat_updated <- dat
  dat_updated[names(cols_update)] <- res$data[cols_update]

  group_out_updated <- group_out
  ## Makeup the suffix from res$touchstone_mod$touchstone_name
  name_suffix <- unlist(strsplit(res$touchstone_mod$touchstone_name, "-"))
  name_suffix <- name_suffix[length(name_suffix)]
  group_out_updated$touchstone <-
    paste0(sub("-[0-9]{6}.*", "", group_out$touchstone),"-",
           name_suffix )
  stopifnot(all(group_out_updated$touchstone %in%
                tr_touchstone$montagu))
  group_out_updated$touchstone_short <-
    translate_vals(group_out_updated$touchstone,
                   tr_touchstone,
                   from = "montagu",
                   to = "montagu_short")

  i <- match(res$data$index, res$group$index)
  d1 <- cbind(group_out[i, ], dat)
  d2 <- cbind(group_out_updated[i, ], dat_updated)
  d3 <- rbind(d1, d2)
  rownames(d3) <- NULL

  ## Filter output to a set of years per touchstone:
  keep <- read_csv("meta/years_output.csv")
  i <- logical(nrow(d3))
  for (j in seq_len(nrow(keep))) {
    i[d3$touchstone == keep$touchstone[j] &
      d3$year >= keep$year_from[j] &
      d3$year <= keep$year_to[j]] <- TRUE
  }
  ## Exclude MCV1
  i[d3$vaccine == "MCV1"] <- FALSE
  ret <- d3[i, ]
  rownames(ret) <- NULL
  ret
}

mu_prepare <- function(con, touchstone_new) {
  ## NOTE: duplicated from orderly - could pull out there and remove
  ## this duplication later.
  temporary_view <- function(name, sql) {
    sprintf("CREATE TEMPORARY VIEW v_%s AS\n%s", name, sql)
  }
  path_sql <- system.file("sql/modified_update", package = "jenner",
                          mustWork = TRUE)
  read_sql <- function(name) {
    txt <- read_file(file.path(path_sql, paste0(name, ".sql")))
    whisker::whisker.render(txt, list(touchstone_new = touchstone_new))
  }

  views <- c("migrate_coverage", "burden_fvps", "impact", "coverage",
             "target_pop_total", "target_pop_routine")
  for (v in views) {
    DBI::dbExecute(con, temporary_view(v, read_sql(v)))
  }

  DBI::dbGetQuery(con, read_sql("query_metadata"))
}

mu_build_data <- function(con, index, meta, pop) {
  stopifnot(length(index) == 1)
  message("building data for ", index)
  year_min <- min(pop$total$year)
  year_max <- max(pop$total$year)
  w <- 4L # 1/2 window for rolling average

  ## Process our arguments a bit more:
  x <- meta$group[meta$group$index == index, ]; print(x)
  xd <- meta$impacts[meta$impacts$index == index, ]
  x_deaths <- xd[xd$impact_outcome == "deaths_averted", ]
  x_cases <- xd[xd$impact_outcome == "cases_averted", ]
  x_fvps <- xd[xd$impact_outcome == "fvps_added", ]

  sql <- paste("SELECT country",
               "  FROM touchstone_country",
               " WHERE touchstone = $1",
               "   AND disease = $2",
               " ORDER BY country",
               sep = "\n")
  countries <- DBI::dbGetQuery(con, sql, list(x$touchstone, x$disease))[[1]]

  if (length(countries) == 0L) {
    stop("Import error")
  }

  window <- 2 * w + 1
  year_max2 <- year_max + w
  years <- year_min:year_max2
  n_years <- length(years)

  dat <- expand.grid(year = years,
                     country = countries,
                     index = index,
                     stringsAsFactors = FALSE)[3:1]
  dat$.code <- paste(dat$country, dat$year, sep = "\r")

  merge_in <- function(dat, d, cols) {
    d$.code <- paste(d$country, d$year, sep = "\r")
    i <- match(dat$.code, d$.code)
    keep <- d[i, cols, drop = FALSE]
    rownames(keep) <- NULL
    nms <- names(cols)
    if (!is.null(nms)) {
      nms[!nzchar(nms)] <- cols[!nzchar(nms)]
      names(keep) <- nms
    }
    cbind(dat, keep)
  }

  ## 1. impact:
  sql <- paste("SELECT * FROM v_impact",
               " WHERE YEAR BETWEEN 2001 AND $2",
               "   AND impact_estimate_set = $1")

  d_deaths_averted <-
    DBI::dbGetQuery(con, sql, list(x_deaths$impact_estimate_set, year_max2))
  dat <- merge_in(dat, d_deaths_averted, c(deaths_averted = "value"))

  if (nrow(x_cases) == 0) {
    dat$cases_averted <- NA_real_
  } else {
    d_cases_averted <-
      DBI::dbGetQuery(con, sql, list(x_cases$impact_estimate_set, year_max2))
    dat <- merge_in(dat, d_cases_averted, c(cases_averted = "value"))
  }

  if (nrow(x_fvps) > 0) {
    d_fvps_added <-
      DBI::dbGetQuery(con, sql, list(x_fvps$impact_estimate_set, year_max2))
    dat <- merge_in(dat, d_fvps_added, c(fvps = "value"))
  } else {
    stop("should be unused now?")
    sql <- paste("SELECT * FROM v_burden_fvps",
                 " WHERE YEAR BETWEEN 2001 AND $2",
                 "   AND burden_estimate_set = $1")
    d_fvps <- DBI::dbGetQuery(con, sql, list(x$burden_estimate_set, year_max2))
    dat <- merge_in(dat, d_fvps, c(fvps = "value"))
  }

  ## 3. old coverage
  sql <- paste("SELECT * FROM v_coverage",
               " WHERE YEAR BETWEEN 2001 and $2",
               "   AND coverage_set = $1")
  d_cov_old <- DBI::dbGetQuery(con, sql, list(x$coverage_set, year_max2))
  dat <-
    merge_in(dat, mu_fix_coverage(d_cov_old),
             c(coverage_old = "coverage", coverage_target = "target"))

  ## 4. new coverage
  if (is.na(x$coverage_set_new)) {
    stop("Import error: no new coverage found")
  } else {
    if(meta$touchstone_mod$touchstone_name == meta$group$touchstone_name[meta$group$index == index])  {
      d_cov_new <- d_cov_old
    } else {
      d_cov_new <- DBI::dbGetQuery(con, sql, list(x$coverage_set_new, year_max2))
    }
    dat <- merge_in(dat, mu_fix_coverage(d_cov_new),
                    c(coverage_new = "coverage",
                      coverage_target_new = "target"))
  }



  ## 5. population estimates
  pop_total <- pop$total[pop$total$demographic_source == x$demographic_source, ]
  pop_routine <- pop$routine[
    pop$routine$age_from == x$vaccine_routine_age &
      pop$routine$demographic_source == x$demographic_source, ]
  dat <- merge_in(dat, pop_total, c(pop_total = "value"))
  dat <- merge_in(dat, pop_routine, c(pop_routine = "value"))

  ## 6. Impact rates

  ## NOTE: There is some fiddling required here.  We might have cases
  ## where there is nonzero impact but zero fvps.  There are two forms
  ## of this!
  ##
  ## 1. In the first there is no coverage.  I'm following what appears
  ## to have been done in the previous GAVI analyses and zeroing the
  ## fvps and impact in this case.
  ##
  ## 2. For some cases (all of which come from Measles PSU campaigns)
  ## the fvps are zero and the impact is positive even though the
  ## coverage is nonzero!  In this case I am taking the target as fvps
  ## but the other option would be to zero the impact.  There are a
  ## few unfortunate cases though where the coverage is nonzero but
  ## the coverage_target is not known; those cases I've opted to just
  ## zero the rate calculation but leave the impact in.
  ## (we have such cases for routines as well.)


  i <- is_blank(dat$fvps) &
    !(is_blank(dat$deaths_averted) & is_blank(dat$cases_averted)) &
    !is_blank(dat$coverage_target)
  if (any(i)) {
    stopifnot(x$activity_type == "campaign")
    message("Creating synthetic fvps for ", x$model)
    ## Otherwise use pop_routine.
    dat$fvps[i] <- dat$coverage_old[i] * dat$coverage_target[i]
  }

  ## There are cases where we have blank fvps_old, but positive coverage_old
  ## This caused the Inf/-Inf impact rate issue for CHN and TCD
  ## We create synthetic fvps for such cases when applicable.
  i <- is_blank(dat$fvps) & !is_blank(dat$coverage_old)
  if (any(i)) {
    message("Creating synthetic fvps for ", x$model)
    if((x$activity_type == "campaign")){
      dat$fvps[i] <- dat$coverage_old[i] * dat$coverage_target[i]
    }else{
      dat$fvps[i] <- dat$coverage_old[i] * dat$pop_routine[i]
    }
  }

  dat <- mu_calculate_rate("deaths", dat, window, n_years)
  dat <- mu_calculate_rate("cases", dat, window, n_years)

  ## And another quantity needed
  ##
  ## NOTE: Here, we can run into trouble where the coverage is zero
  ## but the modellers have reported fvps.  I'm setting this target
  ## pop to NA now (rather than zero so that is not counted in the
  ## rolling calculation)
  dat$target_pop_estimated <- dat$fvps / dat$coverage_old
  dat$target_pop_estimated[dat$fvps > 0 & is_blank(dat$coverage_old)] <- NA

  dat$target_pop_estimated_avg <-
    roll_mean_by(dat$target_pop_estimated, window, n_years)

  if (x$activity_type == "routine") {
    dat$target_pop_given <- dat$pop_routine
    dat$target_pop_given_new <- dat$pop_routine
  } else {
    dat$target_pop_given <- dat$coverage_target
    dat$target_pop_given_new <- dat$coverage_target_new
  }

  ## We also want new fvps which we compute as coverage * target pop
  v <- if (x$activity_type == "campaign")
    "coverage_target_new" else "pop_routine"
  dat$fvps_new <- dat$coverage_new * dat[[v]]

  ## At the moment we can do nothing for MHL, TUV and XK, because we have no routine pop for them
  i <- !is_blank(dat$coverage_new) & is_blank(dat$fvps_new) & dat$year < 2031#& !is_blank(dat$target_pop_estimated)
  if (any(i)) {
    if (x$activity_type == "routine" &&
        all(dat$country[i] %in% c("MHL", "TUV", "XK"))) {
      dat$fvps_new[i] <- dat$coverage_new[i] * dat$target_pop_estimated[i]
    } else if (!all(is_blank(dat$fvps[i])))
      stop("modified update error")
  }
  #For analysis purpose, a touchstone can be 'updated' by itself - assumes equal fvps
  if(meta$touchstone_mod$touchstone_name == meta$group$touchstone_name[meta$group$index == index])
    dat$fvps_new <- dat$fvps
  ## drop excess temporary things
  dat$.code <- NULL
  dat <- dat[dat$year <= year_max, ]

  dat
}

mu_impact_metadata <- function(meta, touchstone_use) {
  meta <- meta[meta$touchstone_name %in% touchstone_use, ]
  rownames(meta) <- NULL

  vary <- c("impact_outcome", "impact_estimate_set", "impact_estimate_recipe",
            "focal_ingredient")
  same <- setdiff(names(meta), vary)

  f <- function(y) {
    n <- vapply(y, function(x) length(unique(x)), integer(1))
    stopifnot(all(n[same] == 1))
    stopifnot(all(n[vary] > 1))
  }
  meta_code <- paste(meta$model, meta$touchstone_name, meta$disease,
                     meta$vaccine, meta$activity_type, meta$support_type, meta$responsibility,
                     sep = "\r")
  meta_code <- factor(meta_code, unique(meta_code))

  i1 <- unname(tapply(seq_along(meta_code), meta_code, min))
  group <- meta[i1, same]
  group$index <- seq_len(nrow(group))
  rownames(group) <- NULL

  impacts <- meta[vary]
  impacts$index <- group$index[tapply(meta_code, meta_code)]
  impacts <- impacts[order(impacts$index, impacts$impact_outcome), ]

  list(group = group, impacts = impacts)
}

##' Calculate updated impact
##' @title Calculate updated impact
##'
##' @param name Impact type: deaths_averted or cases_averted
##'
##' @param d Data: use impact_rate_tot (method 2)
##'
##' @export
mu_scale <- function(name, d) {
  ## This chunck becomes simpler, since only method 2 is used throughout.
  rate_tot_old <- d[[paste0(name, "_rate_tot")]]
  fvps_new <- d$fvps_new

  ret <- fvps_new * rate_tot_old
  ret[!is.finite(ret)] <- NA
  j <- !is.na(d$coverage_new) & d$coverage_new == 0
  ret[j] <- 0
  ret
}

mu_fix_coverage <- function(d) {
  ## This is probably something that we will end up reusing a lot; this
  ## (attempts to) combine campaign coverage for the cases where there
  ## are multiple campaigns per year.  It's not perfect - there are
  ## cases where there are no target populations and that makes it very
  ## impossible to weight the given coverages.
  make_code <- function(..., args = list(...)) {
    stopifnot(length(unique(lengths(args))) == 1)
    x <- lapply(args, as.factor)
    n <- vapply(x, nlevels, integer(1))
    v <- as.integer(x[[1]])
    p <- 1L
    for (i in seq_along(x)[-1]) {
      p <- p * n[[i - 1]]
      v <- v + as.integer(x[[i]]) * p
    }
    v
  }

  code <- make_code(args = d[c("coverage_set", "country", "year")])
  n <- table(code)
  fix <- code %in% as.integer(names(n[n > 1]))

  d_fix <- d[fix, ]
  i <- code[fix]

  tmp <- d_fix[tapply(seq_along(i), i, head, 1), ]
  tmp$target <- unname(tapply(d_fix$target, i, sum, na.rm = TRUE))
  reached <- tapply(d_fix$coverage * d_fix$target, i, sum, na.rm = TRUE)
  tmp$coverage <- unname(reached / tmp$target)

  ret <- rbind(d[!fix, ], tmp)
  ret[order(ret$coverage_set, ret$country, ret$year), ]
}

mu_calculate_rate <- function(name, dat, window, n_years) {
  v_averted <- sprintf("%s_averted", name)
  v_inst <- sprintf("%s_averted_rate_inst", name)
  v_avg <- sprintf("%s_averted_rate_avg", name)
  v_tot <- sprintf("%s_averted_rate_tot", name)
  v_use <- sprintf("%s_averted_rate", name)
  v_type <- sprintf("%s_averted_rate_type", name)

  dat[[v_inst]] <- dat[[v_averted]] / dat$fvps
  dat[[v_inst]][
    is_blank(dat$fvps) & !is_blank(dat[[v_averted]])] <- NA

  dat[[v_avg]] <-
    roll_sum_by(dat[[v_averted]], window, n_years) /
    roll_sum_by(dat$fvps, window, n_years)

  dat[[v_tot]] <-
    roll_sum_by(dat[[v_averted]], Inf, n_years) /
    roll_sum_by(dat$fvps, Inf, n_years)

  ## Some cases don't have any non-NA rates
  type <- rep(NA, nrow(dat))
  use <- rep(NA_real_, nrow(dat))
  ## Most cases we can take the total rate
  i <- !is.na(dat[[v_tot]])
  type[i] <- "tot"
  use[i] <- dat[[v_tot]][i]

  dat[[v_use]] <- use
  dat[[v_type]] <- type

  dat
}

mu_fix_sdf7_psu <- function(meta, data){
  ### This function fixes SDF7 - PSU - Measles - Campaign impacts
  ### Youtrack: https://vimc.myjetbrains.com/youtrack/issue/VIMC-1266
  ### Motiation: the modified update is conducted for each impact estimate set. 
  ### However, for SDF7 PSU Measles campaign, we have two estimate sets - due to previous Measelse and MR_Measels distinction.
  ### Given current modififed_update script, any modified update with SDF7 as the source touchstone is wrong.
  
  ### Firstly, locate the problem sources
  i <- meta$group$touchstone_src == "201210gavi-201303gavi-1" & 
    meta$group$modelling_group == "PSU-Ferrari" &
    meta$group$vaccine == "Measles"
  dat_id <- meta$group$index[i]
  
  ### Only run the following for probelem source 
  if(length(dat_id) == 2){
    ## keep the fine data
    keep <- data[!(data$index %in% dat_id), ]
    ## locate problem source
    dat1 <- data[data$index == dat_id[1], ]
    dat2 <- data[data$index == dat_id[2], ]
    ## combining dat1 and dat2 into one - dat1 and dat2 actually belong to the same Measles impact
    dat <- dat1
    i <- is.na(dat1$deaths_averted) | dat1$deaths_averted == 0.
    
    dat$deaths_averted[i] <- dat2$deaths_averted[i]
    dat$cases_averted[i] <- dat2$cases_averted[i]
    dat$fvps[i] <- dat2$fvps[i]
    dat$coverage_old[i] <- dat2$coverage_old[i]
    dat$coverage_target[i] <- dat2$coverage_target[i]
    
    ## check if we have lost any impact 
    i <- sum(dat$deaths_averted, na.rm = TRUE) - 
      (sum(dat1$deaths_averted, na.rm = TRUE) + sum(dat2$deaths_averted, na.rm = TRUE))
    stopifnot( abs(i) < 1 )
    
    ### re-calculate impact rates
    i <- is_blank(dat$fvps) &
      !(is_blank(dat$deaths_averted) & is_blank(dat$cases_averted)) &
      !is_blank(dat$coverage_target)
    if (any(i)) {
      stopifnot(x$activity_type == "campaign")
      message("Creating synthetic fvps for ", x$model)
      dat$fvps[i] <- dat$coverage_old[i] * dat$coverage_target[i]
    }
    
    i <- is_blank(dat$fvps) & !is_blank(dat$coverage_old)
    if (any(i)) {
      message("Creating synthetic fvps for ", x$model)
      if((x$activity_type == "campaign")){
        dat$fvps[i] <- dat$coverage_old[i] * dat$coverage_target[i]
      }else{
        dat$fvps[i] <- dat$coverage_old[i] * dat$pop_routine[i]
      }
    }
    
    year_min <- min(dat$year)
    year_max <- max(dat$year)
    w <- 4L # 1/2 window for rolling average
    window <- 2 * w + 1
    years <- year_min:year_max
    n_years <- length(years)
    
    dat <- mu_calculate_rate("deaths", dat, window, n_years)
    dat <- mu_calculate_rate("cases", dat, window, n_years)
    
    dat$target_pop_estimated <- dat$fvps / dat$coverage_old
    dat$target_pop_estimated[dat$fvps > 0 & is_blank(dat$coverage_old)] <- NA
    
    dat$target_pop_estimated_avg <-
      roll_mean_by(dat$target_pop_estimated, window, n_years)
    
    
    dat$target_pop_given <- dat$coverage_target
    dat$target_pop_given_new <- dat$coverage_target_new
    
    
    ## We also want new fvps which we compute as coverage * target pop
    v <- "coverage_target_new"
    dat$fvps_new <- dat$coverage_new * dat[[v]]
    
    ## At the moment we can do nothing for MHL, TUV and XK, because we have no routine pop for them
    i <- !is_blank(dat$coverage_new) & is_blank(dat$fvps_new) & dat$year < 2031
    if (any(i)) {
      if (x$activity_type == "routine" &&
          all(dat$country[i] %in% c("MHL", "TUV", "XK"))) {
        dat$fvps_new[i] <- dat$coverage_new[i] * dat$target_pop_estimated[i]
      } else if (!all(is_blank(dat$fvps[i])))
        stop("modified update error")
    }
    
    #For analysis purpose, a touchstone can be 'updated' by itself - assumes equal fvps
    if(meta$touchstone_mod$touchstone_name == meta$group$touchstone_name[meta$group$index == dat_id[1]])
      dat$fvps_new <- dat$fvps
    ## drop excess temporary things
    dat$.code <- NULL
    dat <- dat[dat$year <= year_max, ]
    
    dat <- rbind(dat, keep)
  } else {dat <- data}
 dat
}

