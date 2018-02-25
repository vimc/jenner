##' Calcualte impact with method2 that allocate impact by fvps*impact_rate
##'
##' @title Impact Calculation (method 2)
##'
##' @param con Database connection.  You will need to be \code{readonly} user
##' to run this function.
##' @param touchstone_name is the touchstone relevant to which impact is calculated
##' @param cohort_min minimal birth year of interest
##' @param cohort_max maximal birth year year of interest
##' @param method impact calculation method - chose from method1 and method2
##' impact outcome can be provided as age specific if simplified=FALSE
##' @export
impact_calculation <- function(con, touchstone_name = "201710gavi", 
                               cohort_min = 2000, cohort_max = 2030, 
                               routine_tot_rate_shape = "trace_cohort", method = "method2") {
  ## prepare metadata
  meta <- prepare_recipe(con, recipe = "impact.csv") 
  
  ## impact calculation
  meta2 <- split(meta, meta$index)
  if(method == "method1") {
    ## method 1 
    impact <- lapply(meta2, function(i) 
      make_impact_method1(con, i, cohort_min, cohort_max))
  } else {
    ## method 2
    impact <- lapply(meta2, function(i) 
      make_impact(con, i, cohort_min, cohort_max, routine_tot_rate_shape))
  }
  
  ## return output
  ## meta output: group -> recipes; scripts -> calculation scripts
  cols_group <- c("index", "touchstone", "modelling_group", "disease", "vaccine",
                  "impact_outcome", "activity_type", "name", "focal_ingredient",
                  "baseline", "burden_outcome")
  
  cols_script <- c("index", "responsibility_set_id",
                   "responsibility_id", "burden_estimate_set_id", #"stochastic_burden_estimate_set_id",
                   "coeff", "burden_outcome_id")
  groups <- unique(meta[cols_group])
  scripts <- meta[cols_script]
  
  ## bind impact estimates
  message("binding data - be patient")
  v <- impact_output(impact, method)
  
  return(list(groups = groups, scripts = scripts, impact_full = v$impact_full, impact_simplified = v$impact_simplified))
}

prepare_recipe <- function(con, recipe) {
  sql_group <- read_sql(file_name = "group")
  sql_burden_outcomes <- read_sql(file_name = "burden_outcomes")
  group <- DBI::dbGetQuery(con, sql_group)
  burden_outcomes <- DBI::dbGetQuery(con, sql_burden_outcomes)
  
  ## read impact recipes - this will change once the recipe is imported into Montagu
  recipe <- read_csv(recipe)
  recipe <- recipe[recipe$central_estimates_complete,]
  #recipe <- recipe[recipe$index == 47, ]
  recipe2 <- split(recipe, recipe$index)
  meta <- lapply(recipe2, function(i) line_up(i, group, burden_outcomes))
  meta <- do.call(rbind, meta)
  row.names(meta) <- NULL
  meta
}

make_impact <- function(con, index, cohort_min, cohort_max, routine_tot_rate_shape = "trace_cohort") {
  # This is method 2 impact calcualtion
  message(sprintf("building impact for index %s", unique(index$index)))
  ## 1. parameters to condition sql
  # locate base and focal scenarios, burden sets
  i <- match("burden_outcome_id", names(index))
  v <- index[-i]
  base <- unique(v[v$coeff == 1, ])
  focal <- unique(v[v$coeff == -1, ])
  # vacciene and activity type for matching with population, coverage, fvps
  vaccine <- unique(index$vaccine)
  activity_type <- unique(index$activity_type)
  # burden outcomes used for impact calculation
  outcomes <- sql_in(unique(index$burden_outcome_id))
  # to calculate impact_rate, we have to have total impact and total fvps
  # which are shape specific for routine and campaign
  if(activity_type == "campaign"){
    shape <- paste( sprintf("AND year BETWEEN %s", 2000),
                    sprintf(" AND %s", 2100), sep="\n")
  }else{
  if(routine_tot_rate_shape == "trace_cohort"){
    shape <- paste( sprintf("AND (year-age) BETWEEN %s", cohort_min),
                    sprintf(" AND %s", cohort_max), sep="\n")
  } else {
    shape <- paste( sprintf("AND (year-age) >= %s", cohort_min),
                    sprintf(" AND year <= %s", 2100), sep="\n")
  }
  }
  
  ## 2.1 sql - total impact by country
  sql_1 <- paste("SELECT tmp.country, sum(tmp.value) AS tot_impact",
                 "FROM (SELECT country, year, age, value",
                 "FROM burden_estimate",
                 sprintf("WHERE burden_estimate_set = %s",base$burden_estimate_set_id),
                 shape,
                 sprintf("AND burden_outcome IN %s ", outcomes),
                 "UNION ALL",
                 "SELECT country, year, age, value*(-1) AS value",
                 "FROM burden_estimate",
                 sprintf("WHERE burden_estimate_set = %s",focal$burden_estimate_set_id),
                 shape,
                 sprintf("AND burden_outcome IN %s ) AS tmp", outcomes),
                 "RIGHT JOIN",
                 "(SELECT DISTINCT country",
                 "FROM burden_estimate",
                 sprintf("WHERE burden_estimate_set = %s",focal$burden_estimate_set_id),
                 ") as focal_country",
                 "ON tmp.country = focal_country.country",
                 "GROUP BY tmp.country", sep="\n")
  # total impact
  tot_impact <- DBI::dbGetQuery(con, sql_1)
  tot_impact$.code <- tot_impact$country
  ## 2.2 sql - coverage, pop, fvps
  ## Rubella routine and HepB_BD are two special cases
  ## Rubella = RCV1 + RCV2
  ## HepB_BD = HepB_BD + HepB_BD_home
  if(vaccine == "Rubella" & activity_type == "routine") {
    vaccine_sql <-  "WHERE vaccine IN ('Rubella', 'RCV2')"
  } else if (vaccine == "HepB_BD_both") {
    vaccine_sql <-  "WHERE vaccine IN ('HepB_BD', 'HepB_BD_home')"
  } else {
    vaccine_sql <- sprintf("WHERE vaccine = '%s'", vaccine)
  }

  if(activity_type == "campaign"){
    shape <- paste( sprintf("AND year BETWEEN %s", 2000),
                    sprintf(" AND %s", 2030), sep="\n")
  }else{
    if(routine_tot_rate_shape == "trace_cohort"){
      shape <- paste( sprintf("AND (year-age) BETWEEN %s", cohort_min),
                      sprintf(" AND %s", cohort_max), sep="\n")
    } else {
      shape <- paste( sprintf("AND (year-age) >= %s", cohort_min),
                      sprintf(" AND year <= %s", 2100), sep="\n")
    }
  }
  sql_2 <- paste("SELECT * FROM temporary_coverage_fvps",
                 vaccine_sql,
                 sprintf("AND activity_type = '%s'", activity_type),
                 sprintf("AND country IN %s", sql_in_text(unique(tot_impact$country))),
                 shape, sep="\n")
  
  ## 3. rate calculation
  
  # total fvps
  dat <- DBI::dbGetQuery(con, sql_2)
  dat$.code <- dat$country
  tot_fvps <- aggregate(fvps ~ .code, data = dat, sum, na.rm=TRUE)
  # merge in total impact and total fvps
  dat <- merge_in(dat, tot_impact, c(tot_impact = "tot_impact"))
  dat$.code <- dat$country
  dat <- merge_in(dat, tot_fvps, c(tot_fvps = "fvps"))
  # impact rate
  dat$tot_rate <- dat$tot_impact / dat$tot_fvps
  # avoid inf rate
  i <- dat$tot_fvps == 0.
  dat$tot_rate[i] <- 0.
  
  ## 4. impact calculation
  dat$impact <- dat$fvps * dat$tot_rate
  
  ## 5. distinguish between total and gavi impact by per-year gavi_support
  # total impact
  total_impact <- dat
  total_impact$support_type <- "total"
  
  # gavi impact, if any
  i <- dat$gavi_support
  if(any(i)) {
    gavi_impact <- dat
    gavi_impact$coverage[!i] <- 0
    gavi_impact$fvps[!i] <- 0
    gavi_impact$impact[!i] <- 0
    gavi_impact$support_type <- "gavi"
    dat <- rbind(total_impact, gavi_impact)
  } else {
    dat <- total_impact
  }
  dat$cohort <- dat$year - dat$age
  dat$index <- focal$index
  
  ## 6. impact output
  ## 1) full impact estimates - very large output - good for investigation purpose
  cols_impact1 <- c("index", "support_type", "country", "vaccine","year", "age", "cohort","gavi_support", "coverage", "population",
                    "fvps", "tot_rate", "impact")
  impact1 <- dat[cols_impact1]
  
  ## 2) aggrefated impact
  cols_impact2 <- c("index", "impact_type","support_type", "country", "vaccine", "year", "tot_rate", "impact")
  # impact by birth cohort
  impact_cohort <- aggregate(impact ~ index + support_type + country + vaccine + cohort + tot_rate, data=dat, sum, na.rm=TRUE)
  impact_cohort$impact_type <- "cohort"
  names(impact_cohort)[which(names(impact_cohort) == "cohort")] <- "year"
  
  # # impact by calendar year
  # impact_calendar <- aggregate(impact ~ index + support_type + country + vaccine + year + tot_rate, data=dat, sum, na.rm=TRUE)
  # impact_calendar$impact_type <- "calendar"
  # impact2 <- rbind(impact_cohort, impact_calendar)
  impact2 <- impact_cohort[cols_impact2]
  impact2 <- impact2[impact2$year %in% (cohort_min:cohort_max), ]
  
  ## 3) End
  message("DONE.")
  return( list(impact_full = impact1, impact_simplified = impact2) )
}

make_impact_method1 <- function(con, index, cohort_min, cohort_max) {
  # This function provides method1 imapct, it is direct calculation from scenarios without re-allocating with respect to fvps_added
  # And it will be total impact only, as we are not running seperately no-gavi scenarios
  # It is provided for reporting purpose
  message(sprintf("building impact for index %s", unique(index$index)))
  ## 1. parameters to condition sql
  # locate base and focal scenarios, burden sets
  i <- match("burden_outcome_id", names(index))
  v <- index[-i]
  base <- unique(v[v$coeff == 1, ])
  focal <- unique(v[v$coeff == -1, ])
  # vacciene and activity type for matching with population, coverage, fvps
  vaccine <- unique(index$vaccine)
  activity_type <- unique(index$activity_type)
  # burden outcomes used for impact calculation
  outcomes <- sql_in(unique(index$burden_outcome_id))
  
  ## 2.1 sql - impact by country-year-age
  sql <- paste("SELECT tmp.country, tmp.year, tmp.age, sum(tmp.value) AS impact",
               "FROM (SELECT country, year, age, value",
               "FROM burden_estimate",
               sprintf("WHERE burden_estimate_set = %s",base$burden_estimate_set_id),
               sprintf("AND (year-age) BETWEEN %s", cohort_min),
               sprintf(" AND %s", cohort_max),
               sprintf("AND burden_outcome IN %s ", outcomes),
               "UNION ALL",
               "SELECT country, year, age, value*(-1) AS value",
               "FROM burden_estimate",
               sprintf("WHERE burden_estimate_set = %s",focal$burden_estimate_set_id),
               sprintf("AND (year-age) BETWEEN %s", cohort_min),
               sprintf(" AND %s", cohort_max),
               sprintf("AND burden_outcome IN %s ) AS tmp", outcomes),
               "RIGHT JOIN",
               "(SELECT DISTINCT country",
               "FROM burden_estimate",
               sprintf("WHERE burden_estimate_set = %s",focal$burden_estimate_set_id),
               ") as focal_country",
               "ON tmp.country = focal_country.country",
               "GROUP BY tmp.country, tmp.year, tmp.age", sep="\n")
  dat <- DBI::dbGetQuery(con, sql)
  
  ## 6. impact output: 1) full impact - impact by year and age/cohort 2) simplified impact - impact by year / cohort
  dat$cohort <- dat$year - dat$age
  dat$index <- focal$index
  
  # full impact - good for investigation purpose
  cols_impact1 <- c("index", "country","year", "age", "cohort", "impact")
  impact1 <- dat[cols_impact1]
  
  # distinguish between 'impact by year' and 'impact by birth cohort'
  cols_impact2 <- c("index", "impact_type","country", "year", "impact")
  # impact by birth cohort
  impact_cohort <- aggregate(impact ~ index + country + cohort, data=dat, sum, na.rm=TRUE)
  impact_cohort$impact_type <- "cohort"
  names(impact_cohort)[which(names(impact_cohort) == "cohort")] <- "year"
  # impact by calendar year
  # impact_calendar <- aggregate(impact ~ index + country + year, data=dat, sum, na.rm=TRUE)
  # impact_calendar$impact_type <- "calendar"
  # impact2 <- rbind(impact_cohort, impact_calendar)
  impact2 <- impact_cohort
  impact2 <- impact2[cols_impact2]
  message("DONE.")
  return( list(impact_full = impact1, impact_simplified = impact2) )
}

##' Provide age-specific coverage-un_pop-fvps
##' @title Impact Calculation (method 2)
##'
##' @param con Database connection.  You will need to be \code{readonly} user
##' to run this function.
##' @param touchstone_name 
##' @export
fix_coverage_fvps <- function(con, touchstone_name = "201710gavi") {
  ### This function convert input data - coverage and UNWPP
  ### i.e. input data by country, year and age
  message("Preparing temporary table <temporary_coverage_fvps>")
  message("In progress ...")
  
  ## 1. touchstone specification
  # given touchstone name, use the latest version touchstone
  version <- DBI::dbGetQuery(con, "SELECT MAX(touchstone.version) as version FROM touchstone
                             WHERE touchstone_name = $1", touchstone_name)
  touchstone <- DBI::dbGetQuery(con, "SELECT id FROM touchstone
                                WHERE touchstone_name = $1
                                AND version = $2", list(touchstone_name, version$version))
  
  ## 2. creat a number table. It will be used to duplicate input data by [age_from, age_to]
  i <- data.frame(i = 1:101, stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "num", i, temporary = TRUE, overwrite=TRUE)
  
  ## 3. select minimal needed input data from db and make it age stratified - gender specific (modup is not considering gender)
  sql <- read_sql(file_name = "coverage_pop")
  tab <- DBI::dbGetQuery(con, sql, list(touchstone$id, 2000, 2100))
  
  ## 4. construct population for each vaccination activity using UN pop -
  # this is needed to convert campaign coverage from target_pop level to UN pop level
  # we need this to allocate campaign fvps to each targeted age
  tab$.code <- paste(tab$vaccine, tab$activity_type, tab$country,
                     tab$year, tab$gavi_support, tab$gender, tab$coverage,sep="><")
  target_cohortS <- aggregate(population ~ .code, data = tab, sum, na.rm = TRUE)
  tab <- merge_in(tab, target_cohortS, c(target_cohortS = "population"))
  
  ## 5. calculate age level fvps
  tab$fvps <- NA
  i <- is.na(tab$target)
  # campaign
  tab$fvps[!i] <- tab$coverage[!i] * tab$target[!i] / tab$target_cohortS[!i] * tab$population[!i]
  # routine
  tab$fvps[i] <- tab$coverage[i] * tab$population[i]
  # report problems - fvps >> pop
  tab$diff <- (tab$target - tab$target_cohortS) / tab$target_cohortS
  write.csv(tab[tab$diff > 1., ], "problematic_campaign.csv", row.names = FALSE)
  
  ## 6. calculate age level coverage
  tab$coverage <- tab$fvps / (tab$population+1) # plus one to avoid 0 pop for old age
  tab <- tab[c("vaccine", "activity_type", "country", "year", "age", "gavi_support", "population", "coverage", "fvps")]
  
  ## cap fvps by population, and coverage by 1 - eg. AFG 2015 Measles campagin ; HTI Measles 2025
  ## This operation makes sense - you cannot vaccinate more people than population
  ## In addition, we need to cap coverage by 1 for method3 implementation (not happening for now)
  i <- tab$coverage > 1. & !is.na(tab$coverage)
  tab$coverage[i] <- 1
  tab$fvps[i] <- tab$population[i]
  
  ## 8. make a local temporary table
  ## so that we only manipulate pop-coverage-fvps once
  DBI::dbWriteTable(con, "temporary_coverage_fvps", tab, temporary = TRUE, overwrite = TRUE)
  message("Temporary table created - Success!")
  
}

impact_output <- function(impact, method) {
  ## bind output
  impact_full <- NULL
  impact_simplified <- NULL
  for(i in seq_along(impact)){
    impact_full <- rbind(impact[[i]]$impact_full, impact_full)
    impact_simplified <- rbind(impact[[i]]$impact_simplified, impact_simplified)
  }
  ## order output
  if(method == "method1") {
    impact_full <- impact_full[order(impact_full$index, impact_full$country, impact_full$year, impact_full$age), ]
    impact_simplified <- impact_simplified[order(impact_simplified$index, impact_simplified$impact_type, impact_simplified$country, impact_simplified$year), ]
    
  } else {
    impact_full <- impact_full[order(impact_full$index, impact_full$support_type, impact_full$country, impact_full$year, impact_full$age), ]
    impact_simplified <- impact_simplified[order(impact_simplified$index, impact_simplified$impact_type, impact_simplified$support_type, impact_simplified$country, impact_simplified$year), ]
    
  }
  row.names(impact_full) <- NULL
  row.names(impact_simplified) <- NULL
  return(list(impact_full = impact_full, impact_simplified = impact_simplified))
}

line_up <- function(recipe, group, burden_outcomes) {
  # The line_up function will subject to change, once impact recipes are imported into montagu
  stopifnot(nrow(recipe) == 1L)
  ### burden_outcome
  burden_outcome <- unlist(strsplit(recipe$burden_outcome, ","))
  j <- duplicated(burden_outcome)
  if(any(j)) stop("Duplicated burden_outcomes not expected.")
  
  j <- match(burden_outcome, burden_outcomes$code)
  k <- is.na(j)
  if(any(k)) stop("Unexpected burden outcome in recipe file.")
  burdens <- data.frame(index = recipe$index,
                        burden_outcome_id = burden_outcomes$id[j])
  
  ### meta ids
  ids <- c("responsibility_set_id", "responsibility_id", "burden_estimate_set_id", "stochastic_burden_estimate_set_id")
  
  j <- recipe$touchstone == group$touchstone &
    recipe$modelling_group == group$modelling_group &
    recipe$disease == group$disease &
    recipe$baseline == group$scenario
  v <- unique(group[j, ][ids])
  if(nrow(v) == 0) stop("Impact recipe error: no known baseline burden_estimate_set.")
  stopifnot(nrow(v) == 1L)
  v$coeff <- 1
  recipe_base <- cbind(recipe, v)
  
  k <- recipe$touchstone == group$touchstone &
    recipe$modelling_group == group$modelling_group &
    recipe$disease == group$disease &
    recipe$focal_ingredient == group$scenario
  w <- unique(group[k, ][ids])
  if(nrow(w) == 0) stop("Impact recipe error: no known focal burden_estimate_set.")
  stopifnot(nrow(w) == 1L)
  w$coeff <- -1
  recipe_focal <- cbind(recipe, w)
  recipe_base$coverage_set_id <- recipe_focal$coverage_set_id
  
  stopifnot(v$responsibility_set_id == w$responsibility_set_id)
  recipe <- rbind(recipe_base, recipe_focal)
  recipe2 <- merge(recipe, burdens, by = "index")
  recipe2
  
}

sql_in <- function(items) {
  paste0("(",
         paste(items, collapse=","),
         paste0(")"))
}

sql_in_text <- function(items) {
  paste0("('",
         paste(items, collapse="' ,'"),
         paste0("')"))
}

merge_in <- function(dat, d, cols) {
  i <- match(dat$.code, d$.code)
  keep <- d[i, cols, drop = FALSE]
  rownames(keep) <- NULL
  nms <- names(cols)
  if (!is.null(nms)) {
    nms[!nzchar(nms)] <- cols[!nzchar(nms)]
    names(keep) <- nms
  }
  v <- cbind(dat, keep)
  v <- v[-which(names(v) == ".code")]
}

read_sql <- function(file_name) {
  path_sql <- system.file("sql/impact_method2_metadata", package = "jenner",
                          mustWork = TRUE)
  sql <- read_file(file.path(path_sql, paste0(file_name,".sql")))
}

