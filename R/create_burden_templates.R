
create_burden_template <- function(files, files_path, assert_files = c("Country_Disease_Table.csv", "model_meta.csv", "model_outcomes.csv"), central_templates_only = FALSE) {

  assert_has_files(files, assert_files)
  files <- paste0(files_path, files)

  country_disease_table <- read_csv(files[1])
  model_meta_data <- read_csv(files[2])
  model_outcomes <- read_csv(files[3])

  names(country_disease_table)[names(country_disease_table) == "Country"] <-
    "country_name"
  names(country_disease_table)[names(country_disease_table) == "ISO3.code"] <-
    "country"

  ## making sure that the final year makes sense:
  year_end <- 2030
  model_meta_data$year_max <- apply(cbind(2100, year_end + model_meta_data$burden_max_age), 1, min)
  ## write templates
  output_files <- sapply(seq_along(model_meta_data[, 1]),
                         function(i) write_template_files(model_meta_data[i, ],
                                                          year_end,
                                                          country_disease_table,
                                                          model_outcomes,
                                                          central_templates_only))
}

### From Nick's insert_function.R
# function adds new rows to dataframe with contents of value_insert
# to the column column_name

insert_dataframe = function(data,column_name,value_insert){

  length_values <- length(value_insert)

  data$column_name <- value_insert[1]

  cf <- "column_name"
  names(data)[names(data)%in%cf] <- c(column_name)

  data1 <- data

  for(i in 2:length_values){
    data1[, column_name] <- value_insert[i]
    data <- rbind(data,data1)
  }

  return(data)

}

# inserts country name into every line of data frame for each country iso code

country_function <- function(k,country_n,country_ISOcode,country_data){

  country_name_rep <- rep(country_n[k],length(country_data[country_data$country == country_ISOcode[k], ]$country))
  return(country_name_rep)

}

# outer wrapper function that produces the csv files
# that selects the models associated with each disease

model_select_function <- function(j){

  model_name <- unique(model_meta_data[model_meta_data$disease == disease_name[j], ]$modelling_group)

  sapply(1:length(model_name),generate_csv,model_name,j)

}

# inner wrapper function that produces the csv files
# this function has no return

generate_csv <- function(i,model_name,j){

  country_data <- data.frame(disease = disease_name[j])

  age_min <- model_meta_data[model_meta_data$disease == disease_name[j] & model_meta_data$modelling_group == model_name[i], ]$burden_min_age
  age_max <- model_meta_data[model_meta_data$disease == disease_name[j] & model_meta_data$modelling_group == model_name[i], ]$burden_max_age
  year_start <- model_meta_data[model_meta_data$disease == disease_name[j] & model_meta_data$modelling_group == model_name[i], ]$year_min

  country_ISOcode <- unique(country_meta_data[country_meta_data$vaccine == disease_name[j], ]$country)
  country_n <- country_meta_data[country_meta_data$vaccine == disease_name[j], ]$country_name

  country_data$run_id <- ""

  if(disease_name[j] == "HPV"){

    age_values <- c(age_min:age_max)
    age_column_name<-c("age")
    year_values <- c(year_start:year_end)
    year_column_name<-c("year")

    country_data <- insert_dataframe(country_data,year_column_name,year_values)

    country_data <- insert_dataframe(country_data,age_column_name,age_values)

    age_min_k <- age_min + 1

    for(k in age_min_k:age_max){

      country_data1<-HPV_generate_csv(k,j,disease_name,age_min,age_max,year_end)

      country_data <- rbind(country_data,country_data1)
      rm(country_data1)

      country_data <- country_data[order(country_data$age),]
    }

  }

  if(disease_name[j] != "HPV"){

    year_till <- year_end + age_max
    if(year_till > 2100){year_till <- 2100}

    age_values <- c(age_min:age_max)
    age_column_name<-c("age")
    year_values <- c(year_start:year_till)
    year_column_name<-c("year")

    country_data <- insert_dataframe(country_data,year_column_name,year_values)

    country_data <- insert_dataframe(country_data,age_column_name,age_values)

  }

  country_column_name <-c("country")

  country_data <- insert_dataframe(country_data,country_column_name,country_ISOcode)

  b<-lapply(1:length(country_n),country_function,country_n,country_ISOcode,country_data)

  country_data$country_name <- unlist(b)

  outcome_name <- unique(model_outcomes[model_outcomes$disease == "standard" & model_outcomes$model == "standard", ]$outcome)

  if(disease_name[j] == "Hib3" & model_name[i] == "JHU-Tam"){
    outcome_name <- unique(model_outcomes[model_outcomes$disease == disease_name[j] & model_outcomes$model == model_name[i], ]$outcome)
  }

  if(disease_name[j] == "HepB" & model_name[i] == "IC-Hallett"){
    outcome_name <- unique(model_outcomes[model_outcomes$disease == disease_name[j] & model_outcomes$model == model_name[i], ]$outcome)
  }

  if(disease_name[j] == "HepB" & model_name[i] == "CDA-Razavi"){
    outcome_name <- unique(model_outcomes[model_outcomes$disease == disease_name[j] & model_outcomes$model == model_name[i], ]$outcome)
  }

  if(disease_name[j] == "HepB" & model_name[i] == "Li"){
    outcome_name <- unique(model_outcomes[model_outcomes$disease == disease_name[j] & model_outcomes$model == model_name[i], ]$outcome)
  }

  if(disease_name[j] == "Rubella" & model_name[i] == "PHE-Vynnycky"){
    outcome_name <- unique(model_outcomes[model_outcomes$disease == disease_name[j] & model_outcomes$model == model_name[i], ]$outcome)
  }

  for(l in seq_along(outcome_name)){
    country_data[, paste0(outcome_name[l])] <- ""
  }

  country_data_stochastic <- country_data

  country_data <- country_data[, !(names(country_data) %in% "run_id")]

  title_stochastic <- paste0("stochastic_burden_template_",disease_name[j],"-",model_name[i],".csv")
  title <- paste0("central_burden_template_",disease_name[j],"-",model_name[i],".csv")

  write.csv(country_data_stochastic, file = title_stochastic, row.names = FALSE)
  write.csv(country_data, file = title, row.names = FALSE)

  return()

}

# function that generates the triangular form of the data (between 2031 to around 2130)
# needed for the HPV burden estimate templates

HPV_generate_csv <- function(k,j,disease_name,age_min,age_max,year_end){

  country_data1 <- data.frame(disease = disease_name[j])
  country_data1$run_id <- ""

  age_values <- c(k:age_max)
  age_column_name<-c("age")
  year_values <- c(year_end-age_min+k)
  year_column_name<-c("year")

  country_data1$year <- year_values

  if(length(age_values)>1){
    country_data1 <- insert_dataframe(country_data1,age_column_name,age_values)
  }

  if(length(age_values)==1){
    country_data1$age <- age_max
  }

  return(country_data1)

}

### From Nick's country_function

get_countries_for_model <- function(model_meta_line,
                                    country_disease_table) {

  disease_curr <-  model_meta_line$disease
  modelling_group_curr <- model_meta_line$modelling_group

  sel <- country_disease_table[, grep(disease_curr,
                                      names(country_disease_table))] == "Y"

  countries_for_model <- cbind(disease = disease_curr,
                               modelling_group = modelling_group_curr,
                               country_disease_table[sel, c("country",
                                                            "country_name")])

  return(countries_for_model)
}

make_filled_columns <- function(model_meta_line,
                                country_disease_table) {

  ## need to generate columns:
  ## cols_filled = c("disease", "year", "age", "country", "country_name")

  country_df <- get_countries_for_model(model_meta_line,
                                        country_disease_table)

  ages <- model_meta_line$burden_min_age : model_meta_line$burden_max_age
  years <- model_meta_line$year_min : model_meta_line$year_max

  out <- expand.grid(age = ages, year = years, country = country_df$country)
  out <- cbind(disease = model_meta_line$disease, out)
  out$country_name <-
    country_df$country_name[match(out$country, country_df$country)]
  ## NOTE: The order here is very important - upload will not work if
  ## it is not correct.
  out[c("disease", "year", "age", "country", "country_name")]
}

find_model_outcomes <- function(disease_curr,
                                modelling_group_curr,
                                model_outcomes) {

  outcomes_curr <- model_outcomes %>%
    filter(disease == disease_curr & model == modelling_group_curr) %>%
    select(outcome)
  if(nrow(outcomes_curr) == 0) { ## use the standard outcomes:
    outcomes_curr <- model_outcomes %>%
      filter(disease == "standard" & model == "standard") %>%
      select(outcome)
  }
  return(outcomes_curr)
}


write_template_files <- function(model_meta_line,
                                 year_end,
                                 country_disease_table,
                                 model_outcomes,
                                 central_templates_only = FALSE) {

  out <- make_filled_columns(model_meta_line,
                             country_disease_table)

  ## knocking out the bottom triangle post 2030 - needs to be done for
  ## List, Trivac and HPV:
  if(model_meta_line$disease == "HPV" |
     model_meta_line$model_name %in% c("TRIVAC", "LiST")) {
    out <- knock_out_triangle(out, model_meta_line, year_end)
  }

  outcomes_curr <- find_model_outcomes(model_meta_line$disease,
                                       model_meta_line$modelling_group,
                                       model_outcomes)
  if (any(duplicated(outcomes_curr$outcome))) {
    stop("Duplicated model outcomes")
  }

  tmp <- matrix("", nrow = nrow(out), ncol = nrow(outcomes_curr))
  colnames(tmp) <- outcomes_curr$outcome
  out <- cbind(out, tmp)

  expected <- c("disease", "year", "age", "country", "country_name",
                "cohort_size")
  if (!identical(names(out)[seq_along(expected)], expected)) {
    stop("incorrect template headers")
  }

  ## writing the central template file:
  filename1 <- sprintf("central_burden_template_%s-%s.csv",
                      model_meta_line$disease,
                      model_meta_line$modelling_group)

  write.csv(out, filename1, row.names = FALSE)
  if ( !central_templates_only ) {
    ## adapting the file to the stochastic output:
    out <- cbind(disease = out$disease, run_id = "", out[, -1])
    filename2 <- sprintf("stochastic_burden_template_%s-%s.csv",
                        model_meta_line$disease,
                        model_meta_line$modelling_group)

    write.csv(out, filename2, row.names = FALSE)
    return(list(filename1, filename2))
  } else {
    return(filename1)
  }
}


knock_out_triangle <- function(out, model_meta_line, year_end) {

  year_end <- 2030
  year_birth_last <- year_end - model_meta_line$burden_min_age
  ## this is good for Trivac:
  out <- out %>%
    mutate(year_birth = year - age) %>%
    filter(year_birth <= year_birth_last) %>%
    select(disease, year, age, country, country_name)
  return(out)
}

#### From Nick's check_function.R
check_central_columns_filled <- function(dat, cols_filled) {
  if(sum(is.na(match(cols_filled, names(dat)))) != 0)
    warning("Not all columns that should be filled in are available.")

  if(sum(is.na(dat[, cols_filled])) != 0)
    warning("Missing entries in some columns that should be filled in.")

}

check_central_disease <- function(dat, disease_curr) {
  tt = table(dat$disease, useNA = "ifany")
  if(length(tt) != 1)
    warning("More or less than a single disease present.")
  if(names(tt) != disease_curr)
    warning("disease in file does not match filename.")
}

check_central_year <- function(dat,
                               model_meta,
                               disease_curr,
                               modelling_group_curr) {

  year_range <- model_meta %>%
    select(year_min, year_max)

  if(min(dat$year) != year_range$year_min)
    warning("First year not as given in model_meta file.")
  if(max(dat$year) != year_range$year_max)
    warning("Last year not as given in model_meta file.")

  tt = table(dat$year)
  if(!all(names(tt) == year_range$year_min : year_range$year_max))
    warning("Not all years in the specified year range appear.")

  if(length(table(tt)) != 1) {

    tmp <- dat %>%
      group_by(age, year) %>%
      summarise(npt = n())
    g <- ggplot(tmp, aes(x = year, y = age, colour = npt)) +
      geom_point() +
      ggtitle(sprintf("%s: %s", modelling_group_curr, disease_curr)) +
      theme_light()
    print(g)

    warning("There are not the same number of entries for each year.")
  }
}

check_central_age <- function(dat,
                              model_meta,
                              disease_curr,
                              modelling_group_curr) {

  age_range <- model_meta %>%
    select(burden_min_age, burden_max_age)

  if(min(dat$age) != age_range$burden_min_age)
    warning("First age not as given in model_meta file.")
  if(max(dat$age) != age_range$burden_max_age) {
    warning("Last age not as given in model_meta file.")
  }

  tt = table(dat$age)
  if(!all(names(tt) == age_range$burden_min_age : age_range$burden_max_age))
    warning("Not all years in the specified year range appear.")

  if(length(table(tt)) != 1)
    warning("There are not the same number of entries for each age.")
  ## this could still be ok - for instance for HPV.

}

check_central_country <- function(dat,
                                  model_meta,
                                  country_disease_table,
                                  countries_excluded) {

  disease_curr <- model_meta$disease
  n_countries <- model_meta$number_countries

  country_meta_data <- get_countries_for_model(model_meta,
                                               country_disease_table,
                                               countries_excluded)

  countries <- country_meta_data %>%
    filter(disease == disease_curr) %>%
    select(country, country_name)

  tt <- table(dat$country)
  if(length(tt) != n_countries)
    warning(sprintf("Number of countries = %d, should be %d.",
                    length(tt), n_countries))

  ## to the countries match what's pulled of Montagu?
  if(sum(is.na(match(names(tt), countries$country))) != 0)
    warning("There are countries in the .csv file that are not listed on Montagu for this disease and touchstone.")
  if(sum(is.na(match(countries$country, names(tt)))) != 0)
    warning("There are countries listed in Montagu for this disease and touchstone that are missing in the .csv.")

  if(length(table(tt)) != 1)
    warning("The number of rows per country varies between countries.")

  ## checking country names and country ISO codes:
  ttn <- table(table(dat$country, dat$country_name))
  ## this should be mostly 0, but have n_country entries of some other number.
  if(names(ttn)[1] != "0" | ttn[2] != n_countries)
    warning("The country names and countries don't match up.")


}
################################################################################
compare_central_stochastic_columns <- function(dat, dat_stochastic) {

  ## comparing columns of the central and stochastic files:
  mm = match(names(dat_stochastic), names(dat))
  if(sum(is.na(mm)) != 1) {
    warning("Columns in the central and stochastic files do not match up.")
  } else {
    new_col <- names(dat_stochastic[is.na(mm)])
    if(new_col != "run_id")
      warning("Additional column in stochastic file is `%s`, not `run_id`.")
  }

}

compare_central_stochastic_rows <- function(dat, dat_stochastic, cols_filled) {

  if(nrow(dat) != nrow(dat_stochastic))
    warning("number of rows in central and stochastic files does not match.")


  if(!all(dat_stochastic[, cols_filled] == dat[, cols_filled]))
    warning("stochastic and central files are not aligned.")


}

################################################################################
check_model_files <- function(model_meta_line,
                              country_disease_table,
                              countries_excluded) {

  disease_curr <- model_meta_line$disease
  modelling_group_curr <- model_meta_line$modelling_group

  ## reading in both central and stochastic burden template files:
  file_central <- sprintf("central_burden_template_%s-%s.csv",
                          disease_curr, modelling_group_curr)
  file_stochastic <- sprintf("stochastic_burden_template_%s-%s.csv",
                             disease_curr, modelling_group_curr)

  dat <- read.csv(file_central, stringsAsFactors = TRUE)
  dat_stochastic <- read.csv(file_stochastic, stringsAsFactors = TRUE)

  ## checking the central file:
  ## columns that should be filled in:
  cols_filled = c("disease", "year", "age", "country", "country_name")
  check_central_columns_filled(dat, cols_filled)
  check_central_disease(dat, disease_curr)

  check_central_year(dat, model_meta_line, disease_curr, modelling_group_curr)
  check_central_age(dat, model_meta_line, disease_curr, modelling_group_curr)

  check_central_country(dat, model_meta_line, country_disease_table,
                        countries_excluded)


  ## the number of filled columns that are expected but missing:
  n_col_mis <- sum(is.na(match(cols_filled, names(dat))))
  if(n_col_mis != 0)
    warning(sprintf("There are %d pre-filled columns missing among those expected (%s).",
                    n_col_mis, paste(cols_filled, collapse = ", ")))

  ## number of missing entries in the columns that should be filled:
  n_mis_entries <- sum(colSums(is.na(dat[, cols_filled])))
  if(n_mis_entries != 0)
    warning(sprintf("There are %d entries missing missing among those expected in columns %s.",
                    n_col_mis, paste(cols_filled, collapse = ", ")))


  ## finding the expected outcomes:
  outcomes_curr <- find_model_outcomes(disease_curr, modelling_group_curr,
                                       model_outcomes)
  ## the number of outcomes that are expected but missing:
  n_mismatched_outcomes <- sum(is.na(match(outcomes_curr$outcome, names(dat))))
  if(n_mismatched_outcomes != 0)
    warning(sprintf("There are %d outcomes missing of the expected outcomes (%s).",
                    n_mismatched_outcomes, paste(outcomes_curr$outcome, collapse = ", ")))


  ## are the columns right?
  compare_central_stochastic_columns(dat, dat_stochastic)
  compare_central_stochastic_rows(dat, dat_stochastic, cols_filled)

}
