context("create_burden_templates")

test_that("burden_templates", {
  files_path <- "jenner-test-data/burden_template_meta_files/"
  files <- c("Country_Disease_Table.csv", "model_meta.csv", "model_outcomes.csv")
  output_files <- unlist(create_burden_template(files, files_path, central_templates_only = TRUE))

  test_path <- "jenner-test-data/burden_templates/"
  test_files <- paste0(test_path, output_files)

  f <- rep(TRUE, length(output_files))
  v <- rep(FALSE, length(output_files))
  for(i in seq_along(output_files)) {
    f1 <- read_csv(output_files[i])
    f2 <- read_csv(test_files[i])
    v[i] <- all.equal(f1, f2)
  }
  file.remove(output_files)
  expect_equal(f, v)
})

