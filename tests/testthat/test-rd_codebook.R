make_dic <- function() {
  data.frame(
    field_name = c("age", "sex", "visit_date"),
    field_label = c("Age", "Sex", "Visit date"),
    form_name = c("demo", "demo", "demo"),
    stringsAsFactors = FALSE
  )
}

make_data <- function() {
  data.frame(
    record_id = 1:6,
    age = c(20, 30, 40, NA, 50, 60),
    sex = factor(c("Male", "Female", "Male", "Female", NA, "Male")),
    visit_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", NA, "2020-04-01", "2020-05-01")),
    arm = c("A", "A", "B", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
}

test_that("rd_codebook errors when data or dic are missing", {
  expect_error(rd_codebook(data = NULL, dic = NULL), "Both `data` and `dic")
})

test_that("rd_codebook errors when requested variables are not in the dataset", {
  expect_error(
    rd_codebook(data = make_data(), dic = make_dic(), variables = c("age", "not_a_var")),
    "not found in the dataset"
  )
})

test_that("rd_codebook errors when `by` is not in the dataset", {
  expect_error(
    rd_codebook(data = make_data(), dic = make_dic(), variables = "age", by = "not_a_var"),
    "was not found in the dataset"
  )
})

test_that("rd_codebook summarizes numeric variables with mean (SD) and reports missingness", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "age")

  tab <- res$table
  expect_equal(tab$Level, c("Mean (SD)", "Missing"))

  x <- c(20, 30, 40, 50, 60)
  expect_equal(tab$Overall[tab$Level == "Mean (SD)"], sprintf("%.1f (%.1f)", mean(x), sd(x)))
  expect_equal(tab$Overall[tab$Level == "Missing"], "1 (16.7%)")
})

test_that("rd_codebook supports median_iqr and both numeric summaries", {
  res_med <- rd_codebook(data = make_data(), dic = make_dic(), variables = "age", numeric_summary = "median_iqr")
  expect_equal(res_med$table$Level, c("Median [Q1, Q3]", "Missing"))

  res_both <- rd_codebook(data = make_data(), dic = make_dic(), variables = "age", numeric_summary = "both")
  expect_equal(res_both$table$Level, c("Mean (SD)", "Median [Q1, Q3]", "Missing"))
})

test_that("rd_codebook summarizes factor variables with n (%) per level", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "sex")
  tab <- res$table

  expect_equal(tab$Level, c("Female", "Male", "Missing"))
  expect_equal(tab$Overall[tab$Level == "Female"], "2 (40.0%)")
  expect_equal(tab$Overall[tab$Level == "Male"], "3 (60.0%)")
  expect_equal(tab$Overall[tab$Level == "Missing"], "1 (16.7%)")
})

test_that("rd_codebook summarizes Date variables with a range", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "visit_date")
  tab <- res$table

  expect_equal(tab$Level, c("Range", "Missing"))
  expect_equal(tab$Overall[tab$Level == "Range"], "2020-01-01 to 2020-05-01")
})

test_that("rd_codebook stratifies by a grouping variable and adds an Overall column", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "sex", by = "arm")
  tab <- res$table

  expect_true(all(c("Overall", "A", "B") %in% names(tab)))

  # Arm A (rows 1, 2, 5): Male, Female, NA -> 1 Male, 1 Female, 1 missing
  expect_equal(tab$A[tab$Level == "Male"], "1 (50.0%)")
  expect_equal(tab$A[tab$Level == "Female"], "1 (50.0%)")
  expect_equal(tab$A[tab$Level == "Missing"], "1 (33.3%)")

  # Arm B (rows 3, 4, 6): Male, Female, Male -> 2 Male, 1 Female, no missing
  expect_equal(tab$B[tab$Level == "Male"], "2 (66.7%)")
  expect_equal(tab$B[tab$Level == "Female"], "1 (33.3%)")
  expect_equal(tab$B[tab$Level == "Missing"], "0 (0.0%)")
})

test_that("rd_codebook keeps a consistent set of levels across strata, even when a level is absent in one group", {
  d <- make_data()
  d$sex[d$arm == "B"] <- factor("Male")[1] # force arm B to have only Male

  res <- rd_codebook(data = d, dic = make_dic(), variables = "sex", by = "arm")
  tab <- res$table

  expect_true("Female" %in% tab$Level)
  expect_equal(tab$B[tab$Level == "Female"], "0 (0.0%)")
})

test_that("rd_codebook uses dictionary labels", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "age")
  expect_equal(unique(res$table$Label), "Age")
})

test_that("rd_codebook falls back to the variable name when no label is available", {
  dic_nolab <- make_dic()
  dic_nolab$field_label <- NA_character_

  res <- rd_codebook(data = make_data(), dic = dic_nolab, variables = "age")
  expect_equal(unique(res$table$Label), "age")
})

test_that("rd_codebook returns NULL results when return_viewer = FALSE", {
  res <- rd_codebook(data = make_data(), dic = make_dic(), variables = "age", return_viewer = FALSE)
  expect_null(res$results)
})

test_that("rd_codebook defaults to dictionary fields present in the data", {
  res <- rd_codebook(data = make_data(), dic = make_dic())
  expect_setequal(unique(res$table$Variable), c("age", "sex", "visit_date"))
})
