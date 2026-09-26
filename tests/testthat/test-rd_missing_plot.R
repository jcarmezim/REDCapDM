make_dic <- function() {
  data.frame(
    field_name = c("age", "weight", "sbp"),
    field_label = c("Age", "Weight", "Systolic BP"),
    form_name = c("demo", "demo", "vitals"),
    stringsAsFactors = FALSE
  )
}

test_that("rd_missing_plot errors when data or dic are missing", {
  expect_error(rd_missing_plot(data = NULL, dic = NULL), "Both `data` and `dic")
})

test_that("rd_missing_plot computes correct percentages for non-longitudinal data", {
  toy_data <- data.frame(
    record_id = 1:4,
    age = c(20, NA, 30, 40),
    weight = c(NA, NA, 70, 80),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()

  res <- rd_missing_plot(data = toy_data, dic = toy_dic, variables = c("age", "weight"), plot = FALSE)

  expect_named(res, c("summary", "plot"))
  expect_null(res$plot)

  s <- res$summary
  expect_equal(s$Pct_missing[s$Variable == "age"], 25)
  expect_equal(s$Pct_missing[s$Variable == "weight"], 50)
  expect_true(all(is.na(s$Event)))
})

test_that("rd_missing_plot errors when specified variables are not in the dataset", {
  toy_data <- data.frame(record_id = 1:3, age = c(1, 2, NA))
  toy_dic <- make_dic()

  expect_error(
    rd_missing_plot(data = toy_data, dic = toy_dic, variables = c("age", "not_a_var")),
    "not found in the dataset"
  )
})

test_that("rd_missing_plot defaults to dictionary fields present in the data", {
  toy_data <- data.frame(record_id = 1:3, age = c(1, NA, 3))
  toy_dic <- make_dic()

  res <- rd_missing_plot(data = toy_data, dic = toy_dic, plot = FALSE)
  expect_equal(res$summary$Variable, "age")
})

test_that("rd_missing_plot avoids overestimating missingness using event_form", {
  toy_data <- data.frame(
    record_id = rep(1:3, each = 2),
    redcap_event_name = rep(c("baseline_arm_1", "followup_arm_1"), 3),
    age = c(20, NA, 30, NA, 40, NA),
    sbp = c(NA, 120, NA, 130, NA, 140),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()
  toy_event_form <- data.frame(
    unique_event_name = c("baseline_arm_1", "followup_arm_1"),
    form = c("demo", "vitals"),
    stringsAsFactors = FALSE
  )

  res <- rd_missing_plot(
    data = toy_data, dic = toy_dic, event_form = toy_event_form,
    variables = c("age", "sbp"), plot = FALSE
  )

  s <- res$summary

  # 'age' is only collected at baseline, where it's never missing
  expect_equal(s$Pct_missing[s$Variable == "age" & s$Event == "baseline_arm_1"], 0)
  expect_false("followup_arm_1" %in% s$Event[s$Variable == "age"])

  # 'sbp' is only collected at follow-up, where it's never missing
  expect_equal(s$Pct_missing[s$Variable == "sbp" & s$Event == "followup_arm_1"], 0)
  expect_false("baseline_arm_1" %in% s$Event[s$Variable == "sbp"])
})

test_that("rd_missing_plot warns when longitudinal data is provided without event_form", {
  toy_data <- data.frame(
    record_id = rep(1:2, each = 2),
    redcap_event_name = rep(c("baseline_arm_1", "followup_arm_1"), 2),
    age = c(20, NA, 30, 40),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()

  expect_warning(
    res <- rd_missing_plot(data = toy_data, dic = toy_dic, variables = "age", plot = FALSE),
    "event_form"
  )
})

test_that("rd_missing_plot returns a ggplot object when plot = TRUE and ggplot2 is available", {
  skip_if_not_installed("ggplot2")

  toy_data <- data.frame(record_id = 1:4, age = c(20, NA, 30, 40))
  toy_dic <- make_dic()

  res <- rd_missing_plot(data = toy_data, dic = toy_dic, variables = "age", plot = TRUE)
  expect_s3_class(res$plot, "ggplot")
})
