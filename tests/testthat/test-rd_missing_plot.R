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

test_that("rd_missing_plot errors when `by` is not in the dataset", {
  toy_data <- data.frame(record_id = 1:3, age = c(1, 2, NA))
  toy_dic <- make_dic()

  expect_error(
    rd_missing_plot(data = toy_data, dic = toy_dic, variables = "age", by = "not_a_var"),
    "was not found in the dataset"
  )
})

test_that("rd_missing_plot stratifies by a grouping variable for non-longitudinal data", {
  toy_data <- data.frame(
    record_id = 1:6,
    age = c(20, NA, 30, NA, 50, 60),
    dag = c("site_a", "site_a", "site_a", "site_b", "site_b", "site_b"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()

  res <- rd_missing_plot(data = toy_data, dic = toy_dic, variables = "age", by = "dag", plot = FALSE)
  s <- res$summary

  expect_setequal(s$By, c("site_a", "site_b"))
  expect_equal(s$Pct_missing[s$By == "site_a"], 100 * 1 / 3)
  expect_equal(s$Pct_missing[s$By == "site_b"], 100 * 1 / 3)
})

test_that("rd_missing_plot excludes rows with a missing `by` value, with a warning", {
  toy_data <- data.frame(
    record_id = 1:4,
    age = c(20, NA, 30, 40),
    dag = c("site_a", "site_a", NA, "site_b"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()

  expect_warning(
    res <- rd_missing_plot(data = toy_data, dic = toy_dic, variables = "age", by = "dag", plot = FALSE),
    "missing `dag` value"
  )

  expect_equal(sum(res$summary$N), 3)
})

test_that("rd_missing_plot excludes the `by` variable itself from the summarized variables", {
  toy_data <- data.frame(
    record_id = 1:4,
    age = c(20, NA, 30, 40),
    dag = c("site_a", "site_a", "site_b", "site_b"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_dic()
  toy_dic <- rbind(toy_dic, data.frame(field_name = "dag", field_label = "DAG", form_name = "demo"))

  res <- rd_missing_plot(data = toy_data, dic = toy_dic, by = "dag", plot = FALSE)
  expect_false("dag" %in% res$summary$Variable)
})

test_that("rd_missing_plot facets by `by` for longitudinal data with event_form", {
  # record_id 1-2 (rows 1-4) are site_a, record_id 3-4 (rows 5-8) are site_b
  # (rep(c("site_a", "site_a", "site_b", "site_b"), each = 2) duplicates each
  # of those 4 elements in place, giving 4 site_a rows then 4 site_b rows).
  toy_data <- data.frame(
    record_id = rep(1:4, each = 2),
    redcap_event_name = rep(c("baseline_arm_1", "followup_arm_1"), 4),
    age = c(20, NA, 30, NA, NA, NA, NA, NA),
    dag = rep(c("site_a", "site_a", "site_b", "site_b"), each = 2),
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
    variables = "age", by = "dag", plot = FALSE
  )
  s <- res$summary

  # 'age' is only collected at baseline (record_id 1 & 2's baseline rows are
  # site_a, record_id 3 & 4's baseline rows are site_b): site_a's baseline
  # ages (20, 30) are never missing, site_b's (NA, NA) are always missing.
  expect_equal(s$Pct_missing[s$By == "site_a" & s$Event == "baseline_arm_1"], 0)
  expect_equal(s$Pct_missing[s$By == "site_b" & s$Event == "baseline_arm_1"], 100)
})

test_that("rd_missing_plot returns a faceted ggplot object when `by` and event_form are both specified", {
  skip_if_not_installed("ggplot2")

  toy_data <- data.frame(
    record_id = rep(1:4, each = 2),
    redcap_event_name = rep(c("baseline_arm_1", "followup_arm_1"), 4),
    age = c(20, NA, 30, NA, 40, NA, NA, NA),
    dag = rep(c("site_a", "site_a", "site_b", "site_b"), each = 2),
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
    variables = "age", by = "dag", plot = TRUE
  )
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$plot$facet, "FacetWrap")
})
