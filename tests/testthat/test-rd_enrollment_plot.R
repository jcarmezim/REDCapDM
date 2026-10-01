test_that("rd_enrollment_plot errors when data is missing", {
  expect_error(rd_enrollment_plot(data = NULL, date_var = "x"), "must be provided")
})

test_that("rd_enrollment_plot errors when record_id is missing", {
  expect_error(rd_enrollment_plot(data = data.frame(x = 1), date_var = "x"), "record_id")
})

test_that("rd_enrollment_plot errors when date_var is not found", {
  toy_data <- data.frame(record_id = 1, enroll_date = as.Date("2021-01-01"))
  expect_error(rd_enrollment_plot(data = toy_data, date_var = "not_a_col"), "was not found")
})

test_that("rd_enrollment_plot errors when date_var is not Date/POSIXct", {
  toy_data <- data.frame(record_id = 1, enroll_date = "2021-01-01", stringsAsFactors = FALSE)
  expect_error(rd_enrollment_plot(data = toy_data, date_var = "enroll_date"), "must be of class")
})

test_that("rd_enrollment_plot errors when `by` is not found", {
  toy_data <- data.frame(record_id = 1, enroll_date = as.Date("2021-01-01"))
  expect_error(
    rd_enrollment_plot(data = toy_data, date_var = "enroll_date", by = "not_a_var"),
    "was not found"
  )
})

test_that("rd_enrollment_plot computes cumulative counts per period", {
  toy_data <- data.frame(
    record_id = 1:4,
    enroll_date = as.Date(c("2021-01-05", "2021-01-20", "2021-02-10", "2021-02-15")),
    stringsAsFactors = FALSE
  )

  res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", plot = FALSE)
  s <- res$summary

  expect_equal(s$N[s$Period == as.Date("2021-01-01")], 2)
  expect_equal(s$N[s$Period == as.Date("2021-02-01")], 2)
  expect_equal(s$Cumulative_N[s$Period == as.Date("2021-01-01")], 2)
  expect_equal(s$Cumulative_N[s$Period == as.Date("2021-02-01")], 4)
})

test_that("rd_enrollment_plot collapses multiple rows per record using the first non-missing date", {
  toy_data <- data.frame(
    record_id = c(1, 1, 2),
    enroll_date = as.Date(c("2021-01-05", NA, "2021-01-10")),
    stringsAsFactors = FALSE
  )

  res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", plot = FALSE)
  s <- res$summary

  expect_equal(sum(s$N), 2)
  expect_equal(s$N[s$Period == as.Date("2021-01-01")], 2)
})

test_that("rd_enrollment_plot stratifies cumulative counts by `by`", {
  toy_data <- data.frame(
    record_id = 1:4,
    enroll_date = as.Date(c("2021-01-05", "2021-01-20", "2021-02-10", "2021-02-15")),
    dag = c("site_a", "site_a", "site_b", "site_b"),
    stringsAsFactors = FALSE
  )

  res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", by = "dag", plot = FALSE)
  s <- res$summary

  expect_setequal(s$By, c("site_a", "site_b"))
  expect_equal(s$Cumulative_N[s$By == "site_a" & s$Period == as.Date("2021-01-01")], 2)
  expect_equal(s$Cumulative_N[s$By == "site_b" & s$Period == as.Date("2021-02-01")], 2)
})

test_that("rd_enrollment_plot excludes records with a missing date, with a warning", {
  toy_data <- data.frame(
    record_id = 1:3,
    enroll_date = as.Date(c("2021-01-05", NA, "2021-01-10")),
    stringsAsFactors = FALSE
  )

  expect_warning(
    res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", plot = FALSE),
    "missing `enroll_date`"
  )
  expect_equal(sum(res$summary$N), 2)
})

test_that("rd_enrollment_plot excludes records with a missing `by` value, with a warning", {
  toy_data <- data.frame(
    record_id = 1:3,
    enroll_date = as.Date(c("2021-01-05", "2021-01-10", "2021-01-15")),
    dag = c("site_a", NA, "site_b"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", by = "dag", plot = FALSE),
    "missing `dag`"
  )
  expect_equal(sum(res$summary$N), 2)
})

test_that("rd_enrollment_plot always returns both N and Cumulative_N in summary regardless of `cumulative`", {
  toy_data <- data.frame(
    record_id = 1:4,
    enroll_date = as.Date(c("2021-01-05", "2021-01-20", "2021-02-10", "2021-02-15")),
    stringsAsFactors = FALSE
  )

  res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", cumulative = FALSE, plot = FALSE)
  expect_named(res$summary, c("Period", "By", "N", "Cumulative_N"))
  expect_equal(res$summary$Cumulative_N, cumsum(res$summary$N))
})

test_that("rd_enrollment_plot returns a ggplot object when plot = TRUE and ggplot2 is available", {
  skip_if_not_installed("ggplot2")

  toy_data <- data.frame(
    record_id = 1:4,
    enroll_date = as.Date(c("2021-01-05", "2021-01-20", "2021-02-10", "2021-02-15")),
    stringsAsFactors = FALSE
  )

  res <- rd_enrollment_plot(data = toy_data, date_var = "enroll_date", plot = TRUE)
  expect_s3_class(res$plot, "ggplot")
})
