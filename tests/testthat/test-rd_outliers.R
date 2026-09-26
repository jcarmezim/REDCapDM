test_that("outlier_iqr flags values outside the Tukey fences", {
  x <- c(1, 2, 3, 4, 5, 100)
  res <- outlier_iqr(x)

  expect_type(res, "logical")
  expect_length(res, length(x))
  expect_false(res[1])
  expect_true(res[6])
})

test_that("outlier_iqr preserves NA values", {
  x <- c(1, 2, NA, 4, 100)
  res <- outlier_iqr(x)

  expect_true(is.na(res[3]))
  expect_true(res[5])
})

test_that("outlier_iqr respects the k argument", {
  x <- c(1, 2, 3, 4, 5, 10)

  expect_false(outlier_iqr(x, k = 5)[6])
  expect_true(outlier_iqr(x, k = 0.5)[6])
})

test_that("outlier_iqr errors with invalid arguments", {
  expect_error(outlier_iqr("a"), "must be a numeric vector")
  expect_error(outlier_iqr(1:5, k = -1), "non-negative number")
  expect_error(outlier_iqr(1:5, k = c(1, 2)), "non-negative number")
})

test_that("outlier_iqr warns and returns NA with fewer than 2 non-missing values", {
  expect_warning(res <- outlier_iqr(c(1, NA, NA)), "Not enough non-missing values")
  expect_true(all(is.na(res)))
})

test_that("outlier_zscore flags values above the threshold", {
  x <- c(rep(10, 19), 200)
  res <- outlier_zscore(x)

  expect_type(res, "logical")
  expect_length(res, length(x))
  expect_false(res[1])
  expect_true(res[20])
})

test_that("outlier_zscore preserves NA values", {
  x <- c(rep(10, 19), NA, 200)
  res <- outlier_zscore(x)

  expect_true(is.na(res[20]))
})

test_that("outlier_zscore respects the threshold argument", {
  x <- c(rep(10, 19), 200)

  expect_false(outlier_zscore(x, threshold = 5)[20])
  expect_true(outlier_zscore(x, threshold = 1)[20])
})

test_that("outlier_zscore errors with invalid arguments", {
  expect_error(outlier_zscore("a"), "must be a numeric vector")
  expect_error(outlier_zscore(1:5, threshold = -1), "positive number")
  expect_error(outlier_zscore(1:5, threshold = c(1, 2)), "positive number")
})

test_that("outlier_zscore warns and returns NA with fewer than 2 non-missing values", {
  expect_warning(res <- outlier_zscore(c(1, NA, NA)), "Not enough non-missing values")
  expect_true(all(is.na(res)))
})

test_that("outlier_zscore warns and returns FALSE when sd is zero", {
  x <- c(5, 5, 5, NA)
  expect_warning(res <- outlier_zscore(x), "standard deviation")
  expect_identical(res, c(FALSE, FALSE, FALSE, NA))
})

test_that("outlier_iqr and outlier_zscore work inside rd_query()", {
  toy_data <- data.frame(record_id = 1:6, potassium = c(3.5, 3.8, 4.0, 4.2, 3.9, 15))
  toy_dic <- data.frame(
    field_name = "potassium",
    field_label = "Potassium",
    form_name = "labs",
    branching_logic_show_field_only_if = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- rd_query(data = toy_data, dic = toy_dic, variables = "potassium", expression = "outlier_iqr(x)")

  expect_equal(nrow(res$queries), 1)
  expect_equal(res$queries$Identifier, 6)
})
