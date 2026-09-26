make_queries <- function() {
  data.frame(
    Identifier = c("1", "2"),
    DAG = c("Site A", "Site B"),
    Event = c("Baseline", "Baseline"),
    Field = c("age", "age"),
    Query = c("The value is NA and it should not be missing", "The value is NA and it should not be missing"),
    Code = c("1-1", "2-1"),
    stringsAsFactors = FALSE
  )
}

test_that("add_status_columns adds Status and Comment columns with defaults", {
  q <- make_queries()
  res <- add_status_columns(q)

  expect_true(all(c("Status", "Comment") %in% names(res)))
  expect_true(all(res$Status == "Pending"))
  expect_true(all(res$Comment == ""))
})

test_that("add_status_columns preserves existing Status/Comment values", {
  q <- make_queries()
  q$Status <- c("Resolved", "Pending")
  q$Comment <- c("Fixed manually", "")

  res <- add_status_columns(q)

  expect_equal(res$Status, c("Resolved", "Pending"))
  expect_equal(res$Comment, c("Fixed manually", ""))
})

test_that("run_query_app errors informatively when shiny/DT are not available", {
  skip_if(requireNamespace("shiny", quietly = TRUE) && requireNamespace("DT", quietly = TRUE),
          "shiny and DT are installed; the informative error path can't be triggered here")

  expect_error(run_query_app(make_queries()), "shiny.*DT")
})

test_that("run_query_app builds a shiny.appobj without launching it when launch = FALSE", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  app <- run_query_app(make_queries(), launch = FALSE)

  expect_s3_class(app, "shiny.appobj")
})

test_that("run_query_app accepts the list output of rd_query()/check_queries()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  app <- run_query_app(list(queries = make_queries()), launch = FALSE)

  expect_s3_class(app, "shiny.appobj")
})

test_that("run_query_app works without REDCap push arguments (export-only mode)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  app <- run_query_app(make_queries(), launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})
