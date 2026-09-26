make_queries <- function() {
  data.frame(
    Identifier = c("1", "2"),
    DAG = c("-", "-"),
    Event = c("-", "-"),
    Instrument = c("Demographics", "Demographics"),
    Field = c("age", "age"),
    Repetition = c("-", "-"),
    Description = c("Age", "Age"),
    Query = c("The value is NA and it should not be missing", "The value is NA and it should not be missing"),
    Code = c("1-1", "2-1"),
    stringsAsFactors = FALSE
  )
}

test_that("rd_write_queries errors when uri/token/field are missing", {
  q <- make_queries()
  expect_error(rd_write_queries(queries = q, uri = NULL, token = "x", field = "note"), "uri.*and.*token")
  expect_error(rd_write_queries(queries = q, uri = "http://x", token = NULL, field = "note"), "uri.*and.*token")
  expect_error(rd_write_queries(queries = q, uri = "http://x", token = "x", field = NULL), "single REDCap field name")
})

test_that("rd_write_queries errors when queries has no Identifier column", {
  expect_error(
    rd_write_queries(queries = data.frame(x = 1), uri = "http://x", token = "t", field = "note"),
    "Identifier"
  )
})

test_that("rd_write_queries accepts the list output of rd_query()/check_queries()", {
  q <- make_queries()
  res <- rd_write_queries(queries = list(queries = q), uri = "http://x", token = "t", field = "note")

  expect_equal(nrow(res$data), 2)
  expect_true(res$dry_run)
})

test_that("rd_write_queries defaults to using the Query column as the value", {
  q <- make_queries()
  res <- rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note")

  expect_equal(res$data$note, q$Query)
  expect_equal(res$data$record_id, c("1", "2"))
})

test_that("rd_write_queries accepts a literal string as value", {
  q <- make_queries()
  res <- rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note", value = "Please review")

  expect_true(all(res$data$note == "Please review"))
})

test_that("rd_write_queries accepts a column name as value", {
  q <- make_queries()
  res <- rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note", value = "Description")

  expect_equal(res$data$note, q$Description)
})

test_that("rd_write_queries errors on repeating instrument rows", {
  q <- make_queries()
  q$Repetition <- c("form-1", "-")

  expect_error(
    rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note"),
    "repeating instrument"
  )
})

test_that("rd_write_queries errors when the same record maps to conflicting values", {
  q <- make_queries()
  q$Identifier <- c("1", "1")
  q$Query <- c("Value A", "Value B")

  expect_error(
    rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note"),
    "more than one value"
  )
})

test_that("rd_write_queries resolves event labels back to raw event names using `data`", {
  q <- data.frame(
    Identifier = c("1", "2"),
    Event = c("Baseline visit", "Follow-up visit"),
    Query = c("Q1", "Q2"),
    stringsAsFactors = FALSE
  )

  toy_data <- data.frame(
    record_id = c("1", "2"),
    redcap_event_name = c("baseline_visit_arm_1", "followup_visit_arm_1"),
    redcap_event_name.factor = c("Baseline visit", "Follow-up visit"),
    stringsAsFactors = FALSE
  )

  res <- rd_write_queries(data = toy_data, queries = q, uri = "http://x", token = "t", field = "note")

  expect_equal(res$data$redcap_event_name, c("baseline_visit_arm_1", "followup_visit_arm_1"))
})

test_that("rd_write_queries errors when an event label cannot be resolved", {
  q <- data.frame(
    Identifier = "1",
    Event = "Unknown event",
    Query = "Q1",
    stringsAsFactors = FALSE
  )

  toy_data <- data.frame(
    record_id = "1",
    redcap_event_name = "baseline_visit_arm_1",
    redcap_event_name.factor = "Baseline visit",
    stringsAsFactors = FALSE
  )

  expect_error(
    rd_write_queries(data = toy_data, queries = q, uri = "http://x", token = "t", field = "note"),
    "Could not resolve"
  )
})

test_that("rd_write_queries does not call the API in dry_run mode", {
  q <- make_queries()

  called <- FALSE
  fake_write <- function(...) {
    called <<- TRUE
  }
  mockery::stub(rd_write_queries, "REDCapR::redcap_write", fake_write)

  res <- rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note", dry_run = TRUE)

  expect_false(called)
  expect_null(res$result)
})

test_that("rd_write_queries calls REDCapR::redcap_write() when dry_run = FALSE", {
  q <- make_queries()

  captured <- NULL
  fake_write <- function(ds, redcap_uri, token, ...) {
    captured <<- ds
    list(success = TRUE, records_affected_count = nrow(ds))
  }
  mockery::stub(rd_write_queries, "REDCapR::redcap_write", fake_write)

  res <- rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note", dry_run = FALSE)

  expect_false(res$dry_run)
  expect_equal(nrow(captured), 2)
  expect_true(res$result$success)
})

test_that("rd_write_queries surfaces a clear error when the API write fails", {
  q <- make_queries()

  fake_write <- function(...) stop("boom")
  mockery::stub(rd_write_queries, "REDCapR::redcap_write", fake_write)

  expect_error(
    rd_write_queries(queries = q, uri = "http://x", token = "t", field = "note", dry_run = FALSE),
    "REDCap write failed"
  )
})
