old_data <- data.frame(
  record_id = c(1, 2, 3),
  age = c(45, 50, 60),
  sex = c("Male", "Female", "Male"),
  stringsAsFactors = FALSE
)

new_data <- data.frame(
  record_id = c(1, 2, 4),
  age = c(45, 51, 70),
  sex = c("Male", "Female", "Female"),
  stringsAsFactors = FALSE
)

test_that("rd_diff errors when old/new are not data frames", {
  expect_error(rd_diff(old = list(), new = new_data), "must be a data frame")
  expect_error(rd_diff(old = old_data, new = "x"), "must be a data frame")
})

test_that("rd_diff errors when an id_vars column is missing", {
  expect_error(rd_diff(old = data.frame(x = 1), new = new_data), "must contain the identifier column")
  expect_error(rd_diff(old = old_data, new = new_data, id_vars = "not_a_col"), "must contain the identifier column")
})

test_that("rd_diff errors with multiple report titles", {
  expect_error(rd_diff(old_data, new_data, report_title = c("a", "b")), "only one")
})

test_that("rd_diff errors with duplicated id_vars combinations", {
  dup <- rbind(old_data, old_data[1, ])
  expect_error(rd_diff(dup, new_data), "Duplicated combinations")
})

test_that("rd_diff correctly classifies added, removed, modified and unchanged rows", {
  res <- rd_diff(old_data, new_data)

  expect_type(res, "list")
  expect_named(res, c("diff", "results"))

  diff <- res$diff

  expect_equal(as.character(diff$Status[diff$record_id == "4"]), "Added")
  expect_equal(as.character(diff$Status[diff$record_id == "3"]), "Removed")
  expect_equal(as.character(diff$Status[diff$record_id == "2"]), "Modified")
  expect_equal(as.character(diff$Status[diff$record_id == "1"]), "Unchanged")

  expect_equal(diff$`Changed fields`[diff$record_id == "2"], "age")
  expect_equal(diff$`Changed fields`[diff$record_id == "1"], "")
})

test_that("rd_diff treats NA as equal to NA", {
  old_na <- data.frame(record_id = 1, note = NA_character_, stringsAsFactors = FALSE)
  new_na <- data.frame(record_id = 1, note = NA_character_, stringsAsFactors = FALSE)

  res <- rd_diff(old_na, new_na)
  expect_equal(as.character(res$diff$Status), "Unchanged")
})

test_that("rd_diff returns NULL results when return_viewer = FALSE", {
  res <- rd_diff(old_data, new_data, return_viewer = FALSE)
  expect_null(res$results)
})

test_that("rd_diff warns when there are no comparable columns", {
  old_min <- data.frame(record_id = c(1, 2), stringsAsFactors = FALSE)
  new_min <- data.frame(record_id = c(2, 3), stringsAsFactors = FALSE)

  expect_warning(res <- rd_diff(old_min, new_min), "share no columns")
  expect_equal(as.character(res$diff$Status[res$diff$record_id == "2"]), "Unchanged")
})

test_that("rd_diff supports multi-column id_vars (e.g. record_id + event)", {
  old_long <- data.frame(
    record_id = c(1, 1, 2),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1", "baseline_arm_1"),
    sbp = c(120, 125, 130),
    stringsAsFactors = FALSE
  )
  new_long <- data.frame(
    record_id = c(1, 1, 2),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1", "baseline_arm_1"),
    sbp = c(120, 128, 130),
    stringsAsFactors = FALSE
  )

  res <- rd_diff(old_long, new_long, id_vars = c("record_id", "redcap_event_name"))
  diff <- res$diff

  expect_equal(
    as.character(diff$Status[diff$record_id == "1" & diff$redcap_event_name == "followup_arm_1"]),
    "Modified"
  )
  expect_equal(
    as.character(diff$Status[diff$record_id == "1" & diff$redcap_event_name == "baseline_arm_1"]),
    "Unchanged"
  )
})
