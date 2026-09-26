old_dic <- data.frame(
  field_name = c("age", "sex", "weight", "notes"),
  field_label = c("Age", "Sex", "Weight", "Notes"),
  field_type = c("text", "radio", "text", "notes"),
  choices_calculations_or_slider_labels = c(NA, "1, Male | 2, Female", NA, NA),
  stringsAsFactors = FALSE
)

new_dic <- data.frame(
  field_name = c("age", "sex", "height", "notes"),
  field_label = c("Age in years", "Sex", "Height", "Notes"),
  field_type = c("text", "radio", "text", "notes"),
  choices_calculations_or_slider_labels = c(NA, "1, Male | 2, Female", NA, NA),
  stringsAsFactors = FALSE
)

test_that("check_dictionary errors when old/new are not data frames", {
  expect_error(check_dictionary(old = list(), new = new_dic), "must be a data frame")
  expect_error(check_dictionary(old = old_dic, new = "x"), "must be a data frame")
})

test_that("check_dictionary errors when field_name is missing", {
  expect_error(check_dictionary(old = data.frame(x = 1), new = new_dic), "must contain a 'field_name' column")
})

test_that("check_dictionary errors with multiple report titles", {
  expect_error(check_dictionary(old_dic, new_dic, report_title = c("a", "b")), "only one")
})

test_that("check_dictionary errors with duplicated field_name", {
  dup <- rbind(old_dic, old_dic[1, ])
  expect_error(check_dictionary(dup, new_dic), "Duplicated 'field_name'")
})

test_that("check_dictionary correctly classifies added, removed, modified and unchanged fields", {
  res <- check_dictionary(old_dic, new_dic)

  expect_type(res, "list")
  expect_named(res, c("dictionary", "results"))

  dict <- res$dictionary

  expect_equal(dict$Status[dict$Field == "height"], factor("Added", levels = levels(dict$Status)))
  expect_equal(dict$Status[dict$Field == "weight"], factor("Removed", levels = levels(dict$Status)))
  expect_equal(dict$Status[dict$Field == "age"], factor("Modified", levels = levels(dict$Status)))
  expect_equal(dict$Status[dict$Field == "notes"], factor("Unchanged", levels = levels(dict$Status)))

  expect_equal(dict$`Changed fields`[dict$Field == "age"], "field_label")
  expect_equal(dict$`Changed fields`[dict$Field == "notes"], "")
})

test_that("check_dictionary keeps the newer field label for modified/added fields and the older one for removed fields", {
  res <- check_dictionary(old_dic, new_dic)
  dict <- res$dictionary

  expect_equal(dict$`Field label`[dict$Field == "age"], "Age in years")
  expect_equal(dict$`Field label`[dict$Field == "height"], "Height")
  expect_equal(dict$`Field label`[dict$Field == "weight"], "Weight")
})

test_that("check_dictionary returns NULL results when return_viewer = FALSE", {
  res <- check_dictionary(old_dic, new_dic, return_viewer = FALSE)
  expect_null(res$results)
})

test_that("check_dictionary warns when there are no comparable columns", {
  old_min <- data.frame(field_name = c("a", "b"), stringsAsFactors = FALSE)
  new_min <- data.frame(field_name = c("b", "c"), stringsAsFactors = FALSE)

  expect_warning(res <- check_dictionary(old_min, new_min), "share no columns")
  expect_equal(as.character(res$dictionary$Status[res$dictionary$Field == "b"]), "Unchanged")
})
