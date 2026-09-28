#' Compare Two Versions of a REDCap Dictionary
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Compares an older REDCap data dictionary (`old`) with a newer one (`new`) and classifies each field into one of four statuses:
#' \itemize{
#'   \item \strong{Added} — the field is present in `new` but not in `old`,
#'   \item \strong{Removed} — the field is present in `old` but not in `new`,
#'   \item \strong{Modified} — the field exists in both, but at least one shared column (e.g. `field_label`, `field_type`, `choices_calculations_or_slider_labels`, `branching_logic_show_field_only_if`) differs between the two,
#'   \item \strong{Unchanged} — the field exists in both and every shared column is identical.
#' }
#'
#' This is useful for auditing how a REDCap project's data dictionary has evolved between two exports (e.g. between two rounds of data collection, or before re-running analysis code written against an older version of the project), similarly to how [check_queries()] tracks changes between two query reports.
#'
#' @param old Data frame containing the older dictionary (as returned in the `dictionary` element of [redcap_data()]). Must include a `field_name` column.
#' @param new Data frame containing the newer dictionary. Must include a `field_name` column.
#' @param report_title Optional single string used as the caption for the HTML summary table. Defaults to `"Dictionary comparison report"`.
#' @param return_viewer Logical; if `TRUE` (default) an HTML table (knitr/kable + kableExtra) summarizing the counts per status is produced and returned in the `results` element of the returned list. If `FALSE`, no HTML viewer is produced (useful for non-interactive runs).
#'
#' @details
#' Only columns present in **both** `old` and `new` are compared (besides `field_name`, which is used as the matching key). Columns that exist in only one of the two dictionaries are ignored for the purpose of detecting modifications, since they can't be compared. `NA` values are treated as equal to other `NA` values.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`dictionary`}{A data frame with one row per field found in either `old` or `new`, with columns `Field`, `Field label` (from whichever version has it, preferring `new`), `Status`, and `Changed fields` (a comma-separated list of the columns that differ, only populated for `Modified` fields).}
#'   \item{`results`}{If `return_viewer = TRUE`, an HTML `knitr::kable` (styled with `kableExtra`) summarizing the total fields per status. If `return_viewer = FALSE`, this is `NULL`.}
#' }
#'
#' @examples
#' old <- data.frame(
#'   field_name = c("age", "sex", "weight"),
#'   field_label = c("Age", "Sex", "Weight"),
#'   field_type = c("text", "radio", "text"),
#'   stringsAsFactors = FALSE
#' )
#'
#' new <- data.frame(
#'   field_name = c("age", "sex", "height"),
#'   field_label = c("Age in years", "Sex", "Height"),
#'   field_type = c("text", "radio", "text"),
#'   stringsAsFactors = FALSE
#' )
#'
#' res <- check_dictionary(old, new)
#' res$dictionary
#'
#' @export
#' @importFrom rlang .data

check_dictionary <- function(old, new, report_title = NULL, return_viewer = TRUE) {
  # Ensure both objects provided are data frames
  if (!is.data.frame(old) | !is.data.frame(new)) {
    stop("The 'old' and 'new' arguments must be a data frame.", call. = FALSE)
  }

  if (!"field_name" %in% names(old) | !"field_name" %in% names(new)) {
    stop("Both 'old' and 'new' must contain a 'field_name' column.", call. = FALSE)
  }

  if (!is.null(report_title) && length(report_title) > 1) {
    stop("There is more than one title for the report, please choose only one.", call. = FALSE)
  }

  if (anyDuplicated(old$field_name) > 0) {
    stop("Duplicated 'field_name' values found in 'old'. Each field must appear only once.", call. = FALSE)
  }

  if (anyDuplicated(new$field_name) > 0) {
    stop("Duplicated 'field_name' values found in 'new'. Each field must appear only once.", call. = FALSE)
  }

  # Columns that can actually be compared between both dictionaries
  compare_cols <- setdiff(intersect(names(old), names(new)), "field_name")

  if (length(compare_cols) == 0) {
    warning("'old' and 'new' share no columns besides 'field_name'; only additions and removals can be detected.", call. = FALSE)
  }

  fields <- union(old$field_name, new$field_name)

  comparison <- tibble::tibble(field_name = fields) |>
    dplyr::mutate(
      in_old = .data$field_name %in% old$field_name,
      in_new = .data$field_name %in% new$field_name,
      status = dplyr::case_when(
        !.data$in_old & .data$in_new ~ "Added",
        .data$in_old & !.data$in_new ~ "Removed",
        TRUE ~ NA_character_
      ),
      changed_fields = ""
    )

  common_fields <- comparison$field_name[comparison$in_old & comparison$in_new]

  if (length(common_fields) > 0 & length(compare_cols) > 0) {
    old_common <- old[match(common_fields, old$field_name), compare_cols, drop = FALSE]
    new_common <- new[match(common_fields, new$field_name), compare_cols, drop = FALSE]

    for (i in seq_along(common_fields)) {
      diffs <- purrr::map_lgl(compare_cols, function(col) {
        a <- old_common[[col]][i]
        b <- new_common[[col]][i]
        a <- ifelse(is.na(a), "", as.character(a))
        b <- ifelse(is.na(b), "", as.character(b))
        !identical(a, b)
      })

      idx <- comparison$field_name == common_fields[i]

      if (any(diffs)) {
        comparison$status[idx] <- "Modified"
        comparison$changed_fields[idx] <- paste(compare_cols[diffs], collapse = ", ")
      } else {
        comparison$status[idx] <- "Unchanged"
      }
    }
  } else if (length(common_fields) > 0) {
    # No comparable columns: fields present in both are treated as unchanged
    comparison$status[comparison$field_name %in% common_fields] <- "Unchanged"
  }

  # Attach a readable field label, preferring the newer dictionary's version
  if ("field_label" %in% names(old) | "field_label" %in% names(new)) {
    old_label <- old$field_label[match(comparison$field_name, old$field_name)]
    new_label <- if ("field_label" %in% names(new)) new$field_label[match(comparison$field_name, new$field_name)] else rep(NA_character_, nrow(comparison))
    old_label <- if ("field_label" %in% names(old)) old_label else rep(NA_character_, nrow(comparison))

    comparison$field_label <- trimws(gsub("<.*?>", "", dplyr::coalesce(new_label, old_label)))
  }

  comparison <- comparison |>
    dplyr::mutate(status = factor(.data$status, levels = c("Added", "Removed", "Modified", "Unchanged"))) |>
    dplyr::select("field_name", dplyr::any_of("field_label"), "status", "changed_fields") |>
    dplyr::rename(
      "Field" = "field_name",
      "Status" = "status",
      "Changed fields" = "changed_fields"
    )

  if ("field_label" %in% names(comparison)) {
    comparison <- comparison |>
      dplyr::rename("Field label" = "field_label")
  }

  comparison <- comparison |>
    dplyr::arrange(.data$Status, .data$Field) |>
    as.data.frame()

  # Handle report title
  if (is.null(report_title)) {
    report_title <- "Dictionary comparison report"
  }

  # Summarize statuses
  report <- comparison |>
    dplyr::group_by(.data$Status, .drop = FALSE) |>
    dplyr::summarise(Total = dplyr::n(), .groups = "drop") |>
    as.data.frame()
  names(report) <- c("Status", "Total")

  # Generate styled HTML summary
  viewer <- NULL
  if (isTRUE(return_viewer)) {
    viewer <- build_html_table(report, align = c("cc"), caption = report_title)
  }

  # Return results
  list(
    dictionary = comparison,
    results = viewer
  )
}
