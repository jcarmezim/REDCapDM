#' Compare Two Snapshots of a REDCap Dataset
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Compares an older extraction of a REDCap dataset (`old`) with a newer one (`new`), matching rows by `id_vars` and classifying each matched key into one of four statuses:
#' \itemize{
#'   \item \strong{Added} — the key is present in `new` but not in `old`,
#'   \item \strong{Removed} — the key is present in `old` but not in `new`,
#'   \item \strong{Modified} — the key exists in both, but at least one shared column differs between the two,
#'   \item \strong{Unchanged} — the key exists in both and every shared column is identical.
#' }
#'
#' Useful for monitoring an active study between two pulls of the same project (e.g. two weekly API extracts), complementing [check_dictionary()], which compares the dictionary/metadata rather than the data itself.
#'
#' @param old Data frame containing the older dataset (e.g. the `data` element of a previous [redcap_data()] call).
#' @param new Data frame containing the newer dataset.
#' @param id_vars Character vector of column name(s) present in both `old` and `new` that together uniquely identify a row. Defaults to `"record_id"`. For a longitudinal project, pass `c("record_id", "redcap_event_name")` (and `"redcap_repeat_instance"` if the comparison involves a repeating instrument) so that rows are matched by record *and* event/instance rather than by record alone.
#' @param report_title Optional single string used as the caption for the HTML summary table. Defaults to `"Dataset comparison report"`.
#' @param return_viewer Logical; if `TRUE` (default) an HTML table (knitr/kable + kableExtra) summarizing the counts per status is produced and returned in the `results` element of the returned list. If `FALSE`, no HTML viewer is produced (useful for non-interactive runs).
#'
#' @details
#' Only columns present in **both** `old` and `new` (besides `id_vars`) are compared; columns that exist in only one of the two are ignored for the purpose of detecting modifications, since they can't be compared. `NA` values are treated as equal to other `NA` values. Values are compared via `as.character()`, so comparing a column that changed class between the two snapshots (e.g. numeric in `old`, factor in `new`) may report spurious differences — this function is meant for a quick "what changed" overview, not a type-aware reconciliation.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`diff`}{A data frame with one row per key found in either `old` or `new`, with columns for each of `id_vars`, `Status`, and `Changed fields` (a comma-separated list of the columns that differ, only populated for `Modified` rows).}
#'   \item{`results`}{If `return_viewer = TRUE`, an HTML `knitr::kable` (styled with `kableExtra`) summarizing the total rows per status. If `return_viewer = FALSE`, this is `NULL`.}
#' }
#'
#' @examples
#' old <- data.frame(
#'   record_id = c(1, 2, 3),
#'   age = c(45, 50, 60),
#'   sex = c("Male", "Female", "Male"),
#'   stringsAsFactors = FALSE
#' )
#'
#' new <- data.frame(
#'   record_id = c(1, 2, 4),
#'   age = c(45, 51, 70),
#'   sex = c("Male", "Female", "Female"),
#'   stringsAsFactors = FALSE
#' )
#'
#' res <- rd_diff(old, new)
#' res$diff
#'
#' @export
#' @importFrom rlang .data
rd_diff <- function(old, new, id_vars = "record_id", report_title = NULL, return_viewer = TRUE) {

  if (!is.data.frame(old) || !is.data.frame(new)) {
    stop("The 'old' and 'new' arguments must be a data frame.", call. = FALSE)
  }

  missing_old <- setdiff(id_vars, names(old))
  missing_new <- setdiff(id_vars, names(new))
  if (length(missing_old) > 0 || length(missing_new) > 0) {
    stop(sprintf("Both 'old' and 'new' must contain the identifier column(s): %s.", paste(id_vars, collapse = ", ")), call. = FALSE)
  }

  if (!is.null(report_title) && length(report_title) > 1) {
    stop("There is more than one title for the report, please choose only one.", call. = FALSE)
  }

  # Build a single string key per row from `id_vars`, using a control
  # character as separator so it can't collide with real field values.
  key_of <- function(df) {
    do.call(paste, c(as.list(df[id_vars]), sep = "\u0001"))
  }

  old_key <- key_of(old)
  new_key <- key_of(new)

  id_label <- paste(id_vars, collapse = ", ")

  if (anyDuplicated(old_key) > 0) {
    stop(sprintf("Duplicated combinations of %s found in 'old'. Each row must be uniquely identified by `id_vars`.", id_label), call. = FALSE)
  }
  if (anyDuplicated(new_key) > 0) {
    stop(sprintf("Duplicated combinations of %s found in 'new'. Each row must be uniquely identified by `id_vars`.", id_label), call. = FALSE)
  }

  # Columns that can actually be compared between both snapshots
  compare_cols <- setdiff(intersect(names(old), names(new)), id_vars)

  if (length(compare_cols) == 0) {
    warning("'old' and 'new' share no columns besides `id_vars`; only additions and removals can be detected.", call. = FALSE)
  }

  all_keys <- union(old_key, new_key)
  in_old <- all_keys %in% old_key
  in_new <- all_keys %in% new_key

  # Identifier columns for the output: prefer `new`'s values whenever the key
  # exists there (Added/Modified/Unchanged), falling back to `old` only for
  # keys that exist exclusively in `old` (Removed).
  id_df <- stats::setNames(
    lapply(id_vars, function(col) {
      vals <- rep(NA_character_, length(all_keys))
      vals[in_new] <- as.character(new[match(all_keys[in_new], new_key), col])
      vals[!in_new] <- as.character(old[match(all_keys[!in_new], old_key), col])
      vals
    }),
    id_vars
  )

  comparison <- as.data.frame(id_df, stringsAsFactors = FALSE)
  comparison$Status <- dplyr::case_when(
    !in_old & in_new ~ "Added",
    in_old & !in_new ~ "Removed",
    TRUE ~ NA_character_
  )
  comparison$`Changed fields` <- ""

  common_keys <- all_keys[in_old & in_new]

  if (length(common_keys) > 0 && length(compare_cols) > 0) {
    old_common <- old[match(common_keys, old_key), compare_cols, drop = FALSE]
    new_common <- new[match(common_keys, new_key), compare_cols, drop = FALSE]

    # Looping over columns (typically tens, not hundreds) rather than rows
    # (potentially many thousands) keeps this comparison fast on large datasets.
    diffs <- matrix(FALSE, nrow = length(common_keys), ncol = length(compare_cols), dimnames = list(NULL, compare_cols))
    for (col in compare_cols) {
      a <- old_common[[col]]
      b <- new_common[[col]]
      a <- ifelse(is.na(a), "\u0001NA\u0001", as.character(a))
      b <- ifelse(is.na(b), "\u0001NA\u0001", as.character(b))
      diffs[, col] <- a != b
    }

    any_diff <- rowSums(diffs) > 0
    idx <- match(common_keys, all_keys)

    comparison$Status[idx] <- ifelse(any_diff, "Modified", "Unchanged")

    if (any(any_diff)) {
      comparison$`Changed fields`[idx[any_diff]] <- apply(
        diffs[any_diff, , drop = FALSE], 1,
        function(r) paste(compare_cols[r], collapse = ", ")
      )
    }
  } else if (length(common_keys) > 0) {
    # No comparable columns: rows present in both are treated as unchanged
    comparison$Status[in_old & in_new] <- "Unchanged"
  }

  comparison <- comparison |>
    dplyr::mutate(Status = factor(.data$Status, levels = c("Added", "Removed", "Modified", "Unchanged"))) |>
    dplyr::arrange(.data$Status, .data[[id_vars[1]]]) |>
    as.data.frame()

  rownames(comparison) <- NULL

  if (is.null(report_title)) {
    report_title <- "Dataset comparison report"
  }

  # Summarize statuses
  report <- comparison |>
    dplyr::group_by(.data$Status, .drop = FALSE) |>
    dplyr::summarise(Total = dplyr::n(), .groups = "drop") |>
    as.data.frame()

  # Generate styled HTML summary
  viewer <- NULL
  if (isTRUE(return_viewer)) {
    viewer <- build_html_table(report, align = c("cc"), caption = report_title)
  }

  list(
    diff = comparison,
    results = viewer
  )
}
