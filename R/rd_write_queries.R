#' Write Query Annotations Back to REDCap
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Pushes a text annotation (typically the query description itself, or a fixed note such as `"Please review"`) back into REDCap for every record (and event, for longitudinal projects) identified in a query report produced by [rd_query()], [rd_event()], or [check_queries()]. This closes the loop between identifying data queries in R and communicating them to the data-entry team directly inside REDCap, via [REDCapR::redcap_write()].
#'
#' @param project Optional list containing `data` (used only to resolve event labels back to their raw REDCap event names; see Details). Ignored if `data` is provided directly.
#' @param data Optional `data.frame`/`tibble` of the REDCap dataset (as returned by [redcap_data()]), used only to resolve event labels back to their raw REDCap event names for longitudinal projects. Not required if `queries$Event` already contains raw event names, or the project is not longitudinal.
#' @param queries A data frame of queries (must contain an `Identifier` column, and an `Event` column for longitudinal projects), or a list with a `queries` element (e.g. the direct output of [rd_query()], [rd_event()], or [check_queries()]).
#' @param uri REDCap API base URI.
#' @param token REDCap API token. **This token must have data-import permission on the target project.**
#' @param field Name of the (already existing) REDCap field to write the annotation into, e.g. a custom "Data management note" text field added to the project for this purpose.
#' @param value Optional. Either `NULL` (default, uses each row's `Query` column as the written value), a single string (written identically to every matched row), or the name of a column in `queries` to use instead.
#' @param dry_run Logical. If `TRUE` (the default), no data is written to REDCap: the function only returns the data frame that *would* be written, so it can be reviewed first. Set to `FALSE` to actually perform the write.
#'
#' @details
#' **This function can modify data in a live REDCap project and, depending on how the target field is used downstream, may be hard to reverse. Always inspect the `data` element of the result with `dry_run = TRUE` (the default) before re-running with `dry_run = FALSE`.**
#'
#' If `queries$Event` contains event *labels* (the `.factor` version, as produced by [rd_query()] when the dataset has a `redcap_event_name.factor` column) rather than the raw `unique_event_name` REDCap expects for the API, pass the original `data` (or `project`) so the labels can be mapped back to their raw event names. If `queries$Event` already contains raw event names, `data`/`project` can be omitted.
#'
#' If more than one row of `queries` maps to the same record (and event), the function stops with an error rather than silently keeping only one of the conflicting values — combine the relevant rows (e.g. by pasting the `Query` texts together) before calling this function.
#'
#' Repeating instruments are not currently supported: rows whose `Repetition` column (when present) is not `"-"` will cause an error.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The data frame that was (or, in dry-run mode, would be) sent to REDCap, with columns `record_id`, `redcap_event_name` (if applicable), and the target `field`.}
#'   \item{dry_run}{Logical, whether the write was actually skipped.}
#'   \item{result}{The response object from [REDCapR::redcap_write()], or `NULL` if `dry_run = TRUE`.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- rd_query(covican, variables = "age", expression = "is.na(x)", event = "baseline_visit_arm_1")
#'
#' # Review what would be written first (default dry_run = TRUE)
#' preview <- rd_write_queries(
#'   queries = res,
#'   uri = "https://redcap.example.org/api/",
#'   token = "REPLACE_WITH_TOKEN",
#'   field = "dm_query_note"
#' )
#' preview$data
#'
#' # Once reviewed, actually write it
#' final <- rd_write_queries(
#'   queries = res,
#'   uri = "https://redcap.example.org/api/",
#'   token = "REPLACE_WITH_TOKEN",
#'   field = "dm_query_note",
#'   dry_run = FALSE
#' )
#' }
#'
#' @export

rd_write_queries <- function(project = NULL, data = NULL, queries, uri, token, field, value = NULL, dry_run = TRUE) {

  if (!is.null(project) && is.null(data)) {
    data <- project$data
  }

  if (missing(uri) || missing(token) || is.null(uri) || is.null(token)) {
    stop("Both `uri` and `token` are required to write to REDCap.", call. = FALSE)
  }

  if (missing(field) || is.null(field) || length(field) != 1 || !is.character(field)) {
    stop("`field` must be a single REDCap field name to write into.", call. = FALSE)
  }

  queries <- normalize_queries(queries)

  longitudinal <- "Event" %in% names(queries) && any(!is.na(queries$Event) & queries$Event != "-")

  if ("Repetition" %in% names(queries) && any(!queries$Repetition %in% c("-", NA))) {
    stop("`queries` contains rows belonging to a repeating instrument, which `rd_write_queries()` does not currently support.", call. = FALSE)
  }

  # Resolve the value to write
  if (is.null(value)) {
    if (!"Query" %in% names(queries)) {
      stop("`queries` has no `Query` column to use as the default `value`. Please specify the `value` argument explicitly.", call. = FALSE)
    }
    write_values <- as.character(queries$Query)
  } else if (length(value) == 1 && value %in% names(queries)) {
    write_values <- as.character(queries[[value]])
  } else if (length(value) == 1) {
    write_values <- rep(as.character(value), nrow(queries))
  } else {
    stop("`value` must be `NULL`, a single string, or the name of a column in `queries`.", call. = FALSE)
  }

  import_df <- data.frame(
    record_id = as.character(queries$Identifier),
    stringsAsFactors = FALSE
  )

  if (longitudinal) {
    event_raw <- as.character(queries$Event)

    # If the events look like display labels rather than raw REDCap event names, try to resolve them using `data`
    if (!is.null(data) && "redcap_event_name" %in% names(data) && "redcap_event_name.factor" %in% names(data)) {
      needs_mapping <- !all(event_raw %in% unique(as.character(data$redcap_event_name)))

      if (needs_mapping) {
        lookup <- stats::setNames(
          as.character(data$redcap_event_name),
          as.character(data$redcap_event_name.factor)
        )
        lookup <- lookup[!duplicated(names(lookup))]

        unresolved <- setdiff(unique(event_raw), names(lookup))
        if (length(unresolved) > 0) {
          stop(sprintf("Could not resolve the following event(s) to a raw REDCap event name: %s. Please check the `Event` column of `queries` or provide `data`/`project` for the correct project.", paste(unresolved, collapse = ", ")), call. = FALSE)
        }

        event_raw <- unname(lookup[event_raw])
      }
    }

    import_df$redcap_event_name <- event_raw
  }

  import_df[[field]] <- write_values

  # Detect conflicting rows: same record (and event) mapped to different values
  key_cols <- intersect(c("record_id", "redcap_event_name"), names(import_df))
  dup_key <- import_df[key_cols]
  is_dup <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)

  if (any(is_dup)) {
    conflicting <- unique(dup_key[is_dup, , drop = FALSE])
    stop(
      sprintf(
        "Found %d record(s)%s mapped to more than one value for `%s`. Combine the relevant rows in `queries` (e.g. paste the `Query` texts together) before calling `rd_write_queries()`.",
        nrow(conflicting),
        if ("redcap_event_name" %in% names(import_df)) "/event" else "",
        field
      ),
      call. = FALSE
    )
  }

  result <- NULL

  if (isTRUE(dry_run)) {
    message(sprintf("Dry run: %d row(s) would be written to the `%s` field. Set `dry_run = FALSE` to perform the write.", nrow(import_df), field))
  } else {
    result <- tryCatch(
      REDCapR::redcap_write(ds = import_df, redcap_uri = uri, token = token),
      error = function(e) {
        stop(sprintf("REDCap write failed: %s", conditionMessage(e)), call. = FALSE)
      }
    )

    message(sprintf("Wrote %d row(s) to the `%s` field in REDCap.", nrow(import_df), field))
  }

  list(
    data = import_df,
    dry_run = isTRUE(dry_run),
    result = result
  )
}
