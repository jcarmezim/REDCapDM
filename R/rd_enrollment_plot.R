#' Plot Enrollment Over Time
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Computes and (optionally) plots the number of records enrolled per time period, cumulative by default, based on a per-record enrollment (or screening/randomization) date. Useful for tracking recruitment progress in an active study, optionally with one line/set of bars per site (Data Access Group) or treatment arm.
#'
#' @param project A list containing the REDCap data (expected `redcap_data()` output). Overrides `data`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset. Must contain a `record_id` column.
#' @param date_var Single character string: the name of the enrollment date column in `data`. Must already be of class `Date` or `POSIXct` (see [rd_dates()]).
#' @param by Optional single variable name (present in `data`) used to plot one line/set of bars per level, e.g. a Data Access Group or treatment arm.
#' @param interval One of `"day"`, `"week"`, `"month"` (default), or `"year"`: the time bucket used to group enrollment counts.
#' @param cumulative Logical. If `TRUE` (default), plots (and reports) the running cumulative total. If `FALSE`, plots the count of new records enrolled in each period.
#' @param plot Logical. If `TRUE` (default), a `ggplot2` plot is returned in the `plot` element. Requires the `ggplot2` package. If `FALSE`, only the numeric summary is returned.
#'
#' @details
#' If a record has more than one row (e.g. a longitudinal project with one row per event), the first non-missing value of `date_var` (and of `by`, if specified) across that record's rows is used — i.e. `date_var` and `by` are expected to be essentially constant within a record, as is typical for an enrollment date and a Data Access Group/arm. Records with a missing `date_var` (after collapsing to one row per record) are excluded, with a warning; the same applies to a missing `by` value when `by` is specified.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{A data frame with columns `Period`, `By` (`NA` if `by` is not specified), `N` (records newly enrolled in that period), and `Cumulative_N`.}
#'   \item{plot}{A `ggplot2` object, or `NULL` if `plot = FALSE` or `ggplot2` is not installed.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- rd_enrollment_plot(covican, date_var = "screening_date")
#' res$summary
#' res$plot
#'
#' # One line per Data Access Group
#' res_by_dag <- rd_enrollment_plot(
#'   covican,
#'   date_var = "screening_date",
#'   by = "redcap_data_access_group.factor"
#' )
#' res_by_dag$plot
#' }
#'
#' @export
#' @importFrom rlang .data
rd_enrollment_plot <- function(project = NULL, data = NULL, date_var, by = NULL, interval = "month", cumulative = TRUE, plot = TRUE) {

  interval <- match.arg(interval, c("day", "week", "month", "year"))

  if (!is.null(project) && is.null(data)) {
    data <- project$data
  }

  if (is.null(data)) {
    stop("`data` (or `project`) must be provided.", call. = FALSE)
  }

  data <- as.data.frame(data)

  if (!"record_id" %in% names(data)) {
    stop("`data` must contain a `record_id` column.", call. = FALSE)
  }

  if (missing(date_var) || length(date_var) != 1 || !is.character(date_var)) {
    stop("`date_var` must be a single column name.", call. = FALSE)
  }
  if (!date_var %in% names(data)) {
    stop(sprintf("The `date_var` column '%s' was not found in the dataset.", date_var), call. = FALSE)
  }
  if (!inherits(data[[date_var]], "Date") && !inherits(data[[date_var]], "POSIXct")) {
    stop(sprintf("The `date_var` column '%s' must be of class `Date` or `POSIXct`. Use `rd_dates()` to format it first.", date_var), call. = FALSE)
  }

  if (!is.null(by) && !by %in% names(data)) {
    stop(sprintf("The `by` variable '%s' was not found in the dataset.", by), call. = FALSE)
  }

  # First non-missing value of `x`, preserving its class (so a group with no
  # non-missing values still returns a correctly-typed NA, keeping every
  # group's result the same type for dplyr::summarise()).
  first_non_na <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) x[NA_integer_] else x[idx[1]]
  }

  # Collapse to one row per record: a longitudinal dataset has one row per
  # event, but the enrollment date (and `by`, typically a DAG/arm) is
  # expected to be essentially constant within a record.
  record_df <- data |>
    dplyr::group_by(.data$record_id) |>
    dplyr::summarise(
      .date = first_non_na(.data[[date_var]]),
      .by_group = if (!is.null(by)) as.character(first_non_na(.data[[by]])) else NA_character_,
      .groups = "drop"
    )

  n_missing_date <- sum(is.na(record_df$.date))
  if (n_missing_date > 0) {
    warning(sprintf("%d record(s) with a missing `%s` value were excluded.", n_missing_date, date_var), call. = FALSE)
  }
  record_df <- record_df[!is.na(record_df$.date), , drop = FALSE]

  if (nrow(record_df) == 0) {
    stop("No records with a non-missing enrollment date were found.", call. = FALSE)
  }

  if (!is.null(by)) {
    n_missing_by <- sum(is.na(record_df$.by_group))
    if (n_missing_by > 0) {
      warning(sprintf("%d record(s) with a missing `%s` value were excluded from the stratified summary.", n_missing_by, by), call. = FALSE)
    }
    record_df <- record_df[!is.na(record_df$.by_group), , drop = FALSE]
  }

  record_df$.period <- as.Date(cut(record_df$.date, breaks = interval))

  if (!is.null(by)) {
    summary_df <- record_df |>
      dplyr::count(.data$.period, .data$.by_group, name = "N") |>
      dplyr::arrange(.data$.by_group, .data$.period) |>
      dplyr::group_by(.data$.by_group) |>
      dplyr::mutate(Cumulative_N = cumsum(.data$N)) |>
      dplyr::ungroup() |>
      dplyr::transmute(Period = .data$.period, By = .data$.by_group, N = .data$N, Cumulative_N = .data$Cumulative_N)
  } else {
    summary_df <- record_df |>
      dplyr::count(.data$.period, name = "N") |>
      dplyr::arrange(.data$.period) |>
      dplyr::mutate(Cumulative_N = cumsum(.data$N)) |>
      dplyr::transmute(Period = .data$.period, By = NA_character_, N = .data$N, Cumulative_N = .data$Cumulative_N)
  }

  summary_df <- as.data.frame(summary_df)

  # Build the plot
  gg <- NULL

  if (isTRUE(plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("The `ggplot2` package is required to build the plot. Install it with `install.packages('ggplot2')`. Returning the numeric summary only.", call. = FALSE)
    } else {
      y_var <- if (isTRUE(cumulative)) "Cumulative_N" else "N"
      y_label <- if (isTRUE(cumulative)) "Cumulative records enrolled" else "Records enrolled"
      title <- if (isTRUE(cumulative)) "Cumulative enrollment over time" else "Enrollment over time"

      if (!is.null(by)) {
        gg <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$Period, y = .data[[y_var]], color = .data$By, group = .data$By))

        gg <- if (isTRUE(cumulative)) {
          gg + ggplot2::geom_step(linewidth = 0.8) + ggplot2::geom_point(size = 1.5)
        } else {
          gg + ggplot2::geom_col(ggplot2::aes(fill = .data$By), position = "dodge", width = 0.7, color = NA)
        }

        gg <- gg + ggplot2::labs(color = by, fill = by)
      } else {
        gg <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$Period, y = .data[[y_var]]))

        gg <- if (isTRUE(cumulative)) {
          gg + ggplot2::geom_step(linewidth = 0.8, color = "#256abf") + ggplot2::geom_point(size = 1.5, color = "#256abf")
        } else {
          gg + ggplot2::geom_col(fill = "#256abf", width = 0.7)
        }
      }

      gg <- gg +
        ggplot2::labs(title = title, x = NULL, y = y_label) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = "#52514e"),
          plot.title = ggplot2::element_text(color = "#0b0b0b")
        )
    }
  }

  list(
    summary = summary_df,
    plot = gg
  )
}
