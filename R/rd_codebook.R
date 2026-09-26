#' Generate a Descriptive Summary Table (Codebook / "Table 1")
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Builds a descriptive summary table ("Table 1") for a set of variables in a (typically already transformed, see [rd_transform()]) REDCap dataset, reusing the variable labels already stored in the dictionary/data. Numeric variables are summarized with mean (SD) and/or median [Q1, Q3], date variables with their range, and factor/character variables with counts and percentages per level. The table can optionally be stratified by a grouping variable (e.g. treatment arm, event, or Data Access Group), with an additional "Overall" column.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param variables Optional character vector of variable names to summarize. Defaults to every field in `dic` that is present as a column in `data`.
#' @param by Optional single variable name (present in `data`) used to stratify the table into columns. An additional `"Overall"` column is always included.
#' @param numeric_summary One of `"mean_sd"` (default), `"median_iqr"`, or `"both"`, controlling how numeric variables are summarized.
#' @param digits Number of decimal digits used when rounding numeric summaries and percentages. Default `1`.
#' @param report_title Optional single string used as the caption for the HTML summary table. Defaults to `"Descriptive summary"`.
#' @param return_viewer Logical; if `TRUE` (default) an HTML table (knitr/kable + kableExtra) is produced and returned in the `results` element of the returned list. If `FALSE`, no HTML viewer is produced (useful for non-interactive runs).
#'
#' @details
#' This function is intentionally lightweight (it only relies on packages already used elsewhere in **REDCapDM**) rather than depending on a dedicated summary-table package. For more advanced tabulation needs (e.g. p-values, custom statistical tests), consider passing the transformed data/dictionary labels into a package such as `gtsummary`.
#'
#' Every variable is expected to already carry the class appropriate to how it should be summarized (numeric, `Date`/`POSIXct`, or factor/character) — i.e. this function is meant to be run **after** [rd_transform()] (or at least [rd_factor()] and [rd_dates()]) so that categorical fields are proper factors rather than raw numeric codes.
#'
#' Unlike [rd_query()]/[rd_event()], this function does **not** filter rows by event: for a longitudinal project, running it directly on the full dataset will overestimate missingness for variables that are only collected in some events (rows from every other event count as missing). Subset `data` to the event(s) of interest first (e.g. with `rd_split(by = "event")` or a manual `dplyr::filter()`) before calling `rd_codebook()`.
#'
#' @return A list with:
#' \describe{
#'   \item{table}{A data frame with columns `Variable`, `Label`, `Level`, `Overall`, and (if `by` is specified) one additional column per level of `by`.}
#'   \item{results}{If `return_viewer = TRUE`, an HTML `knitr::kable` (styled with `kableExtra`). If `return_viewer = FALSE`, this is `NULL`.}
#' }
#'
#' @examples
#' \dontrun{
#' trans <- rd_transform(covican)
#'
#' res <- rd_codebook(
#'   data = trans$data,
#'   dic = trans$dictionary,
#'   variables = c("age", "copd", "dm"),
#'   by = "redcap_data_access_group.factor"
#' )
#' res$table
#' }
#'
#' @export
#' @importFrom rlang .data

rd_codebook <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, variables = NULL, by = NULL, numeric_summary = "mean_sd", digits = 1, report_title = NULL, return_viewer = TRUE) {

  numeric_summary <- match.arg(numeric_summary, c("mean_sd", "median_iqr", "both"))

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
  }

  data <- as.data.frame(data)
  dic <- as.data.frame(dic)

  # Resolve variables to summarize
  if (is.null(variables)) {
    variables <- intersect(dic$field_name, names(data))
    variables <- setdiff(variables, "record_id")

    if (length(variables) == 0) {
      stop("No variables from the dictionary were found as columns in the dataset. Please specify the `variables` argument manually.", call. = FALSE)
    }
  } else {
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
      stop(sprintf("The following variables were not found in the dataset: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
    }
  }

  # Resolve the `by` grouping variable
  if (!is.null(by)) {
    if (length(by) != 1) {
      stop("`by` must be a single variable name.", call. = FALSE)
    }
    if (!by %in% names(data)) {
      stop(sprintf("The `by` variable '%s' was not found in the dataset.", by), call. = FALSE)
    }

    variables <- setdiff(variables, by)
  }

  fmt <- function(x) format(round(x, digits), nsmall = digits, trim = TRUE)

  labels_lookup <- stats::setNames(
    trimws(gsub("<.*?>", "", dic$field_label)),
    dic$field_name
  )

  get_label <- function(var) {
    lbl <- unname(labels_lookup[var])
    if (is.na(lbl) || lbl == "") var else lbl
  }

  # The set of "levels" (rows) a variable will have in the final table, computed
  # once from the full dataset so every group (subset) reports the same rows.
  compute_levels <- function(x) {
    if (is.numeric(x)) {
      c(
        if (numeric_summary %in% c("mean_sd", "both")) "Mean (SD)",
        if (numeric_summary %in% c("median_iqr", "both")) "Median [Q1, Q3]"
      )
    } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      "Range"
    } else {
      levels(droplevels(factor(as.character(x))))
    }
  }

  summarise_numeric_rows <- function(x) {
    out <- c()
    if (numeric_summary %in% c("mean_sd", "both")) {
      out["Mean (SD)"] <- if (sum(!is.na(x)) == 0) "-" else sprintf("%s (%s)", fmt(mean(x, na.rm = TRUE)), fmt(stats::sd(x, na.rm = TRUE)))
    }
    if (numeric_summary %in% c("median_iqr", "both")) {
      if (sum(!is.na(x)) == 0) {
        out["Median [Q1, Q3]"] <- "-"
      } else {
        q <- stats::quantile(x, c(0.5, 0.25, 0.75), na.rm = TRUE, names = FALSE)
        out["Median [Q1, Q3]"] <- sprintf("%s [%s, %s]", fmt(q[1]), fmt(q[2]), fmt(q[3]))
      }
    }
    out
  }

  summarise_date_row <- function(x) {
    if (sum(!is.na(x)) == 0) {
      out <- "-"
    } else {
      out <- sprintf("%s to %s", format(min(x, na.rm = TRUE)), format(max(x, na.rm = TRUE)))
    }
    stats::setNames(out, "Range")
  }

  summarise_categorical_rows <- function(x, levels_all) {
    x <- factor(as.character(x), levels = levels_all)
    n_valid <- sum(!is.na(x))
    counts <- table(x, useNA = "no")
    counts <- as.integer(counts[levels_all])
    pct <- if (n_valid > 0) 100 * counts / n_valid else rep(0, length(counts))
    stats::setNames(sprintf("%d (%s%%)", counts, fmt(pct)), levels_all)
  }

  summarise_rows <- function(df, var, levels_all, kind) {
    x <- df[[var]]
    n_total <- length(x)
    n_missing <- sum(is.na(x))

    if (n_total == 0) {
      out <- stats::setNames(rep("-", length(levels_all)), levels_all)
    } else if (kind == "numeric") {
      out <- summarise_numeric_rows(x)[levels_all]
    } else if (kind == "date") {
      out <- summarise_date_row(x)[levels_all]
    } else {
      out <- summarise_categorical_rows(x, levels_all)
    }

    missing_pct <- if (n_total > 0) 100 * n_missing / n_total else 0
    list(rows = unname(out), missing = sprintf("%d (%s%%)", n_missing, fmt(missing_pct)))
  }

  # Build the list of groups to summarize: "Overall" plus one per level of `by` (if provided)
  if (is.null(by)) {
    groups <- list(Overall = data)
  } else {
    by_vals <- data[[by]]
    by_levels <- if (is.factor(by_vals)) levels(droplevels(by_vals)) else sort(unique(stats::na.omit(as.character(by_vals))))

    if (length(by_levels) == 0) {
      stop(sprintf("The `by` variable '%s' has no non-missing values to stratify on.", by), call. = FALSE)
    }

    grp_list <- lapply(by_levels, function(lvl) data[!is.na(by_vals) & as.character(by_vals) == lvl, , drop = FALSE])
    names(grp_list) <- by_levels

    groups <- c(list(Overall = data), grp_list)
  }

  # Build one block of rows per variable
  blocks <- purrr::map(variables, function(var) {
    x_full <- data[[var]]

    kind <- if (is.numeric(x_full)) {
      "numeric"
    } else if (inherits(x_full, "Date") || inherits(x_full, "POSIXct")) {
      "date"
    } else {
      "categorical"
    }

    levels_all <- compute_levels(x_full)

    group_results <- purrr::map(groups, ~ summarise_rows(.x, var, levels_all, kind))

    value_df <- as.data.frame(
      purrr::map(group_results, "rows"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    names(value_df) <- names(groups)
    value_df$Level <- levels_all

    missing_row <- as.data.frame(
      as.list(purrr::map_chr(group_results, "missing")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    names(missing_row) <- names(groups)
    missing_row$Level <- "Missing"

    out <- rbind(value_df, missing_row)
    out$Variable <- var
    out$Label <- get_label(var)

    out[, c("Variable", "Label", "Level", names(groups))]
  })

  table_long <- as.data.frame(dplyr::bind_rows(blocks))
  rownames(table_long) <- NULL

  # Handle report title
  if (is.null(report_title)) {
    report_title <- "Descriptive summary"
  } else if (length(report_title) > 1) {
    stop("There is more than one title for the report, please choose only one.", call. = FALSE)
  }

  # Generate styled HTML summary
  viewer <- NULL
  if (isTRUE(return_viewer)) {
    viewer <- knitr::kable(table_long, align = "llc", row.names = FALSE, caption = report_title, format = "html", longtable = TRUE)
    viewer <- kableExtra::kable_styling(viewer, bootstrap_options = c("striped", "condensed"), full_width = FALSE)
    viewer <- kableExtra::row_spec(viewer, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")
    viewer <- kableExtra::collapse_rows(viewer, columns = 1:2, valign = "top")
  }

  list(
    table = table_long,
    results = viewer
  )
}
