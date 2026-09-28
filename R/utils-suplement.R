############### Other functions###############

## Fill_data ----

#' Fill Rows with Values from One Event
#' @description
#' This function fills all rows in the dataset with the value of a particular variable in a specified event. It is an auxiliary function used in the `rd_rlogic` function.
#' @param which_event String specifying the name of the event.
#' @param which_var String specifying the name of the variable.
#' @param data Dataset containing the REDCap data.

fill_data <- function(which_event, which_var, data) {

  if (!which_event %in% data$redcap_event_name) {
    stop("The logic can't be evaluated after the translation", call. = FALSE)
  }

  fill_values <- data |>
    dplyr::select("record_id", "redcap_event_name", dplyr::all_of(which_var)) |>
    dplyr::rename(var = dplyr::all_of(which_var)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::mutate(var = dplyr::case_when(.data$redcap_event_name != which_event ~ NA, TRUE ~ .data$var)) |>
    tidyr::fill("var", .direction = "downup") |>
    dplyr::pull("var")

  data[[which_var]] <- fill_values

  data
}


## Check_proj ----

#' Handle Project Arguments
#'
#' This helper function processes the `project` argument in the several other functions.
#' It extracts data, dictionary, event form, and results from the project object while handling
#' potential duplication and providing warnings when arguments are provided redundantly.
#'
#' @param project A list or object containing `data`, `dictionary`, and optionally `event_form` and `results`.
#' @param data A data frame (optional) that may be overridden if provided in the `project` object.
#' @param dic A data dictionary (optional) that may be overridden if provided in the `project` object.
#' @param event_form An optional event-form object that may be overridden if provided in the `project`.
#'
#'
check_proj <- function(project, data = NULL, dic = NULL, event_form = NULL) {
  # Ensure 'project' is a list
  if (!is.list(project)) {
    stop("The 'project' argument must be a list.", call. = FALSE)
  }

  # Check for `data` duplication
  if (!is.null(data)) {
    warning("Data has been provided twice. The function will ignore the `data` argument.")
  }

  # Check for `dic` duplication
  if (!is.null(dic)) {
    warning("Dictionary has been provided twice. The function will ignore the `dic` argument.")
  }

  # Extract data and dictionary from the project
  data <- if (!is.null(project$data)) project$data else if (!is.null(project$dat)) project$dat else NULL
  dic <- if (!is.null(project$dictionary)) project$dictionary else if (!is.null(project$dic)) project$dic else NULL

  # Handle `event_form` duplication
  if ("event_form" %in% names(project)) {
    if (!is.null(event_form)) {
      warning("Event-form has been provided twice. The function will ignore the `event_form` argument.")
    }
    event_form <- project$event_form
  }

  # Extract results from the project if present
  if ("results" %in% names(project)) {
    results <- project$results
  } else {
    results <- NULL
  }

  # Return updated arguments as a list
    list(
      data = data, dic = dic, event_form = event_form, results = results
    )
}


## normalize_queries ----

#' Normalize a `queries` Argument Into a Plain Data Frame
#'
#' This helper function accepts either a queries data frame directly, or a list
#' containing one (e.g. the direct output of `rd_query()`, `rd_event()`, or
#' `check_queries()`, including the `by_dag = TRUE` list-of-data-frames form),
#' and returns a single validated data frame with at least an `Identifier` column.
#' Used internally by `rd_write_queries()` and `run_query_app()`.
#'
#' @param queries A data frame of queries, or a list containing (or being) one.
#'
#' @return A validated data frame.

normalize_queries <- function(queries) {
  if (is.list(queries) && !is.data.frame(queries) && "queries" %in% names(queries)) {
    queries <- queries$queries
  }

  if (is.list(queries) && !is.data.frame(queries)) {
    # by_dag = TRUE output: a list of data frames, one per DAG
    queries <- dplyr::bind_rows(queries)
  }

  if (!is.data.frame(queries)) {
    stop("`queries` must be a data frame, or a list containing a `queries` element (the output of `rd_query()`, `rd_event()`, or `check_queries()`).", call. = FALSE)
  }

  if (!"Identifier" %in% names(queries)) {
    stop("`queries` must contain an `Identifier` column.", call. = FALSE)
  }

  if (nrow(queries) == 0) {
    stop("`queries` has no rows to write.", call. = FALSE)
  }

  queries
}


#' Round Numbers to a Specified Number of Digits ----
#'
#' This function rounds numeric values to the specified number of decimal digits,
#' mimicking the behavior of the base R `round()` function but implemented manually.
#'
#' @param x A numeric vector to be rounded.
#' @param digits Integer indicating the number of decimal places to round to.
#'
#' @return A numeric vector rounded to the specified number of digits.
#' @examples
#' round(3.14159, 2)
#' round(c(-2.718, 3.14159), 1)
#'
round <- function(x, digits) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}


## build_html_table ----

#' Build a Styled HTML Summary Table, or a Plain Fallback
#'
#' Internal helper shared by every function that returns a styled HTML
#' `results`/`viewer` table (`knitr::kable()` + `kableExtra`). Centralizes the
#' `requireNamespace()` guard so a missing `knitr`/`kableExtra` (both Suggests
#' packages) produces a clear warning and `NULL` instead of a cryptic
#' "could not find function" error partway through the function.
#'
#' @param df A data.frame to render.
#' @param align Alignment string passed to `knitr::kable()`.
#' @param caption Optional caption/title.
#' @param collapse_cols Optional integer vector of column indices passed to
#'   `kableExtra::collapse_rows()` (used by `rd_codebook()`).
#'
#' @return A styled HTML table object, or `NULL` (with a warning) if `knitr`
#'   or `kableExtra` are not installed.
build_html_table <- function(df, align, caption = NULL, collapse_cols = NULL) {
  if (!requireNamespace("knitr", quietly = TRUE) || !requireNamespace("kableExtra", quietly = TRUE)) {
    warning("The `knitr` and `kableExtra` packages are required to build the HTML summary table. Install them with `install.packages(c('knitr', 'kableExtra'))`. Returning `NULL` for this element; the underlying data is still available in the other element(s) of the result.", call. = FALSE)
    return(NULL)
  }

  viewer <- knitr::kable(df, align = align, row.names = FALSE, caption = caption, format = "html", longtable = TRUE)
  viewer <- kableExtra::kable_styling(viewer, bootstrap_options = c("striped", "condensed"), full_width = FALSE)
  viewer <- kableExtra::row_spec(viewer, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")

  if (!is.null(collapse_cols)) {
    viewer <- kableExtra::collapse_rows(viewer, columns = collapse_cols, valign = "top")
  }

  viewer
}


## build_pipe_table ----

#' Build a Markdown ("pipe") Table Fragment, or a Plain-Text Fallback
#'
#' Internal helper shared by every function that appends a small markdown
#' table to the character `results` summary via `knitr::kable(..., "pipe")`.
#' Falls back to a plain `print()`-based rendering (base R, always available)
#' when `knitr` isn't installed, instead of erroring outright.
#'
#' @param df A data.frame to render.
#' @param align Alignment string passed to `knitr::kable()`.
#' @param caption Optional caption.
#'
#' @return A character vector with the table rendered as text.
build_pipe_table <- function(df, align, caption = NULL) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    out <- utils::capture.output(print(df, row.names = FALSE))
    if (!is.null(caption)) out <- c(paste0(caption, ":"), out)
    return(out)
  }

  knitr::kable(df, "pipe", align = align, caption = caption)
}
