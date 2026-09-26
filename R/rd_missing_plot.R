#' Visualize Missing Data Across a REDCap Project
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Computes and (optionally) plots the percentage of missing values for a set of variables in a REDCap dataset. For longitudinal projects, when `event_form` is provided, the percentage for each variable is computed only among the events where that variable is actually collected (via its form's entry in `event_form`), avoiding the overestimation that would result from counting rows belonging to events where the variable was never meant to be filled in.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param variables Optional character vector of variable names to include. Defaults to every field in `dic` that is present as a column in `data`. Use this argument to target specific variables, or ones whose name no longer matches the dictionary (e.g. checkbox options already renamed by `rd_checkbox()`/`rd_transform()`).
#' @param plot Logical. If `TRUE` (default), a `ggplot2` heatmap (longitudinal projects) or bar chart (non-longitudinal projects) is returned in the `plot` element. Requires the `ggplot2` package. If `FALSE`, only the numeric summary is returned.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{A data frame with columns `Variable`, `Event` (`NA` for non-longitudinal projects), `N` (rows evaluated), `N_missing`, and `Pct_missing`.}
#'   \item{plot}{A `ggplot2` object (heatmap of variables by event for longitudinal projects with `event_form`, otherwise a bar chart of variables), or `NULL` if `plot = FALSE` or `ggplot2` is not installed.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- rd_missing_plot(covican, variables = c("age", "potassium", "resp_rate"))
#' res$summary
#' res$plot
#' }
#'
#' @export
#' @importFrom rlang .data

rd_missing_plot <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, variables = NULL, plot = TRUE) {

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

  # Event filtering/grouping always relies on the *raw* event names, since
  # `event_form$unique_event_name` (used to detect which events a variable belongs to)
  # is expressed in raw form. The `.factor` version (if present) is only used to
  # produce readable labels in the returned summary/plot.
  event_col <- if ("redcap_event_name" %in% names(data)) "redcap_event_name" else NULL

  longitudinal <- !is.null(event_col)

  event_labels <- NULL
  if (longitudinal & "redcap_event_name.factor" %in% names(data)) {
    event_labels <- stats::setNames(
      as.character(data$redcap_event_name.factor),
      as.character(data$redcap_event_name)
    )
    event_labels <- event_labels[!duplicated(names(event_labels))]
  }

  if (longitudinal & is.null(event_form)) {
    warning("The project contains more than one event, but `event_form` was not provided. Missingness will be computed across all rows, which may overestimate missing values for variables that are not collected in every event.", call. = FALSE)
  }

  # Resolve variables to evaluate
  if (is.null(variables)) {
    variables <- intersect(dic$field_name, names(data))
    variables <- setdiff(variables, "record_id")

    if (length(variables) == 0) {
      stop("No variables from the dictionary were found as columns in the dataset. Please specify the `variables` argument manually (e.g. if checkbox fields have already been renamed by `rd_checkbox()`/`rd_transform()`).", call. = FALSE)
    }
  } else {
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
      stop(sprintf("The following variables were not found in the dataset: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
    }
  }

  # For each variable, find which events it is collected in (through its form), if event_form is available
  var_events <- NULL
  if (longitudinal & !is.null(event_form)) {
    var_events <- stats::setNames(
      purrr::map(variables, function(v) {
        base_field <- gsub("___.*$", "", v)
        form <- dic$form_name[dic$field_name %in% base_field]
        if (length(form) == 0) {
          character(0)
        } else {
          event_form$unique_event_name[event_form$form %in% form[1]]
        }
      }),
      variables
    )
  }

  # Compute missingness per variable (and per event, if applicable)
  summary_df <- purrr::map_dfr(variables, function(v) {
    sub <- data

    if (!is.null(var_events)) {
      ev <- var_events[[v]]
      if (length(ev) > 0) {
        sub <- sub[sub[[event_col]] %in% ev, , drop = FALSE]
      }
    }

    if (!longitudinal) {
      n <- nrow(sub)
      n_missing <- sum(is.na(sub[[v]]))

      tibble::tibble(
        Variable = v,
        Event = NA_character_,
        N = n,
        N_missing = n_missing,
        Pct_missing = if (n > 0) 100 * n_missing / n else NA_real_
      )
    } else {
      sub |>
        dplyr::mutate(.event = as.character(.data[[event_col]])) |>
        dplyr::group_by(.data$.event) |>
        dplyr::summarise(
          N = dplyr::n(),
          N_missing = sum(is.na(.data[[v]])),
          .groups = "drop"
        ) |>
        dplyr::transmute(
          Variable = v,
          Event = if (!is.null(event_labels)) unname(event_labels[.data$.event]) else .data$.event,
          N = .data$N,
          N_missing = .data$N_missing,
          Pct_missing = ifelse(.data$N > 0, 100 * .data$N_missing / .data$N, NA_real_)
        )
    }
  })

  summary_df <- as.data.frame(summary_df)

  # Build the plot
  gg <- NULL

  if (isTRUE(plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("The `ggplot2` package is required to build the plot. Install it with `install.packages('ggplot2')`. Returning the numeric summary only.", call. = FALSE)
    } else if (longitudinal) {
      var_order <- summary_df |>
        dplyr::group_by(.data$Variable) |>
        dplyr::summarise(m = mean(.data$Pct_missing, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(.data$m) |>
        dplyr::pull(.data$Variable)

      summary_df_plot <- summary_df
      summary_df_plot$Variable <- factor(summary_df_plot$Variable, levels = var_order)

      gg <- ggplot2::ggplot(summary_df_plot, ggplot2::aes(x = .data$Event, y = .data$Variable, fill = .data$Pct_missing)) +
        ggplot2::geom_tile(color = "#fcfcfb", linewidth = 0.5) +
        ggplot2::scale_fill_gradient(
          name = "% missing",
          low = "#cde2fb", high = "#0d366b",
          limits = c(0, 100)
        ) +
        ggplot2::labs(title = "Missing data by variable and event", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, color = "#52514e"),
          axis.text.y = ggplot2::element_text(color = "#52514e"),
          plot.title = ggplot2::element_text(color = "#0b0b0b")
        )
    } else {
      ord <- summary_df[order(summary_df$Pct_missing), ]
      ord$Variable <- factor(ord$Variable, levels = ord$Variable)

      gg <- ggplot2::ggplot(ord, ggplot2::aes(x = .data$Pct_missing, y = .data$Variable)) +
        ggplot2::geom_col(fill = "#256abf", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$Pct_missing)), hjust = -0.1, size = 3, color = "#52514e") +
        ggplot2::scale_x_continuous(limits = c(0, max(100, max(ord$Pct_missing, na.rm = TRUE) * 1.15)), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(title = "Missing data by variable", x = "% missing", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
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
