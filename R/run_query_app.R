## add_status_columns ----

#' Add Default Status/Comment Columns to a Queries Data Frame
#'
#' Adds a `Status` column (defaulting to `"Pending"`) and a `Comment` column
#' (defaulting to `""`) to a queries data frame, if they are not already present.
#' Used internally by `run_query_app()` (and reusable directly for scripted
#' query-review workflows).
#'
#' @param queries A data frame of queries (e.g. the `queries` element of
#'   `rd_query()`/`rd_event()`/`check_queries()`).
#'
#' @return `queries` with `Status` and `Comment` columns added (if missing).

add_status_columns <- function(queries) {
  if (!"Status" %in% names(queries)) {
    queries$Status <- "Pending"
  }
  if (!"Comment" %in% names(queries)) {
    queries$Comment <- ""
  }
  queries
}


## run_query_app ----

#' Launch a Shiny App to Review and Annotate REDCap Queries
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Launches an interactive Shiny app to browse, filter, and annotate a query report produced by [rd_query()], [rd_event()], or [check_queries()]. Each query can be marked with a `Status` (e.g. `Pending`/`Resolved`/`False alarm`) and a free-text `Comment` directly in the table. The annotated table can be exported to Excel (via [rd_export()]) and, optionally, pushed back into REDCap (via [rd_write_queries()]) after a preview/confirmation step.
#'
#' @param queries A data frame of queries, or a list containing one (e.g. the direct output of [rd_query()], [rd_event()], or [check_queries()]).
#' @param data Optional `data.frame`/`tibble` of the REDCap dataset, passed through to [rd_write_queries()] to resolve event labels back to raw REDCap event names (only needed if pushing to REDCap; see [rd_write_queries()]).
#' @param uri,token,field Optional REDCap API connection details and target field name. If all three are supplied, a "Push to REDCap" action becomes available in the app (gated behind a preview/confirmation modal). If any is missing, the app works in local/export-only mode.
#' @param launch Logical. If `TRUE` (default), the app is launched immediately with [shiny::runApp()]. If `FALSE`, the `shiny.appobj` is returned without being launched (useful for embedding in another app, or for testing).
#'
#' @details
#' Requires the `shiny` and `DT` packages (both in Suggests).
#'
#' Pushing to REDCap from the app always goes through the same `dry_run` preview as [rd_write_queries()]: clicking "Push to REDCap" shows the exact rows/values that would be written and asks for confirmation before anything is actually sent.
#'
#' @return If `launch = TRUE`, invisibly returns `NULL` after the app has been closed (as [shiny::runApp()] does). If `launch = FALSE`, returns a `shiny.appobj` (from [shiny::shinyApp()]) that can be launched later with [shiny::runApp()] or by printing it in an interactive session.
#'
#' @examples
#' \dontrun{
#' result <- rd_query(covican, variables = "age", expression = "is.na(x)", event = "baseline_visit_arm_1")
#' run_query_app(result)
#'
#' # With REDCap push-back enabled
#' run_query_app(
#'   result,
#'   data = covican$data,
#'   uri = "https://redcap.example.org/api/",
#'   token = "REPLACE_WITH_TOKEN",
#'   field = "dm_query_note"
#' )
#' }
#'
#' @export

run_query_app <- function(queries, data = NULL, uri = NULL, token = NULL, field = NULL, launch = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("DT", quietly = TRUE)) {
    stop("The `shiny` and `DT` packages are required to use `run_query_app()`. Install them with `install.packages(c('shiny', 'DT'))`.", call. = FALSE)
  }

  queries <- normalize_queries(queries)
  queries <- add_status_columns(queries)

  push_enabled <- !is.null(uri) && !is.null(token) && !is.null(field)

  filter_choices <- function(col) {
    if (col %in% names(queries)) sort(unique(as.character(stats::na.omit(queries[[col]])))) else character(0)
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("REDCapDM - Query review"),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput("f_dag", "DAG", choices = c("All", filter_choices("DAG")), selected = "All")),
      shiny::column(3, shiny::selectInput("f_event", "Event", choices = c("All", filter_choices("Event")), selected = "All")),
      shiny::column(3, shiny::selectInput("f_field", "Variable", choices = c("All", filter_choices("Field")), selected = "All")),
      shiny::column(3, shiny::selectInput("f_status", "Status", choices = c("All", "Pending", "Resolved", "False alarm"), selected = "All"))
    ),
    shiny::helpText("Double-click a Status or Comment cell to edit it, then press Ctrl+Enter to save the change."),
    shiny::fluidRow(
      shiny::column(12, DT::DTOutput("queries_table"))
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(4, shiny::downloadButton("download", "Download annotated report (.xlsx)")),
      if (push_enabled) shiny::column(4, shiny::actionButton("push_preview", "Push to REDCap..."))
    )
  )

  server <- function(input, output, session) {
    state <- shiny::reactiveVal(queries)

    filtered_idx <- shiny::reactive({
      df <- state()
      keep <- rep(TRUE, nrow(df))
      if (input$f_dag != "All" && "DAG" %in% names(df)) keep <- keep & df$DAG == input$f_dag
      if (input$f_event != "All" && "Event" %in% names(df)) keep <- keep & df$Event == input$f_event
      if (input$f_field != "All" && "Field" %in% names(df)) keep <- keep & df$Field == input$f_field
      if (input$f_status != "All") keep <- keep & df$Status == input$f_status
      which(keep)
    })

    output$queries_table <- DT::renderDT({
      DT::datatable(
        state()[filtered_idx(), , drop = FALSE],
        editable = list(target = "column", disable = list(columns = which(!names(state()) %in% c("Status", "Comment")) - 1)),
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$queries_table_cell_edit, {
      info <- input$queries_table_cell_edit
      visible <- state()[filtered_idx(), , drop = FALSE]
      edited <- DT::editData(visible, info, rownames = FALSE)

      full <- state()
      full[filtered_idx(), ] <- edited
      state(full)
    })

    output$download <- shiny::downloadHandler(
      filename = function() "queries_report.xlsx",
      content = function(file) {
        rd_export(queries = state(), path = file)
      }
    )

    if (push_enabled) {
      shiny::observeEvent(input$push_preview, {
        preview <- tryCatch(
          rd_write_queries(data = data, queries = state(), uri = uri, token = token, field = field, dry_run = TRUE),
          error = function(e) e
        )

        if (inherits(preview, "error")) {
          shiny::showModal(shiny::modalDialog(
            title = "Cannot preview the REDCap write",
            conditionMessage(preview),
            easyClose = TRUE
          ))
          return(invisible())
        }

        shiny::showModal(shiny::modalDialog(
          title = "Confirm push to REDCap",
          shiny::p(sprintf("This will write %d row(s) into the '%s' field.", nrow(preview$data), field)),
          DT::renderDT(DT::datatable(preview$data, rownames = FALSE, options = list(pageLength = 5))),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("push_confirm", "Confirm and write")
          ),
          size = "l", easyClose = TRUE
        ))
      })

      shiny::observeEvent(input$push_confirm, {
        shiny::removeModal()

        result <- tryCatch(
          rd_write_queries(data = data, queries = state(), uri = uri, token = token, field = field, dry_run = FALSE),
          error = function(e) e
        )

        msg <- if (inherits(result, "error")) {
          sprintf("The write to REDCap failed: %s", conditionMessage(result))
        } else {
          sprintf("Wrote %d row(s) to the '%s' field in REDCap.", nrow(result$data), field)
        }

        shiny::showModal(shiny::modalDialog(title = "REDCap write result", msg, easyClose = TRUE))
      })
    }
  }

  app <- shiny::shinyApp(ui, server)

  if (isTRUE(launch)) {
    shiny::runApp(app)
  } else {
    app
  }
}
