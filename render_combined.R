#!/usr/bin/env Rscript
# render_combined.R — render one course's COMBINED dashboard across all
# active graduating classes.
#
# Usage:
#   Rscript render_combined.R --course_id 5
#   Rscript render_combined.R --course_id 5 --skip-narrative
#
# Same tryCatch/finally structure as render_dashboard.R — the pool is
# released exactly once, in the finally block, on every exit path.

suppressPackageStartupMessages({
  library(quarto); library(dplyr); library(purrr); library(readr)
})

script_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = TRUE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f <- args[grep("^--file=", args)]
    if (length(f)) normalizePath(dirname(sub("^--file=", "", f[[1]]))) else getwd()
  }
)
setwd(script_dir)

for (f in c("utils.R", "data_layer.R", "portfolio_runner.R",
            "comparison_engine.R", "ai_interpret.R",
            "plot_helpers.R", "render_helpers.R")) {
  source(file.path("R", f), local = FALSE)
}

args <- parse_cli_args()
course_id_arg <- args$course_id
skip_narr     <- isTRUE(args[["skip-narrative"]] %in% c("TRUE", "true", ""))
if (is.null(course_id_arg)) {
  cat("Usage: Rscript render_combined.R --course_id <ID> [--skip-narrative]\n")
  quit(save = "no", status = 2)
}
course_id <- as.integer(course_id_arg)

render_log(sprintf("START render_combined  course_id=%s", course_id))

course_rows <- load_course_ids()
master      <- course_master_list(course_rows)
course_row  <- master |> dplyr::filter(.data$course_id == !!course_id)
if (nrow(course_row) == 0) {
  render_log(sprintf("course_id %s not in master list", course_id), "ERROR")
  quit(save = "no", status = 1)
}

cohorts <- active_cohorts()

conn <- try_vmh_connect()
if (is.null(conn$pool)) {
  render_log(sprintf("VMH connect FAILED: %s", conn$error %||% "unknown"), "ERROR")
  quit(save = "no", status = 1)
}
render_log("VMH connected")

course_dir <- NULL

exit_status <- tryCatch({
  all_data <- purrr::map(cohorts, function(cohort) {
    period_rows <- periods_for_course_cohorts(course_rows, course_id, cohort)
    if (nrow(period_rows) == 0) {
      render_log(sprintf("  %s: no periods — skipped", cohort))
      return(NULL)
    }
    render_log(sprintf("  %s: running pipeline...", cohort))
    tryCatch(
      run_course_pipeline(
        pool = conn$pool, course_id = course_id,
        course_code = course_row$course_code[[1]],
        course_name = course_row$course_name[[1]],
        period_rows = period_rows
      ),
      error = function(e) {
        render_log(sprintf("  %s FAILED: %s", cohort, conditionMessage(e)), "WARN"); NULL
      }
    )
  }) |> purrr::compact()

  if (length(all_data) == 0) {
    render_log("No cohorts produced data — aborting.", "ERROR")
    stop("No cohorts produced data")
  }
  latest <- all_data[[length(all_data)]]

  narrative <- if (skip_narr) {
    list(available = FALSE,
         narrative = paste(
           "Course narrative was not generated for this render.",
           "Re-run without --skip-narrative to include it."
         ),
         flags = tibble::tibble(), questions = character(),
         generated_at = NULL, model = NULL)
  } else {
    tryCatch(
      generate_course_narrative(
        course_name = latest$course_name, course_code = latest$course_code,
        periods = unlist(lapply(all_data, function(r) r$periods)),
        grade_summary = dplyr::bind_rows(
          lapply(all_data, function(r) {
            bd <- assessment_breakdown(r$grades_df)
            if (nrow(bd) > 0) bd$cohort <- r$cohort_label
            bd
          })
        ),
        tag_coverage_list = latest$tag_coverage,
        comparison_data = all_data
      ),
      error = function(e) {
        render_log(sprintf("narrative FAILED: %s", conditionMessage(e)), "WARN")
        list(available = FALSE,
             narrative = sprintf("Narrative generation failed: %s", conditionMessage(e)),
             flags = tibble::tibble(), questions = character(),
             generated_at = NULL, model = NULL)
      }
    )
  }

  course_dir <<- course_folder(latest$course_code, latest$course_name)
  ensure_dir(course_dir)
  write_shared_assets(course_dir)
  res_path <- stage_rds_inputs(course_dir, "combined", all_data, narrative)

  stage_theme_for_quarto()   # copies www/custom.scss -> templates/lib/custom.scss
  output_html <- combined_html_name(latest$course_code)
  render_log(sprintf("quarto_render -> %s/%s", course_dir, output_html))
  quarto::quarto_render(
    input = file.path("templates", "course_combined_dashboard.qmd"),
    output_file = output_html,
    execute_params = list(
      all_data_path = normalizePath(res_path, mustWork = TRUE),
      render_ts     = format(Sys.time(), "%Y-%m-%d %H:%M")
    ),
    quiet = FALSE
  )

  move_quarto_outputs(output_html, course_dir)
  render_log("render OK")

  consolidate_libs(course_dir)
  clean_render_cache(course_dir)

  render_log(sprintf("DONE  %s", file.path(course_dir, output_html)))
  0L
},
error = function(e) {
  render_log(sprintf("FATAL: %s", conditionMessage(e)), "ERROR")
  if (!is.null(course_dir)) clean_render_cache(course_dir)
  1L
},
finally = {
  safe_vmh_disconnect(conn$pool)
  render_log("VMH pool released (finally)")
})

quit(save = "no", status = exit_status)
