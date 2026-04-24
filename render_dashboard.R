#!/usr/bin/env Rscript
# render_dashboard.R — render a course dashboard.
#
# TWO modes:
#   1. All years (primary) — one HTML per course, all active cohorts
#      rendered into course_all_years_dashboard.qmd with client-side
#      cohort-filter chip bar:
#        Rscript render_dashboard.R --course_id 5 --all-classes
#      Output: outputs/{course_code}_{course_name}/{course_code}_all_years.html
#
#   2. Single class (shareable per-class snapshot) —
#      course_year_dashboard.qmd, one HTML per course+cohort:
#        Rscript render_dashboard.R --course_id 5 --class "Class of 2026"
#      Output: outputs/{course_code}_{course_name}/{course_code}_Class_of_YYYY.html
#
# Shared flags: --with-exports, --skip-narrative, --save-fixture.
#
# Pool lifecycle: opened once at the top, released in `finally` (exactly once,
# on every exit path — protects against dbplyr/pool "checked-out object
# deleted before being returned" warnings).

suppressPackageStartupMessages({
  library(quarto); library(readr); library(dplyr); library(purrr); library(jsonlite)
})

# ---- Resolve project root + load R layer ------------------------------------
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
            "plot_helpers.R", "html_components.R", "render_helpers.R")) {
  source(file.path("R", f), local = FALSE)
}

# ---- CLI args ---------------------------------------------------------------
args <- parse_cli_args()
course_id_arg <- args$course_id
class_arg     <- args$class
all_classes   <- isTRUE(args[["all-classes"]]     %in% c("TRUE", "true", ""))
with_exports  <- isTRUE(args[["with-exports"]]    %in% c("TRUE", "true", ""))
skip_narr     <- isTRUE(args[["skip-narrative"]]  %in% c("TRUE", "true", ""))
save_fixture  <- isTRUE(args[["save-fixture"]]    %in% c("TRUE", "true", ""))

if (is.null(course_id_arg) || (!all_classes && is.null(class_arg))) {
  cat(
    "Usage:\n",
    "  Rscript render_dashboard.R --course_id <ID> --all-classes [flags]\n",
    "  Rscript render_dashboard.R --course_id <ID> --class \"Class of YYYY\" [flags]\n",
    "\nFlags: --with-exports  --skip-narrative  --save-fixture\n", sep = ""
  )
  quit(save = "no", status = 2)
}
course_id <- as.integer(course_id_arg)

mode_str <- if (all_classes) "all-classes" else sprintf("class=%s", class_arg)
render_log(sprintf("START render_dashboard  course_id=%s  %s", course_id, mode_str))

# ---- Offline validation -----------------------------------------------------
course_rows <- load_course_ids()
master      <- course_master_list(course_rows)

course_row <- master |> dplyr::filter(.data$course_id == !!course_id)
if (nrow(course_row) == 0) {
  render_log(sprintf("course_id %s not in master list", course_id), "ERROR")
  quit(save = "no", status = 1)
}

# For single-class mode, verify the cohort has periods for this course.
if (!all_classes) {
  period_rows_check <- periods_for_course_cohorts(course_rows, course_id, class_arg)
  if (nrow(period_rows_check) == 0) {
    render_log(sprintf("no periods for course_id=%s cohort=%s", course_id, class_arg), "ERROR")
    quit(save = "no", status = 1)
  }
}

# ---- VMH pool ---------------------------------------------------------------
conn <- try_vmh_connect()
if (is.null(conn$pool)) {
  render_log(sprintf("VMH connect FAILED: %s", conn$error %||% "unknown"), "ERROR")
  quit(save = "no", status = 1)
}
render_log("VMH connected")

course_dir <- NULL

exit_status <- tryCatch({

  if (all_classes) {
    # -----------------------------------------------------------------------
    # MULTI-COHORT PATH — run the pipeline for every active cohort that has
    # periods for this course. Each cohort's result is added to all_data.
    # -----------------------------------------------------------------------
    cohorts <- active_cohorts()
    render_log(sprintf("iterating %d active cohorts: %s",
                       length(cohorts), paste(cohorts, collapse = ", ")))

    all_data <- purrr::map(cohorts, function(cohort) {
      pr <- periods_for_course_cohorts(course_rows, course_id, cohort)
      if (nrow(pr) == 0) {
        render_log(sprintf("  %s: no periods — skipped", cohort)); return(NULL)
      }
      render_log(sprintf("  %s: running pipeline...", cohort))
      tryCatch(
        run_course_pipeline(
          pool        = conn$pool,
          course_id   = course_id,
          course_code = course_row$course_code[[1]],
          course_name = course_row$course_name[[1]],
          period_rows = pr
        ),
        error = function(e) {
          render_log(sprintf("  %s FAILED: %s", cohort, conditionMessage(e)), "WARN"); NULL
        }
      )
    }) |> purrr::compact()

    if (length(all_data) == 0) stop("No cohorts produced data — aborting.")
    latest <- all_data[[length(all_data)]]

    # -- Narrative (course-level, across all cohorts)
    #
    # All-years dashboard ALWAYS uses the rule-based narrative. OpenAI is
    # reserved for single-class --class renders. This is deliberate: the
    # all-years narrative is the primary product of this dashboard, and
    # it must be deterministic and reproducible for the curriculum
    # committee / AVMA accreditation context. --skip-narrative is kept
    # as a CLI flag for backward compat but no longer alters behavior
    # here — the rule-based generator is fast enough that there's no
    # reason to skip it.
    narrative <- tryCatch(
      generate_rule_based_narrative(
        all_data, cohort_colors = NULL,
        render_ts = format(Sys.time(), "%Y-%m-%d %H:%M")
      ),
      error = function(e) {
        render_log(sprintf("rule-based narrative FAILED: %s", conditionMessage(e)), "WARN")
        list(available = FALSE, source = "rule-based",
             narrative = sprintf("Rule-based narrative generation failed: %s",
                                 conditionMessage(e)),
             flags = tibble::tibble(), questions = character(),
             generated_at = NULL, model = "rule-based")
      }
    )

    course_dir <<- course_folder(latest$course_code, latest$course_name)
    ensure_dir(course_dir)
    write_shared_assets(course_dir)
    res_path <- stage_rds_inputs(course_dir, "all_years", all_data, narrative)

    # ---- Cohort colors + period index (single source of truth) -----------
    # Both R side (plot generation in the template) and JS side (filter bar)
    # read from the same serialized JSON payload so the colors are always
    # aligned across the rendered dashboard.
    names(all_data) <- vapply(all_data, function(r) r$cohort_label, character(1))
    cohort_colors <- assign_cohort_colors(names(all_data))
    period_index  <- build_period_index(all_data)
    # Attach color to each period entry — keeps the JS filter bar simpler.
    period_index <- lapply(period_index, function(p) {
      p$color <- unname(cohort_colors[[p$cohort_label]] %||% "#003F6B")
      p
    })
    cohort_colors_json <- jsonlite::toJSON(as.list(cohort_colors),
                                           auto_unbox = TRUE, null = "null")
    period_index_json  <- jsonlite::toJSON(period_index,
                                           auto_unbox = TRUE, null = "null")
    render_log(sprintf("cohort colors assigned: %s",
                       paste(sprintf("%s=%s", names(cohort_colors),
                                     unname(cohort_colors)), collapse = ", ")))

    # ---- Survey folders — per-cohort lifecycle folders -------------------
    # After the folder is created, scan the existing Box course-portfolio
    # path for PDFs whose filename matches the course digits + "survey"
    # pattern, scoped to the cohort's year. Copy matches in so Ish can see
    # them immediately in the Downloads tab.
    for (r in all_data) {
      mf <- r$max_period_finish_date
      if (!is.null(mf) && !is.na(mf) && (as.Date(mf) + 2) <= Sys.Date()) {
        s_dir <- surveys_dir(r$course_code, r$cohort_label)
        if (!dir.exists(s_dir)) {
          ensure_dir(s_dir)
          render_log(sprintf("surveys folder ready -> %s", s_dir))
        }
        copy_box_survey_pdfs(
          course_code   = r$course_code,
          cohort_label  = r$cohort_label,
          target_dir    = s_dir,
          logger        = render_log
        )
      }
    }

    # ---- Cohort-scale sanity check ------------------------------------
    # Gradebook math can legitimately produce medians > 100 (extra credit
    # pushing weighted overall grade above 100%). Flag it here so anomalies
    # surface in the log without us silently clamping the data.
    for (r in all_data) {
      m <- r$kpi$overall_median
      if (!is.null(m) && is.finite(m) && m > 100) {
        render_log(sprintf(
          "SCALE WARNING: %s median = %.1f%% (>100) — likely extra-credit inflating weighted overall grade",
          r$cohort_label, m
        ), "WARN")
      }
    }

    if (with_exports) build_export_artifacts(conn$pool, latest, course_dir)

    stage_theme_for_quarto()
    safe_code  <- gsub("[^A-Za-z0-9]", "", latest$course_code)
    output_html <- sprintf("%s_all_years.html", safe_code)
    render_log(sprintf("quarto_render -> %s/%s", course_dir, output_html))

    quarto::quarto_render(
      input = file.path("templates", "course_all_years_dashboard.qmd"),
      output_file = output_html,
      execute_params = list(
        all_data_path      = normalizePath(res_path, mustWork = TRUE),
        cohort_colors_json = as.character(cohort_colors_json),
        period_index_json  = as.character(period_index_json),
        render_ts          = format(Sys.time(), "%Y-%m-%d %H:%M")
      ),
      quiet = FALSE
    )
    move_quarto_outputs(output_html, course_dir)
    render_log("render OK")

    if (save_fixture) {
      fx_dir <- file.path(project_root(), "tests", "fixtures")
      ensure_dir(fx_dir)
      fx_tag <- sprintf("%s_all_years", gsub("[^A-Za-z0-9]", "", latest$course_code))
      fx_path <- file.path(fx_dir, sprintf("%s.rds", fx_tag))
      file.copy(res_path, fx_path, overwrite = TRUE)
      narr_src <- sub("\\.rds$", "_narrative.rds", res_path)
      if (file.exists(narr_src)) {
        file.copy(narr_src, sub("\\.rds$", "_narrative.rds", fx_path), overwrite = TRUE)
      }
      render_log(sprintf("saved fixture -> %s", fx_path))
    }

    consolidate_libs(course_dir)
    clean_render_cache(course_dir)

    render_log(sprintf("DONE  %s", file.path(course_dir, output_html)))
    0L

  } else {
    # -----------------------------------------------------------------------
    # SINGLE-CLASS PATH — legacy shareable render for a single cohort.
    # -----------------------------------------------------------------------
    cohort <- class_arg
    pr <- periods_for_course_cohorts(course_rows, course_id, cohort)

    result <- run_course_pipeline(
      pool        = conn$pool,
      course_id   = course_id,
      course_code = course_row$course_code[[1]],
      course_name = course_row$course_name[[1]],
      period_rows = pr
    )
    render_log(sprintf(
      "pipeline complete  gradebook_ok=%s  tag_sets=%d  periods_with_tags=%d",
      result$gradebook_ok,
      result$kpi$n_tag_sets        %||% 0L,
      result$kpi$periods_with_tags %||% 0L
    ))

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
          course_name = result$course_name, course_code = result$course_code,
          periods = result$periods,
          grade_summary = assessment_breakdown(result$grades_df),
          tag_coverage_list = result$tag_coverage
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

    course_dir <<- course_folder(result$course_code, result$course_name)
    ensure_dir(course_dir)
    write_shared_assets(course_dir)
    tag <- cohort_slug(cohort)
    res_path <- stage_rds_inputs(course_dir, tag, result, narrative)

    max_finish <- result$max_period_finish_date
    if (!is.null(max_finish) && !is.na(max_finish) &&
        (as.Date(max_finish) + 2) <= Sys.Date()) {
      s_dir <- surveys_dir(result$course_code, cohort)
      if (!dir.exists(s_dir)) {
        ensure_dir(s_dir)
        render_log(sprintf("surveys folder ready -> %s", s_dir))
      }
    }

    if (with_exports) build_export_artifacts(conn$pool, result, course_dir)

    stage_theme_for_quarto()
    output_html <- year_html_name(result$course_code, result$cohort_label)
    render_log(sprintf("quarto_render -> %s/%s", course_dir, output_html))
    quarto::quarto_render(
      input = file.path("templates", "course_year_dashboard.qmd"),
      output_file = output_html,
      execute_params = list(
        result_path = normalizePath(res_path, mustWork = TRUE),
        render_ts   = format(Sys.time(), "%Y-%m-%d %H:%M")
      ),
      quiet = FALSE
    )
    move_quarto_outputs(output_html, course_dir)
    render_log("render OK")

    if (save_fixture) {
      fx_dir <- file.path(project_root(), "tests", "fixtures")
      ensure_dir(fx_dir)
      fx_tag <- sprintf("%s_%s", gsub("[^A-Za-z0-9]", "", result$course_code), cohort_slug(cohort))
      fx_path <- file.path(fx_dir, sprintf("%s.rds", fx_tag))
      file.copy(res_path, fx_path, overwrite = TRUE)
      narr_src <- sub("\\.rds$", "_narrative.rds", res_path)
      if (file.exists(narr_src)) {
        file.copy(narr_src, sub("\\.rds$", "_narrative.rds", fx_path), overwrite = TRUE)
      }
      render_log(sprintf("saved fixture -> %s", fx_path))
    }

    consolidate_libs(course_dir)
    clean_render_cache(course_dir)

    render_log(sprintf("DONE  %s", file.path(course_dir, output_html)))
    0L
  }
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
