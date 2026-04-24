# R/portfolio_runner.R
# -----------------------------------------------------------------------------
# Thin wrappers around cvm.course.portfolio that:
#   * pick aggregated vs. non-aggregated assessment plots based on course name
#   * auto-set use_end_date when a period's finish date is in the future
#   * pass rgx (LOGICAL) — events_by_course_period looks up the regex internally
#     via cvm.courseEventFilter::regex_by_course_id(course_ID, course_Period)
#   * compute KPI summary stats from gradebook data
# The existing ggplot2 objects returned by cvm.course.portfolio are PRESERVED
# and returned as-is — we do not mutate them.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Per-course, per-cohort pipeline
# -----------------------------------------------------------------------------

#' Run the full data pipeline for one (course_id, cohort) pair and return a
#' list of everything the dashboard needs to render that course/cohort.
#'
#' @param pool         a vmh pool object (must be non-NULL)
#' @param course_id    integer — VMH course identifier
#' @param course_code  character — display code ("VETM 801")
#' @param course_name  character — full name ("Foundations")
#' @param period_rows  rows from the class CSV filtered to this course+cohort
#' @param use_regex    logical — whether to enable event regex filtering
#' @return a list (safe to saveRDS and reload)
run_course_pipeline <- function(pool,
                                course_id,
                                course_code,
                                course_name,
                                period_rows,
                                use_regex = TRUE) {

  stopifnot(!is.null(pool))
  stopifnot(nrow(period_rows) > 0)

  # Auto-select the correct use_end_date. If any period ends in the future,
  # pass Sys.Date() so cvm.course.portfolio treats it as valid.
  max_finish <- suppressWarnings(max(period_rows$period_finish_date, na.rm = TRUE))
  use_end <- if (is.finite(max_finish) && max_finish > Sys.Date()) {
    Sys.Date()
  } else if (is.finite(max_finish)) {
    max_finish
  } else {
    Sys.Date()
  }

  periods <- sort(unique(as.integer(period_rows$cperiod_id)))
  cohort_label <- unique(period_rows$cohort_label)[[1]]

  # Per-period metadata carried through to the client-side filter. The
  # template serializes this via build_period_index() → JSON so the
  # sticky chip bar can render pretty labels like "GC26 · Fall Y1".
  period_meta <- period_rows |>
    dplyr::distinct(cperiod_id, curriculum_type_name, period_start_date, period_finish_date) |>
    dplyr::arrange(cperiod_id)

  # Per-period gradebook data.
  #
  # IMPORTANT — known failure mode: cvm.gradebook::gradebook() emits failures
  # via message() rather than stop() AND takes 30–90s on the happy path. So:
  #   * we wrap with withCallingHandlers to capture messages (both success and
  #     failure signals), log them with timestamps, and let execution continue
  #   * we inspect the returned value — NULL / empty / error-shaped means the
  #     pull failed, and we set gradebook_ok = FALSE on the result so the
  #     Quarto template can surface a "gradebook unavailable" warning banner
  #     while still rendering the tag coverage + comparison tabs cleanly
  grades_messages <- character()
  grades_by_period <- lapply(periods, function(p) {
    log_render_step(sprintf("gradebook_data course=%s period=%s — start", course_id, p))
    t0 <- Sys.time()
    out <- withCallingHandlers(
      tryCatch({
        cvm.course.portfolio::gradebook_data(
          con         = pool,
          courseID    = course_id,
          coursePeriod = p,
          use_end_date = use_end
        )
      }, error = function(e) {
        grades_messages <<- c(grades_messages,
                              sprintf("period %s ERROR: %s", p, conditionMessage(e)))
        NULL
      }),
      message = function(m) {
        txt <- conditionMessage(m)
        # Squelch noisy "processing…" lines but keep error-looking ones.
        if (grepl("Error|fail|invalid|missing|not found", txt, ignore.case = TRUE)) {
          grades_messages <<- c(grades_messages,
                                sprintf("period %s: %s", p, trimws(txt)))
        }
        invokeRestart("muffleMessage")
      }
    )
    log_render_step(sprintf(
      "gradebook_data course=%s period=%s — %s (%.1fs)",
      course_id, p,
      if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) "no data" else sprintf("%d rows", nrow(out)),
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
    out
  })
  names(grades_by_period) <- as.character(periods)

  grades_df <- dplyr::bind_rows(
    Filter(function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0, grades_by_period),
    .id = "cperiod_id"
  )
  gradebook_ok <- !is.null(grades_df) && is.data.frame(grades_df) && nrow(grades_df) > 0

  # Assessment plots: route multi-component courses (VETM 804 A/B/C/D) to the
  # aggregated variant as the spec requires.
  plot_fn <- if (is_multi_component_course(course_name)) {
    cvm.course.portfolio::generate_aggregated_assessment_plots
  } else {
    cvm.course.portfolio::generate_assessment_plots
  }

  assessment_plots <- tryCatch(
    plot_fn(
      con         = pool,
      periods     = periods,
      course_ID   = course_id,
      use_end_date = use_end
    ),
    error = function(e) {
      message(sprintf("assessment plot generation failed: %s", conditionMessage(e)))
      NULL
    }
  )

  # Tagset plots — need a named `data_list` and `classes` keyed by the period id.
  classes <- as.list(rep(sub("Class of ", "", cohort_label), length(periods)))
  names(classes) <- as.character(periods)

  data_list <- lapply(periods, function(p) {
    tryCatch(
      event.curriculum.tags::events_by_course_period(
        con           = pool,
        course_ID     = course_id,
        course_Period = p,
        start         = FALSE,
        stop          = FALSE,
        use_regex     = isTRUE(use_regex)
      ),
      error = function(e) {
        message(sprintf("events pull failed period=%s: %s", p, conditionMessage(e)))
        NULL
      }
    )
  })
  names(data_list) <- as.character(periods)

  # Both event.curriculum.tags::curriculum_tag_counts_by_events AND
  # cvm.course.portfolio::generate_tagset_plots internally call
  # get_co2pig_map(), which in turn makes three boxr::box_search_files calls.
  # When the per-course CO->PIG xlsx isn't present in any of the three Box
  # folders, boxr emits "box.com indicates that no search results were found"
  # for each — typically 3 per call, × 2 call sites × N periods. This is
  # designed behavior (the function falls back to
  # curriculum.tag.maps::co2pigs_map) but drowns our logs.
  #
  # with_box_fallback_notice: intercept only those specific messages, count
  # them, and emit a single explanatory log line the first time we see one.
  # Every other message() call (e.g. "tag counts failed") still flows through.
  box_fallback_seen <- FALSE
  with_box_fallback_notice <- function(expr) {
    withCallingHandlers(
      expr,
      message = function(m) {
        if (grepl("box\\.com indicates", conditionMessage(m), fixed = FALSE)) {
          if (!box_fallback_seen) {
            log_render_step(sprintf(
              "CO->PIG map not found on Box for course %s — using bundled curriculum.tag.maps fallback",
              course_id
            ))
            box_fallback_seen <<- TRUE
          }
          invokeRestart("muffleMessage")
        }
      }
    )
  }

  tag_coverage <- lapply(data_list, function(d) {
    if (is.null(d)) return(NULL)
    tryCatch(
      with_box_fallback_notice(
        event.curriculum.tags::curriculum_tag_counts_by_events(d, pool)
      ),
      error = function(e) {
        message(sprintf("tag counts failed: %s", conditionMessage(e)))
        NULL
      }
    )
  })

  # generate_tagset_plots needs every period to have data, so only call it
  # with the non-NULL subset.
  valid_periods <- names(Filter(Negate(is.null), data_list))
  tagset_plots <- if (length(valid_periods) > 0) {
    tryCatch(
      with_box_fallback_notice(
        cvm.course.portfolio::generate_tagset_plots(
          con       = pool,
          periods   = valid_periods,
          data_list = data_list[valid_periods],
          classes   = classes[valid_periods]
        )
      ),
      error = function(e) {
        message(sprintf("tagset plot generation failed: %s", conditionMessage(e)))
        NULL
      }
    )
  } else {
    NULL
  }

  # KPI summary
  kpi <- summarize_kpis(
    grades_df        = grades_df,
    tag_coverage     = tag_coverage,
    n_periods        = length(periods),
    cohort_label     = cohort_label
  )

  # Expose the RAW max period_finish_date (not the use_end_date clamp) so
  # downstream UI can compute things like "surveys available 2 days after
  # course end" without re-parsing period_rows.
  max_period_finish_date <- if (is.finite(max_finish)) as.Date(max_finish) else NA

  list(
    course_id        = course_id,
    course_code      = course_code,
    course_name      = course_name,
    cohort_label     = cohort_label,
    periods          = periods,
    period_meta      = period_meta,
    use_end_date     = use_end,
    max_period_finish_date = max_period_finish_date,
    grades_by_period = grades_by_period,
    grades_df        = grades_df,
    gradebook_ok     = gradebook_ok,
    gradebook_messages = grades_messages,
    assessment_plots = assessment_plots,
    data_list        = data_list,
    tag_coverage     = tag_coverage,
    tagset_plots     = tagset_plots,
    kpi              = kpi,
    refreshed_at     = Sys.time()
  )
}

#' Append a timestamped line to logs/render.log. Safe to call before the
#' logs dir exists — it creates it lazily.
log_render_step <- function(msg, level = "INFO", file = NULL) {
  file <- file %||% file.path(logs_dir(), "render.log")
  dir <- dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  line <- sprintf("[%s] %s  %s\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  cat(line, file = file, append = TRUE)
  invisible(NULL)
}

#' Compact per-assessment summary table for Tab 1 (period / assessment / stats).
#' Returns tibble: cperiod_id, assessment, mean, median, sd, n.
assessment_breakdown <- function(grades_df) {
  if (is.null(grades_df) || nrow(grades_df) == 0) {
    return(tibble::tibble(
      cperiod_id = character(), assessment = character(),
      mean = numeric(), median = numeric(), sd = numeric(), n = integer()
    ))
  }
  grades_df |>
    dplyr::group_by(.data$cperiod_id, .data$assessment) |>
    dplyr::summarise(
      mean   = mean(.data$grade, na.rm = TRUE),
      median = stats::median(.data$grade, na.rm = TRUE),
      sd     = stats::sd(.data$grade, na.rm = TRUE),
      n      = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$cperiod_id, .data$assessment)
}

#' Median grade trend (per-period) — feed for the plotly overlay.
median_trend <- function(grades_df) {
  if (is.null(grades_df) || nrow(grades_df) == 0) return(tibble::tibble())
  final_grades <- grades_df |>
    dplyr::filter(.data$assessment %in% c(
      "weighted_overall_grade", "individual_weighted_overall"
    ))
  df <- if (nrow(final_grades) > 0) final_grades else grades_df
  df |>
    dplyr::group_by(.data$cperiod_id) |>
    dplyr::summarise(
      median = stats::median(.data$grade, na.rm = TRUE),
      q25    = stats::quantile(.data$grade, 0.25, na.rm = TRUE),
      q75    = stats::quantile(.data$grade, 0.75, na.rm = TRUE),
      n      = dplyr::n_distinct(.data$id),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$cperiod_id)
}

# -----------------------------------------------------------------------------
# KPI hero computation
# -----------------------------------------------------------------------------
summarize_kpis <- function(grades_df, tag_coverage, n_periods, cohort_label) {
  overall_median <- if (!is.null(grades_df) && nrow(grades_df) > 0) {
    stats::median(grades_df$grade, na.rm = TRUE)
  } else NA_real_

  total_students <- if (!is.null(grades_df) && nrow(grades_df) > 0) {
    dplyr::n_distinct(grades_df$id)
  } else 0L

  # tag_coverage is keyed by PERIOD — each entry is a named list of tag-set
  # data.frames (disciplines, species, pigs, avma, epas, cbve_*, objectives).
  # Two distinct metrics:
  #   n_tag_sets         = unique tag sets present in any period (the REAL
  #                        "tag sets with data" number shown on the KPI card)
  #   periods_with_tags  = count of periods that produced non-null tag data
  non_null <- if (!is.null(tag_coverage)) {
    Filter(function(x) !is.null(x) && length(x) > 0, tag_coverage)
  } else list()

  periods_with_tags <- length(non_null)
  n_tag_sets <- if (periods_with_tags > 0) length(non_null[[1]]) else 0L

  list(
    overall_median     = overall_median,
    total_students     = total_students,
    periods_covered    = n_periods,
    n_tag_sets         = n_tag_sets,
    periods_with_tags  = periods_with_tags,
    # Legacy alias so any caller that still reads kpi$tag_sets_with_data keeps
    # working — but now it reports the CORRECT value (n_tag_sets).
    tag_sets_with_data = n_tag_sets,
    cohort_label       = cohort_label
  )
}

# -----------------------------------------------------------------------------
# Tag coverage derivations (for the "% covered" ring, bar chart, uncovered list)
# -----------------------------------------------------------------------------

#' For one tag set (data.frame from curriculum_tag_counts_by_events), compute
#' coverage stats: n total tags in the set, n covered (>0 count in any period),
#' percentage, list of uncovered tag names.
#'
#' Deprecated taxonomy entries (tags starting with "Do not use") are excluded
#' from every metric and from the uncovered list — they should not skew
#' faculty-facing coverage numbers. Their count is returned separately as
#' `$deprecated` so the UI can display a "X deprecated tags hidden" note.
tag_coverage_summary <- function(tagset_df) {
  empty <- function() list(covered = 0L, total = 0L, pct = NA_real_,
                           uncovered = character(), deprecated = 0L)
  if (is.null(tagset_df) || !is.data.frame(tagset_df) || nrow(tagset_df) == 0) {
    return(empty())
  }
  tag_cols <- setdiff(names(tagset_df), c("event_title", "class"))
  if (length(tag_cols) == 0) return(empty())

  dep_mask <- is_deprecated_tag(tag_cols)
  deprecated_count <- sum(dep_mask)
  tag_cols <- tag_cols[!dep_mask]
  if (length(tag_cols) == 0) {
    return(list(covered = 0L, total = 0L, pct = NA_real_,
                uncovered = character(), deprecated = deprecated_count))
  }

  totals <- vapply(tagset_df[tag_cols], function(x) sum(x, na.rm = TRUE), numeric(1))
  covered <- sum(totals > 0)
  total   <- length(totals)
  list(
    covered    = covered,
    total      = total,
    pct        = safe_pct(covered, total),
    uncovered  = names(totals)[totals == 0],
    deprecated = deprecated_count
  )
}
