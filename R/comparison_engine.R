# R/comparison_engine.R
# -----------------------------------------------------------------------------
# Cross-cohort comparison logic. Consumes the per-cohort pipeline outputs from
# portfolio_runner.R and produces the data structures the Tab 3 UI renders:
#   * grade_overlay_df        — long-form data for the overlay plotly
#   * tag_delta               — per-tag change between earliest/latest cohort
#   * summary_table           — one row per cohort
#   * change_narrative        — {improved, at_risk, consistent} lists
# -----------------------------------------------------------------------------

#' Grade overlay: one median per (cohort × period), in period order.
#' Each cohort line is a trend across that cohort's own periods.
build_grade_overlay <- function(cohort_results) {
  if (length(cohort_results) == 0) return(tibble::tibble())
  rows <- lapply(cohort_results, function(res) {
    trend <- median_trend(res$grades_df)
    if (nrow(trend) == 0) return(NULL)
    trend$cohort <- res$cohort_label
    trend$seq    <- seq_len(nrow(trend))  # common x-axis position per cohort
    trend
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

#' Tag delta heatmap data. For each tag set and each tag, compute the
#' earliest cohort's count vs. the latest cohort's count.
build_tag_delta <- function(cohort_results) {
  if (length(cohort_results) < 2) return(list())

  # Sort cohorts by label (chronological because "Class of 2023" < "Class of 2027")
  ordered <- cohort_results[order(vapply(cohort_results,
                                         function(x) x$cohort_label,
                                         character(1)))]
  first <- ordered[[1]]
  last  <- ordered[[length(ordered)]]

  fst_cov <- first$tag_coverage %||% list()
  lst_cov <- last$tag_coverage  %||% list()
  tagsets <- intersect(names(fst_cov), names(lst_cov))

  out <- lapply(tagsets, function(ts) {
    f <- fst_cov[[ts]]; l <- lst_cov[[ts]]
    if (is.null(f) || is.null(l)) return(NULL)
    f_tags <- setdiff(names(f), c("event_title", "class"))
    l_tags <- setdiff(names(l), c("event_title", "class"))
    all_tags <- union(f_tags, l_tags)
    # Faculty dashboards never surface retired taxonomy entries.
    all_tags <- all_tags[!is_deprecated_tag(all_tags)]
    if (length(all_tags) == 0) return(NULL)

    f_totals <- vapply(all_tags, function(t) {
      if (t %in% f_tags) sum(f[[t]], na.rm = TRUE) else 0
    }, numeric(1))
    l_totals <- vapply(all_tags, function(t) {
      if (t %in% l_tags) sum(l[[t]], na.rm = TRUE) else 0
    }, numeric(1))

    tibble::tibble(
      tag        = all_tags,
      first      = f_totals,
      last       = l_totals,
      delta      = l_totals - f_totals,
      direction  = dplyr::case_when(
        l_totals - f_totals > 0 ~ "gained",
        l_totals - f_totals < 0 ~ "lost",
        TRUE                    ~ "unchanged"
      )
    ) |> dplyr::arrange(dplyr::desc(abs(.data$delta)))
  })
  names(out) <- tagsets
  out[!vapply(out, is.null, logical(1))]
}

#' One-row-per-cohort summary with overall median and tag coverage % per set.
#' All numeric values rounded to 1 decimal — the downstream DT renderer
#' otherwise shows full-precision floats like 185.7200012207031.
build_summary_table <- function(cohort_results) {
  if (length(cohort_results) == 0) {
    return(tibble::tibble(cohort = character(), overall_median = numeric()))
  }
  rows <- lapply(cohort_results, function(res) {
    om <- res$kpi$overall_median %||% NA_real_
    row <- tibble::tibble(
      cohort         = res$cohort_label,
      overall_median = if (is.finite(om)) round(om, 1) else NA_real_,
      students       = res$kpi$total_students %||% 0L,
      periods        = res$kpi$periods_covered %||% 0L
    )
    cov <- res$tag_coverage %||% list()
    for (ts in names(cov)) {
      s <- tag_coverage_summary(cov[[ts]])
      row[[paste0("pct_", ts)]] <- if (is.finite(s$pct %||% NA_real_)) round(s$pct, 1) else NA_real_
    }
    row
  })
  dplyr::bind_rows(rows)
}

#' Narrative cards: "Most Improved", "Tags at Risk", "Consistent Strengths".
#' Picks the top N by delta across all tag sets.
change_narrative <- function(tag_delta_list, top_n = 5) {
  if (length(tag_delta_list) == 0) {
    return(list(improved = character(), at_risk = character(), consistent = character()))
  }
  all_tags <- dplyr::bind_rows(
    lapply(names(tag_delta_list), function(ts) {
      df <- tag_delta_list[[ts]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$tagset <- ts
      df
    })
  )
  if (nrow(all_tags) == 0) {
    return(list(improved = character(), at_risk = character(), consistent = character()))
  }

  fmt_tag <- function(row) sprintf("%s (%s: %+d)", row$tag, row$tagset, row$delta)

  improved <- all_tags |>
    dplyr::filter(.data$delta > 0) |>
    dplyr::arrange(dplyr::desc(.data$delta)) |>
    head(top_n)

  at_risk <- all_tags |>
    dplyr::filter(.data$delta < 0) |>
    dplyr::arrange(.data$delta) |>
    head(top_n)

  consistent <- all_tags |>
    dplyr::filter(.data$delta == 0, .data$first > 0) |>
    dplyr::arrange(dplyr::desc(.data$first)) |>
    head(top_n)

  list(
    improved   = if (nrow(improved)   > 0) apply(improved,   1, fmt_tag) else character(),
    at_risk    = if (nrow(at_risk)    > 0) apply(at_risk,    1, fmt_tag) else character(),
    consistent = if (nrow(consistent) > 0) apply(consistent, 1, fmt_tag) else character()
  )
}

# ---------------------------------------------------------------------------
# All-years analysis helpers
# ---------------------------------------------------------------------------
# Used by both the Cross-Year Comparison tab (rebuilt) AND the rule-based
# narrative (`generate_rule_based_narrative`). Shape is tidy and
# render-agnostic — DT / plotly / htmltools consume directly.

#' Per-cohort trend relative to the cohort before it.
#'
#' When the immediately preceding cohort is anomalous (overall median > 100,
#' typically caused by extra-credit inflating the weighted overall grade),
#' the raw subtraction produces an absurd delta like "-87.9". We skip past
#' any anomalous cohort when searching for a baseline and flag the anomalous
#' cohort separately with trend = "anomaly".
#'
#' @return tibble(cohort, median, prev_cohort, prev_median, delta, trend)
#'   trend ∈ {"up","stable","down","anomaly", NA}; |delta| ≤ 1 → stable.
compute_cohort_trajectory <- function(all_data) {
  if (length(all_data) == 0) {
    return(tibble::tibble(cohort = character(), median = numeric(),
                          prev_cohort = character(), prev_median = numeric(),
                          delta = numeric(), trend = character()))
  }
  ordered <- all_data[order(vapply(all_data, function(r) r$cohort_label, character(1)))]
  labels  <- vapply(ordered, function(r) r$cohort_label, character(1))
  meds    <- vapply(ordered, function(r) r$kpi$overall_median %||% NA_real_, numeric(1))

  is_valid <- is.finite(meds) & meds <= 100

  prev_lab <- rep(NA_character_, length(labels))
  prev_med <- rep(NA_real_,      length(labels))
  # For each cohort, walk backward to find the most recent VALID baseline.
  for (i in seq_along(labels)) {
    j <- i - 1L
    while (j >= 1L) {
      if (is_valid[[j]]) {
        prev_lab[[i]] <- labels[[j]]
        prev_med[[i]] <- meds[[j]]
        break
      }
      j <- j - 1L
    }
  }
  delta <- ifelse(is_valid & !is.na(prev_med), meds - prev_med, NA_real_)
  trend <- dplyr::case_when(
    !is_valid          ~ "anomaly",       # own median > 100 or non-finite
    is.na(delta)       ~ NA_character_,   # no prior baseline
    abs(delta) <= 1    ~ "stable",
    delta > 0          ~ "up",
    delta < 0          ~ "down"
  )
  tibble::tibble(
    cohort      = labels,
    median      = round(meds, 1),
    prev_cohort = prev_lab,
    prev_median = round(prev_med, 1),
    delta       = round(delta, 1),
    trend       = trend
  )
}

#' Delivery consistency = coefficient of variation of overall medians
#' across cohorts. Lower = more consistent.
#' @return list(cv, label ∈ {"High","Medium","Low"}, medians)
compute_consistency_score <- function(all_data) {
  meds <- vapply(all_data, function(r) r$kpi$overall_median %||% NA_real_, numeric(1))
  meds <- meds[is.finite(meds)]
  if (length(meds) < 2) {
    return(list(cv = NA_real_, label = "Insufficient data", medians = meds))
  }
  cv <- stats::sd(meds) / mean(meds)
  label <- if (cv < 0.03) "High"
           else if (cv < 0.08) "Medium"
           else "Low"
  list(cv = round(cv * 100, 2), label = label, medians = meds)
}

#' Per-tag stats across cohorts. Input: cohort_counts_by_ts — named list,
#' names = tag set names, values = named list of cohort_label -> named-int vec.
#' @return tibble(tagset, tag, min, max, mean, sd, n_cohorts_covered)
compute_tag_stats <- function(cohort_counts_by_ts) {
  rows <- list()
  for (ts in names(cohort_counts_by_ts)) {
    per_cohort <- cohort_counts_by_ts[[ts]]
    if (length(per_cohort) == 0) next
    all_tags <- unique(unlist(lapply(per_cohort, names)))
    # Drop deprecated tags here — mirrors the user-facing filters.
    all_tags <- all_tags[!is_deprecated_tag(all_tags)]
    for (t in all_tags) {
      vals <- vapply(per_cohort, function(cc) {
        if (t %in% names(cc)) as.numeric(cc[[t]]) else 0
      }, numeric(1))
      rows[[length(rows) + 1]] <- tibble::tibble(
        tagset = ts,
        tag    = t,
        min    = min(vals),
        max    = max(vals),
        mean   = mean(vals),
        sd     = stats::sd(vals),
        n_cohorts_covered = sum(vals > 0)
      )
    }
  }
  if (length(rows) == 0) {
    return(tibble::tibble(tagset = character(), tag = character(),
                          min = numeric(), max = numeric(),
                          mean = numeric(), sd = numeric(),
                          n_cohorts_covered = integer()))
  }
  dplyr::bind_rows(rows)
}

#' Top-K most-consistent tags = highest minimum coverage count across cohorts
#' (tag is covered in every cohort and well covered).
top_consistent_tags <- function(tag_stats, k = 5) {
  if (nrow(tag_stats) == 0) return(tag_stats)
  tag_stats |>
    dplyr::filter(.data$min > 0) |>
    dplyr::arrange(dplyr::desc(.data$min), dplyr::desc(.data$mean)) |>
    head(k)
}

#' Top-K most-variable tags = highest SD of coverage count (curriculum gap
#' candidate — delivered consistently across cohorts is NOT true).
top_variable_tags <- function(tag_stats, k = 5) {
  if (nrow(tag_stats) == 0) return(tag_stats)
  tag_stats |>
    dplyr::filter(.data$sd > 0) |>
    dplyr::arrange(dplyr::desc(.data$sd), dplyr::desc(.data$max - .data$min)) |>
    head(k)
}

#' Students per cohort — for the "volume trend" bar chart.
#' @return tibble(cohort, students), alphabetic (chronological) order
compute_student_volume <- function(all_data) {
  if (length(all_data) == 0) {
    return(tibble::tibble(cohort = character(), students = integer()))
  }
  ordered <- all_data[order(vapply(all_data, function(r) r$cohort_label, character(1)))]
  tibble::tibble(
    cohort   = vapply(ordered, function(r) r$cohort_label, character(1)),
    students = vapply(ordered, function(r) as.integer(r$kpi$total_students %||% 0L), integer(1))
  )
}

#' Per-assessment trend: for each assessment appearing in ≥ min_cohorts,
#' compute the mean across cohorts (first vs. last) and flag declines > 5pts.
#' @return tibble(assessment, n_cohorts, first_mean, last_mean, delta, flag)
compute_assessment_trends <- function(all_data, min_cohorts = 3) {
  if (length(all_data) == 0) {
    return(tibble::tibble(assessment = character(), n_cohorts = integer(),
                          first_mean = numeric(), last_mean = numeric(),
                          delta = numeric(), flag = character()))
  }
  ordered <- all_data[order(vapply(all_data, function(r) r$cohort_label, character(1)))]
  rows <- dplyr::bind_rows(lapply(names(ordered), function(lab) {
    r <- ordered[[lab]]
    if (is.null(r$grades_df) || nrow(r$grades_df) == 0) return(NULL)
    r$grades_df |>
      dplyr::filter(!.data$assessment %in% c("weighted_overall_grade",
                                             "individual_weighted_overall")) |>
      dplyr::group_by(.data$assessment) |>
      dplyr::summarise(cohort_mean = mean(.data$grade, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(cohort = lab, .before = 1)
  }))
  if (nrow(rows) == 0) {
    return(tibble::tibble(assessment = character(), n_cohorts = integer(),
                          first_mean = numeric(), last_mean = numeric(),
                          delta = numeric(), flag = character()))
  }
  # Preserve alphabetical cohort order for first/last identification.
  rows$cohort <- factor(rows$cohort, levels = sort(unique(rows$cohort)))
  rows |>
    dplyr::arrange(.data$assessment, .data$cohort) |>
    dplyr::group_by(.data$assessment) |>
    dplyr::summarise(
      n_cohorts  = dplyr::n(),
      first_mean = dplyr::first(.data$cohort_mean),
      last_mean  = dplyr::last(.data$cohort_mean),
      .groups    = "drop"
    ) |>
    dplyr::filter(.data$n_cohorts >= min_cohorts) |>
    dplyr::mutate(
      delta = round(.data$last_mean - .data$first_mean, 1),
      first_mean = round(.data$first_mean, 1),
      last_mean  = round(.data$last_mean, 1),
      flag = dplyr::case_when(
        .data$delta <= -5 ~ "declining",
        .data$delta >=  5 ~ "improving",
        TRUE              ~ "stable"
      )
    ) |>
    dplyr::arrange(.data$delta)   # declining first
}
