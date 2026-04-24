# R/narrative.R
# -----------------------------------------------------------------------------
# Course narrative generation via cvm.openAI.
#
# Source verification (cvm.openAI_0.0.1):
#   * generate_string(vec) — flattens a vector with ";".
#   * openai_summary(str, gpt_model = "gpt-3.5-turbo-16k", prompt = "")
#     Calls openai::create_chat_completion which reads OPENAI_API_KEY from env.
#     Returns a PLAIN STRING (character scalar). No structured output.
#
# Strategy:
#   To return { narrative, flags, questions } we prompt the model to emit a
#   single JSON blob and parse it with jsonlite. If parsing fails, we fall
#   back to treating the whole response as the narrative with empty flags/qs.
# -----------------------------------------------------------------------------

OPENAI_DEFAULT_MODEL <- Sys.getenv("CVM_OPENAI_MODEL", unset = "gpt-4o-mini")

#' Check whether an OpenAI key is configured. Used by the app to decide
#' whether to call the model or render a "set your key" placeholder.
openai_available <- function() {
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  nzchar(key)
}

#' Build the structured-summary text passed to the LLM. Keeps token usage
#' predictable by summarising rather than dumping raw data.
build_data_summary <- function(course_name,
                               course_code,
                               periods,
                               grade_summary,
                               tag_coverage_list,
                               comparison_data = NULL) {

  grade_block <- if (!is.null(grade_summary) && nrow(grade_summary) > 0) {
    lines <- apply(grade_summary, 1, function(r) {
      sprintf(
        "- Period %s: mean=%s, median=%s, sd=%s, n=%s",
        r[["cperiod_id"]] %||% r[["period"]] %||% "?",
        fmt_num(as.numeric(r[["mean"]])),
        fmt_num(as.numeric(r[["median"]])),
        fmt_num(as.numeric(r[["sd"]])),
        r[["n"]]
      )
    })
    paste(lines, collapse = "\n")
  } else {
    "- No gradebook data available."
  }

  tag_block <- if (!is.null(tag_coverage_list) && length(tag_coverage_list) > 0) {
    parts <- lapply(names(tag_coverage_list), function(nm) {
      s <- tag_coverage_summary(tag_coverage_list[[nm]])
      sprintf("- %s: %s of %s tags covered (%s%%), %s uncovered",
              nm,
              s$covered %||% 0,
              s$total %||% 0,
              ifelse(is.na(s$pct), "n/a", format(s$pct)),
              length(s$uncovered %||% character()))
    })
    paste(parts, collapse = "\n")
  } else {
    "- No tag coverage data available."
  }

  cmp_block <- if (!is.null(comparison_data) && length(comparison_data) > 0) {
    paste0(
      "\nCross-cohort comparison data is attached. Use it to comment on ",
      "improvement, stability, or decline across graduating classes.\n"
    )
  } else ""

  paste0(
    "Course: ", course_code, " — ", course_name, "\n",
    "Periods included: ", paste(periods, collapse = ", "), "\n\n",
    "Gradebook summary:\n", grade_block, "\n\n",
    "Curriculum tag coverage:\n", tag_block, "\n",
    cmp_block
  )
}

NARRATIVE_PROMPT_INSTRUCTIONS <- paste(
  "You are a curriculum analyst writing for the CVM Curriculum Evaluation",
  "Committee (an AVMA-accredited DVM program). Produce a professional,",
  "policy-brief interpretation of the course data that follows.",
  "",
  "Return ONLY a single JSON object with EXACTLY these keys:",
  "  narrative  : string — 4 to 6 markdown paragraphs. Plain prose, no",
  "               headings, no bullet lists. Focus on: overall performance,",
  "               curriculum tag coverage strengths/gaps, trends across",
  "               periods, observable patterns. Do not invent numbers.",
  "  flags      : array of objects {issue, severity, recommendation}.",
  "               severity is one of 'high', 'medium', 'low'.",
  "               Only include flags supported by the data provided.",
  "               May be empty.",
  "  questions  : array of EXACTLY 5 strings — discussion questions for the",
  "               course director, surfacing ambiguity or follow-ups.",
  "",
  "Do NOT wrap the JSON in markdown fences. Do NOT add commentary.",
  "The JSON must parse with jsonlite::fromJSON.",
  sep = "\n"
)

#' Generate a structured course narrative.
#'
#' @param course_name        character
#' @param course_code        character
#' @param periods            character/integer vector of cperiod_id values
#' @param grade_summary      data.frame with columns cperiod_id, mean, median, sd, n
#' @param tag_coverage_list  named list of tag-coverage data.frames (Disciplines, Species, ...)
#' @param comparison_data    optional list of same shape for another cohort
#' @param model              OpenAI model name (defaults to env or "gpt-4o-mini")
#'
#' @return list with $narrative (string), $flags (data.frame), $questions (character vector).
#'   When no API key is set, returns a placeholder with $available = FALSE.
generate_course_narrative <- function(course_name,
                                      course_code,
                                      periods,
                                      grade_summary,
                                      tag_coverage_list,
                                      comparison_data = NULL,
                                      model = OPENAI_DEFAULT_MODEL) {

  if (!openai_available()) {
    return(list(
      available = FALSE,
      narrative = paste(
        "OpenAI API key not configured. Set OPENAI_API_KEY in your",
        ".Renviron file (see README for instructions), then click Regenerate",
        "to produce the narrative interpretation for this course."
      ),
      flags = empty_flags_df(),
      questions = character(0),
      generated_at = NULL,
      model = NULL,
      raw = NULL
    ))
  }

  data_summary <- build_data_summary(
    course_name       = course_name,
    course_code       = course_code,
    periods           = periods,
    grade_summary     = grade_summary,
    tag_coverage_list = tag_coverage_list,
    comparison_data   = comparison_data
  )

  prompt <- paste0(NARRATIVE_PROMPT_INSTRUCTIONS,
                   "\n\nCourse data:\n",
                   data_summary)

  # openai_summary concatenates prompt + str with a space; we pass "" as str.
  raw <- tryCatch(
    cvm.openAI::openai_summary(str = "", gpt_model = model, prompt = prompt),
    error = function(e) {
      message("OpenAI call failed: ", conditionMessage(e))
      NA_character_
    }
  )

  parsed <- parse_narrative_response(raw)

  list(
    available   = TRUE,
    narrative   = parsed$narrative,
    flags       = parsed$flags,
    questions   = parsed$questions,
    generated_at = Sys.time(),
    model       = model,
    raw         = raw
  )
}

empty_flags_df <- function() {
  tibble::tibble(
    issue = character(0),
    severity = character(0),
    recommendation = character(0)
  )
}

#' Parse the model's JSON response into narrative/flags/questions.
#' Tolerates: plain text (falls back to narrative only), markdown-fenced JSON,
#' and a "flags": [] edge case where the array is empty.
parse_narrative_response <- function(raw) {
  if (is.null(raw) || is.na(raw) || !nzchar(raw)) {
    return(list(
      narrative = "The model returned no response. Please try Regenerate.",
      flags = empty_flags_df(),
      questions = character(0)
    ))
  }

  # Strip ```json ... ``` fences if the model added them despite instructions.
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(raw))

  obj <- tryCatch(jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
                  error = function(e) NULL)

  if (is.null(obj)) {
    # Parsing failed. Treat raw as narrative.
    return(list(
      narrative = raw,
      flags = empty_flags_df(),
      questions = character(0)
    ))
  }

  narrative <- obj$narrative %||% ""
  if (!is.character(narrative)) narrative <- paste(unlist(narrative), collapse = "\n\n")

  flags_raw <- obj$flags %||% list()
  flags_df <- if (length(flags_raw) > 0) {
    tibble::tibble(
      issue          = vapply(flags_raw, function(x) as.character(x$issue %||% ""), character(1)),
      severity       = vapply(flags_raw, function(x) as.character(x$severity %||% "medium"), character(1)),
      recommendation = vapply(flags_raw, function(x) as.character(x$recommendation %||% ""), character(1))
    )
  } else empty_flags_df()

  questions <- obj$questions %||% list()
  questions <- vapply(questions, function(q) as.character(q), character(1))

  list(
    narrative = narrative,
    flags = flags_df,
    questions = questions
  )
}

# ---------------------------------------------------------------------------
# Rule-based narrative
# ---------------------------------------------------------------------------
# Generated entirely from the data — no LLM dependency. Triggered when
# --skip-narrative is passed (or when OpenAI isn't configured).
# Returns the SAME shape as generate_course_narrative() so the template can
# render it interchangeably, plus an extra $source flag so the UI can show
# a "Generated from data" badge instead of the "…using gpt-4o-mini" credit.
#
# Policy-brief structure, five paragraphs per spec:
#   1. Opening       — scope + volume + range of medians
#   2. Performance   — best/worst cohort + variability + scale anomalies
#   3. Coverage      — average per tag set, flag < 50%
#   4. Trends        — consistent vs variable tags + gained/lost coverage
#   5. Closing       — as-of date + limitations
generate_rule_based_narrative <- function(all_data, cohort_colors = NULL,
                                          render_ts = NULL) {
  if (length(all_data) == 0) {
    return(list(
      available    = TRUE,
      source       = "rule-based",
      narrative    = paste(
        "No cohort data is available for this course yet.",
        "Once a cohort completes its first period and gradebook data is",
        "pulled, this narrative will populate automatically."
      ),
      flags        = empty_flags_df(),
      questions    = character(0),
      generated_at = Sys.time(),
      model        = "rule-based"
    ))
  }

  cohort_labels <- vapply(all_data, function(r) r$cohort_label, character(1))
  years         <- sub("^Class of ", "", cohort_labels)
  years_num     <- suppressWarnings(as.integer(years))
  years_sorted  <- sort(stats::na.omit(years_num))
  first_year    <- if (length(years_sorted) > 0) min(years_sorted) else NA_integer_
  last_year     <- if (length(years_sorted) > 0) max(years_sorted) else NA_integer_

  total_students <- sum(vapply(all_data, function(r) {
    as.integer(r$kpi$total_students %||% 0L)
  }, integer(1)))

  meds <- vapply(all_data, function(r) r$kpi$overall_median %||% NA_real_, numeric(1))
  meds_ok <- meds[is.finite(meds)]

  course_code <- all_data[[length(all_data)]]$course_code
  course_name <- all_data[[length(all_data)]]$course_name

  # ---- Paragraph 1 — opening ---------------------------------------------
  p1 <- if (length(meds_ok) == 0) {
    sprintf(
      "%s %s has been delivered to %d cohort%s between %s and %s, with a total of %s students assessed. Gradebook data was not available, so overall median grades cannot be reported in this narrative.",
      course_code, course_name,
      length(all_data), if (length(all_data) == 1) "" else "s",
      first_year %||% "unknown", last_year %||% "unknown",
      format(total_students, big.mark = ",")
    )
  } else {
    sprintf(
      "%s %s has been delivered to %d cohort%s between %s and %s, with a total of %s students assessed. Overall median grades have ranged from %.1f to %.1f across cohorts.",
      course_code, course_name,
      length(all_data), if (length(all_data) == 1) "" else "s",
      first_year %||% "unknown", last_year %||% "unknown",
      format(total_students, big.mark = ","),
      min(meds_ok), max(meds_ok)
    )
  }

  # ---- Paragraph 2 — performance -----------------------------------------
  flags_list <- list()
  add_flag <- function(issue, severity, recommendation) {
    flags_list[[length(flags_list) + 1]] <<- list(
      issue = issue, severity = severity, recommendation = recommendation
    )
  }

  consistency <- compute_consistency_score(all_data)
  traj <- compute_cohort_trajectory(all_data)

  p2 <- if (length(meds_ok) < 2) {
    "Only one cohort has complete gradebook data; cross-cohort performance analysis will become available once additional cohorts complete."
  } else {
    best_idx  <- which.max(meds)
    worst_idx <- which.min(meds)
    best_lab  <- cohort_labels[[best_idx]]
    worst_lab <- cohort_labels[[worst_idx]]
    low_flag <- any(meds_ok < 85)
    hi_flag  <- any(meds_ok > 100)

    parts <- c(
      sprintf("The highest-performing cohort is %s (median %.1f); the lowest is %s (median %.1f).",
              best_lab, meds[[best_idx]], worst_lab, meds[[worst_idx]]),
      sprintf("Delivery consistency across cohorts is %s (coefficient of variation: %.2f%%).",
              tolower(consistency$label), consistency$cv %||% NA)
    )
    if (low_flag) {
      below <- cohort_labels[which(meds < 85)]
      parts <- c(parts, sprintf(
        "Cohort%s %s fell below the 85%% median benchmark and warrants review.",
        if (length(below) == 1) "" else "s",
        paste(below, collapse = ", ")
      ))
      add_flag(
        sprintf("Overall median below 85%% for: %s",
                paste(below, collapse = ", ")),
        "high",
        "Investigate the gradebook composition and assessment difficulty for these cohorts."
      )
    }
    if (hi_flag) {
      above <- cohort_labels[which(meds > 100)]
      parts <- c(parts, sprintf(
        "Cohort%s %s show%s an overall median above 100%% — likely caused by extra-credit inflating the weighted overall grade.",
        if (length(above) == 1) "" else "s",
        paste(above, collapse = ", "),
        if (length(above) == 1) "s" else ""
      ))
      add_flag(
        sprintf("Scale anomaly: overall median > 100%% for %s",
                paste(above, collapse = ", ")),
        "medium",
        "Audit extra-credit allocations and weighting for these cohorts."
      )
    }
    paste(parts, collapse = " ")
  }

  # ---- Paragraph 3 — coverage --------------------------------------------
  # Build per-tag-set average coverage %.
  ts_names <- if (!is.null(all_data[[1]]$tag_coverage) &&
                  length(all_data[[1]]$tag_coverage) > 0) {
    names(all_data[[1]]$tag_coverage[[1]] %||% list())
  } else character(0)

  ts_avg <- vapply(ts_names, function(ts) {
    pcts <- vapply(all_data, function(r) {
      if (is.null(r$tag_coverage) || length(r$tag_coverage) == 0) return(NA_real_)
      merged <- dplyr::bind_rows(Filter(Negate(is.null),
                                        lapply(r$tag_coverage, function(x) x[[ts]])))
      s <- tag_coverage_summary(merged)
      if (is.null(s$pct) || is.na(s$pct)) NA_real_ else as.numeric(s$pct)
    }, numeric(1))
    mean(pcts, na.rm = TRUE)
  }, numeric(1))

  p3 <- if (length(ts_avg) == 0 || all(is.na(ts_avg))) {
    "Curriculum tag coverage data was not available for this course."
  } else {
    pretty_ts <- if (exists("pretty_tagset_name", mode = "function")) {
      pretty_tagset_name(names(ts_avg))
    } else names(ts_avg)
    above <- ts_avg[!is.na(ts_avg) & ts_avg >= 50]
    below <- ts_avg[!is.na(ts_avg) & ts_avg <  50]
    parts <- c(sprintf(
      "Average coverage across cohorts ranges from %.1f%% (%s) to %.1f%% (%s).",
      min(ts_avg, na.rm = TRUE), pretty_ts[which.min(ts_avg)],
      max(ts_avg, na.rm = TRUE), pretty_ts[which.max(ts_avg)]
    ))
    if (length(below) > 0) {
      below_names <- pretty_ts[which(!is.na(ts_avg) & ts_avg < 50)]
      parts <- c(parts, sprintf(
        "%s below 50%% average coverage and may need curriculum mapping review: %s.",
        if (length(below_names) == 1) "This tag set is" else "These tag sets are",
        paste(below_names, collapse = ", ")
      ))
      add_flag(
        sprintf("Tag sets with < 50%% average coverage: %s",
                paste(below_names, collapse = ", ")),
        "medium",
        "Schedule a curriculum mapping session to confirm which course objectives should tag these taxonomies."
      )
    }
    paste(parts, collapse = " ")
  }

  # ---- Paragraph 4 — trends ----------------------------------------------
  cohort_counts_by_ts <- stats::setNames(
    lapply(ts_names, function(ts) {
      stats::setNames(
        lapply(all_data, function(r) {
          if (is.null(r$tag_coverage) || length(r$tag_coverage) == 0) return(integer(0))
          merged <- dplyr::bind_rows(Filter(Negate(is.null),
                                            lapply(r$tag_coverage, function(x) x[[ts]])))
          if (exists("tagset_counts_vec", mode = "function")) {
            tagset_counts_vec(merged)
          } else integer(0)
        }),
        vapply(all_data, function(r) r$cohort_label, character(1))
      )
    }),
    ts_names
  )
  tag_stats <- compute_tag_stats(cohort_counts_by_ts)
  td <- build_tag_delta(all_data)

  p4 <- if (nrow(tag_stats) == 0) {
    "Trend analysis requires tag-coverage data across cohorts and could not be computed."
  } else {
    top_c <- top_consistent_tags(tag_stats, 3)
    top_v <- top_variable_tags(tag_stats, 3)
    parts <- c()
    if (nrow(top_c) > 0) {
      parts <- c(parts, sprintf(
        "The most consistently covered tags across cohorts are %s — reliably present in every cohort.",
        paste(sprintf("%s (%s)", top_c$tag, top_c$tagset), collapse = "; ")
      ))
    }
    if (nrow(top_v) > 0) {
      parts <- c(parts, sprintf(
        "The most variable tags are %s — coverage fluctuates between cohorts and may indicate inconsistent curriculum mapping.",
        paste(sprintf("%s (%s)", top_v$tag, top_v$tagset), collapse = "; ")
      ))
    }
    if (length(td) > 0) {
      all_deltas <- dplyr::bind_rows(lapply(names(td), function(ts) {
        d <- td[[ts]]; if (is.null(d) || nrow(d) == 0) return(NULL); d$tagset <- ts; d
      }))
      if (nrow(all_deltas) > 0) {
        gained <- all_deltas |> dplyr::filter(.data$delta > 0) |>
                   dplyr::arrange(dplyr::desc(.data$delta)) |> head(3)
        lost   <- all_deltas |> dplyr::filter(.data$delta < 0) |>
                   dplyr::arrange(.data$delta)                |> head(3)
        if (nrow(gained) > 0) {
          parts <- c(parts, sprintf(
            "Tags with the largest gain between the earliest and latest cohort: %s.",
            paste(sprintf("%s (%+d)", gained$tag, gained$delta), collapse = "; ")
          ))
        }
        if (nrow(lost) > 0) {
          parts <- c(parts, sprintf(
            "Tags with the largest drop: %s.",
            paste(sprintf("%s (%+d)", lost$tag, lost$delta), collapse = "; ")
          ))
          add_flag(
            sprintf("Coverage drop between cohorts for: %s",
                    paste(lost$tag[1:min(3, nrow(lost))], collapse = ", ")),
            "medium",
            "Confirm these tags are still being mapped to sessions or events in the newer cohort."
          )
        }
      }
    }
    paste(parts, collapse = " ")
  }

  # ---- Paragraph 5 — closing --------------------------------------------
  ts_str <- render_ts %||% format(Sys.time(), "%Y-%m-%d %H:%M")
  p5 <- sprintf(
    "This dashboard reflects data as of %s. Narrative is generated from the data and does not substitute for faculty review.",
    ts_str
  )

  # ---- Assemble ---------------------------------------------------------
  narrative_md <- paste(p1, p2, p3, p4, p5, sep = "\n\n")

  flags_df <- if (length(flags_list) > 0) {
    tibble::tibble(
      issue          = vapply(flags_list, function(x) x$issue,          character(1)),
      severity       = vapply(flags_list, function(x) x$severity,       character(1)),
      recommendation = vapply(flags_list, function(x) x$recommendation, character(1))
    )
  } else empty_flags_df()

  # Auto-generate a handful of data-rooted questions for the course director.
  questions <- character()
  if (length(meds_ok) >= 2) {
    best_lab <- cohort_labels[which.max(meds)]
    questions <- c(questions, sprintf(
      "%s has the highest median. What pedagogical or scheduling choices differed for that cohort?",
      best_lab
    ))
  }
  if (length(below <- meds_ok[meds_ok < 85])) {
    questions <- c(questions,
      "Which assessments in the lower-performing cohorts were most responsible for dragging the median below 85%?")
  }
  if (nrow(tag_stats) > 0) {
    top_v <- top_variable_tags(tag_stats, 1)
    if (nrow(top_v) > 0) {
      questions <- c(questions, sprintf(
        "Coverage of '%s' varies widely across cohorts. Is this intentional, or does the curriculum guide need updating?",
        top_v$tag[[1]]
      ))
    }
  }
  if (length(ts_avg) > 0 && any(ts_avg < 50, na.rm = TRUE)) {
    questions <- c(questions,
      "Tag sets with low average coverage — are these tags simply not relevant for this course, or is it a mapping gap?")
  }
  questions <- c(questions,
    "Are there gradebook or taxonomy changes planned that would affect how this course is reported next year?")
  questions <- utils::head(questions, 5)

  list(
    available    = TRUE,
    source       = "rule-based",
    narrative    = narrative_md,
    flags        = flags_df,
    questions    = questions,
    generated_at = Sys.time(),
    model        = "rule-based"
  )
}
