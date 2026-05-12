# scripts/extract_survey_responses.R
# Course master list for the core VETM curriculum used by the survey-response
# extraction pipeline. Derived from data/course_ids/*.csv. SEL electives,
# TBL development, orientation, vacation, and clinical rotations (VETM 830+)
# are intentionally excluded.

courses <- tibble::tribble(
  ~course_id, ~course_code,  ~course_name,                                    ~keyword,
  5L,         "VETM 801",    "Foundations",                                   "801",
  35L,        "VETM 802A",   "Professional Skills",                           "802",
  40L,        "VETM 802B",   "Professional Skills - B",                       "802",
  44L,        "VETM 802C",   "Professional Skills - C",                       "802",
  48L,        "VETM 802D",   "Professional Skills - D",                       "802",
  37L,        "VETM 803A",   "Clinical Skills",                               "803",
  41L,        "VETM 803B",   "Clinical Skills - B",                           "803",
  45L,        "VETM 803C",   "Clinical Skills - C",                           "803",
  49L,        "VETM 803D",   "Clinical Skills - D",                           "803",
  38L,        "VETM 804A",   "Clinical Logic in Doctoring",                   "804",
  42L,        "VETM 804B",   "Clinical Logic in Doctoring - B",               "804",
  46L,        "VETM 804C",   "Clinical Logic in Doctoring - C",               "804",
  50L,        "VETM 804D",   "Clinical Logic in Doctoring - D",               "804",
  61L,        "VETM 805A",   "One Health in One World",                       "805",
  62L,        "VETM 805B",   "One Health in One World - B",                   "805",
  64L,        "VETM 805C",   "One Health in One World - C",                   "805",
  65L,        "VETM 805D",   "One Health in One World - D",                   "805",
  8L,         "VETM 807",    "Musculoskeletal",                               "807",
  11L,        "VETM 808",    "Vital Circuitry",                               "808",
  10L,        "VETM 809",    "Gastrointestinal",                              "809",
  12L,        "VETM 810",    "The Cycle of Life",                             "810",
  9L,         "VETM 811",    "Neurobiology and Behavior",                     "811",
  13L,        "VETM 812",    "The Sum of the Parts",                          "812",
  14L,        "VETM 813",    "Companion Animal: Advanced Clinical Management","813",
  53L,        "VETM 814A",   "Anesthesia and Surgery",                        "814",
  55L,        "VETM 814B",   "Anesthesia and Surgery - B",                    "814",
  52L,        "VETM 815A",   "Advanced Professional Skills",                  "815",
  54L,        "VETM 815B",   "Advanced Professional Skills - B",              "815",
  15L,        "VETM 816",    "Large Animal: Advanced Clinical Management",    "816",
  16L,        "VETM 817",    "Selectives",                                    "817"
)

# Derive folder name from course_code + course_name so it matches the on-disk
# outputs/ directory naming convention (e.g., "VETM 801" + "Foundations" ->
# "VETM801_Foundations"). Trailing "_" stripped for course_names that end in
# punctuation. Mutate in place — courses now also has $folder.
courses <- courses |>
  dplyr::mutate(
    folder = paste0(
      gsub(" ", "", course_code), "_",
      gsub("[^A-Za-z0-9]+", "_", course_name) |>
        stringr::str_replace("_$", "")
    )
  )

# =============================================================================
# extract_survey_responses.R
# Pulls EOC survey responses from Elentra via VMH and saves dashboard CSVs.
# Run weekly via scripts/weekly_refresh.R after dashboard renders.
# Add a new row to cohort_map each year when a new class starts.
# =============================================================================

# `elentra_data_retrieval.R` is structured as a standalone "full run"
# script: function definitions are interleaved with top-level code that
# pulls every report type and then library(gridExtra)s. Sourcing it would
# execute that entire pipeline (and crash on missing gridExtra). Instead
# we parse the file and evaluate ONLY top-level function assignments —
# i.e. `name <- function(...) {...}` — into the global env. Functions
# inside that script use namespaced calls (vmh::, DBI::, tibble::) so
# they work fine without the script's own library() bootstrap.
load_helper_funcs <- function(path) {
  exprs <- parse(path, keep.source = FALSE)
  is_func_assign <- function(e) {
    if (!is.call(e) || length(e) < 3) return(FALSE)
    op <- tryCatch(as.character(e[[1]]), error = function(e) "")
    if (!op %in% c("<-", "=", "<<-")) return(FALSE)
    rhs <- e[[3]]
    if (!is.call(rhs)) return(FALSE)
    rhs_op <- tryCatch(as.character(rhs[[1]]), error = function(e) "")
    identical(rhs_op, "function")
  }
  for (e in as.list(exprs)) {
    if (is_func_assign(e)) eval(e, envir = globalenv())
  }
}
load_helper_funcs(here::here("scripts", "elentra_data_retrieval.R"))

library(dplyr)
library(tibble)
library(readr)
library(here)

# ── Cohort map — add one row per year as new classes start ─────────────────
cohort_map <- tibble::tribble(
  ~date_from,    ~date_to,      ~cohort,
  "2021-08-01",  "2022-07-31",  "Class of 2023",
  "2022-08-01",  "2023-07-31",  "Class of 2024",
  "2023-08-01",  "2024-07-31",  "Class of 2025",
  "2024-08-01",  "2025-07-31",  "Class of 2026",
  "2025-08-01",  "2026-07-31",  "Class of 2027",
  "2026-08-01",  "2027-07-31",  "Class of 2028"
)

# ── Transform raw responses to dashboard CSV format ────────────────────────
transform_to_dashboard_csv <- function(df_responses, cohort_map) {
  if (nrow(df_responses) == 0) return(tibble::tibble())
  df_responses |>
    dplyr::filter(!is.na(response_text), !is.na(question_text)) |>
    dplyr::mutate(submitted_date = as.Date(submitted_at)) |>
    dplyr::rowwise() |>
    dplyr::mutate(cohort = {
      m <- cohort_map |>
        dplyr::filter(
          submitted_date >= as.Date(date_from),
          submitted_date <= as.Date(date_to)
        )
      if (nrow(m) > 0) m$cohort[1] else NA_character_
    }) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(cohort)) |>
    dplyr::group_by(cohort, question_text, response_text) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::rename(
      question        = question_text,
      response_option = response_text
    )
}

# ── Main extraction loop ───────────────────────────────────────────────────
extract_all_survey_responses <- function() {
  cat("============================================================\n")
  cat("  EXTRACT SURVEY RESPONSES — ALL COURSES\n")
  cat("  Run date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("============================================================\n\n")

  em <- open_elentra_me()
  if (is.null(em)) stop("Failed to connect to elentra_me. Check VPN and credentials.")
  on.exit(vmh::stop_connection(em), add = TRUE)

  for (i in seq_len(nrow(courses))) {
    course_code <- courses$course_code[i]
    course_name <- courses$course_name[i]
    keyword     <- courses$keyword[i]
    folder      <- courses$folder[i]

    cat(sprintf(">> %s %s\n", course_code, course_name))

    # Find EOC survey forms for this course
    forms <- tryCatch(
      find_forms(em, keyword),
      error = function(e) { tibble::tibble() }
    )

    eoc_forms <- forms |>
      dplyr::filter(
        grepl("end of course|course eval|survey", title, ignore.case = TRUE)
      )

    if (nrow(eoc_forms) == 0) {
      cat(sprintf("   No EOC forms found — skipping.\n\n"))
      next
    }

    cat(sprintf("   Found %d form(s): %s\n",
                nrow(eoc_forms), paste(eoc_forms$title, collapse = ", ")))

    # Pull responses from all matching forms
    all_responses <- dplyr::bind_rows(lapply(eoc_forms$form_id, function(fid) {
      tryCatch(
        get_form_responses(em, form_id = fid),
        error = function(e) tibble::tibble()
      )
    }))

    if (nrow(all_responses) == 0) {
      cat("   No responses found — skipping.\n\n")
      next
    }

    cat(sprintf("   %d raw response rows.\n", nrow(all_responses)))

    dashboard_csv <- transform_to_dashboard_csv(all_responses, cohort_map)

    if (nrow(dashboard_csv) == 0) {
      cat("   No responses matched cohort map — skipping.\n\n")
      next
    }

    cohorts_found <- unique(dashboard_csv$cohort)
    cat(sprintf("   %d rows across: %s\n",
                nrow(dashboard_csv), paste(sort(cohorts_found), collapse = ", ")))

    # Output path
    out_dir <- here::here("outputs", folder, "data", "survey_responses")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, sprintf("%s.csv", gsub(" ", "", course_code)))

    # Merge with existing CSV — preserve cohorts not in this run
    if (file.exists(out_path)) {
      existing <- readr::read_csv(out_path, show_col_types = FALSE)
      existing_other <- existing |> dplyr::filter(!cohort %in% cohorts_found)
      dashboard_csv <- dplyr::bind_rows(existing_other, dashboard_csv) |>
        dplyr::arrange(cohort, question, response_option)
      cat(sprintf("   Merged with existing (%d prior cohorts preserved).\n",
                  dplyr::n_distinct(existing_other$cohort)))
    }

    readr::write_csv(dashboard_csv, out_path)
    cat(sprintf("   Saved -> %s\n\n", out_path))
  }

  cat("============================================================\n")
  cat("  DONE\n")
  cat("============================================================\n")
}

# Run if called directly (not when sourced)
if (!interactive()) {
  extract_all_survey_responses()
}
