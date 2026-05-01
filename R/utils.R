# R/utils.R
# -----------------------------------------------------------------------------
# Shared helpers: cohort gating, date/label formatting, small purity helpers.
# No side effects, no I/O that isn't explicit. Loaded by every other R file.
# -----------------------------------------------------------------------------

# CVM brand palette (mirrors www/custom.css variables so R plots match the UI)
CVM_PALETTE <- list(
  navy       = "#003F6B",
  navy_dark  = "#002D4D",
  gold       = "#AB8B00",
  gold_light = "#D4B73E",
  bg         = "#F5F6FA",
  white      = "#FFFFFF",
  text       = "#1A1A2E",
  muted      = "#5A6472",
  success    = "#2E7D32",
  warning    = "#F57C00",
  alert      = "#C62828"
)

# Multi-cohort discrete palette (navy / gold / supporting hues).
# Legacy: used by comparison_engine.R's overlay plots.
CVM_COHORT_COLORS <- c(
  "#003F6B", "#AB8B00", "#6A3D9A", "#2E7D32",
  "#1F78B4", "#F57C00", "#C62828", "#5A6472"
)

# -----------------------------------------------------------------------------
# All-years dashboard cohort palette
# -----------------------------------------------------------------------------
# 12 accessible hues — each ≥ 4.5:1 contrast against white and against navy.
# Deliberately ordered so the earliest cohort in any selection gets brand navy,
# the next gets a distinct supporting hue, etc. Do NOT reorder — the first
# color must stay #003F6B (brand anchor) and downstream cycles depend on
# modulo-12 indexing.
CVM_COHORT_PALETTE <- c(
  "#003F6B",  # navy
  "#2E7D32",  # green
  "#F57C00",  # amber
  "#6A1B9A",  # purple
  "#00838F",  # teal
  "#C62828",  # red
  "#1565C0",  # blue
  "#558B2F",  # olive
  "#E65100",  # deep orange
  "#283593",  # indigo
  "#00695C",  # dark teal
  "#AD1457"   # pink
)

#' Assign colors to cohort labels by chronological order (alphabetic sort on
#' "Class of YYYY" is chronological as long as years stay 4-digit).
#' @param cohort_labels character vector — e.g. c("Class of 2023", "Class of 2026")
#' @return named character vector mapping label -> hex color. Names are the
#'   sorted input. Colors cycle via modulo-12 indexing if more than 12 cohorts
#'   ever exist, so new classes auto-assign with zero code changes.
#'
#' Contract (covered by delivery check):
#'   * sort(input)[1] -> CVM_COHORT_PALETTE[1] ("#003F6B")
#'   * 13th sorted label -> CVM_COHORT_PALETTE[1] (cycle)
assign_cohort_colors <- function(cohort_labels) {
  if (length(cohort_labels) == 0) return(stats::setNames(character(0), character(0)))
  sorted <- sort(unique(cohort_labels))
  n <- length(sorted)
  idx <- ((seq_len(n) - 1L) %% length(CVM_COHORT_PALETTE)) + 1L
  stats::setNames(CVM_COHORT_PALETTE[idx], sorted)
}

# -----------------------------------------------------------------------------
# Cohort gating
# -----------------------------------------------------------------------------
# Class-of-YYYY → first day the class begins the curriculum.
# Classes do not appear in the UI until Sys.Date() reaches their start date.
# When 2026-08-01 arrives, Class of 2029 activates with no code change.
COHORT_START_DATES <- list(
  "Class of 2023" = as.Date("2020-08-01"),
  "Class of 2024" = as.Date("2021-08-01"),
  "Class of 2025" = as.Date("2022-08-01"),
  "Class of 2026" = as.Date("2023-08-01"),
  "Class of 2027" = as.Date("2024-08-01"),
  "Class of 2028" = as.Date("2025-08-01"),
  "Class of 2029" = as.Date("2026-08-01")
)

active_cohorts <- function(today = Sys.Date()) {
  names(COHORT_START_DATES)[vapply(
    COHORT_START_DATES,
    function(d) d <= today,
    logical(1)
  )]
}

# Map "Class of 2027" → "class_of_2027" (the CSV filename slug)
cohort_slug <- function(cohort_label) {
  tolower(gsub(" ", "_", cohort_label))
}

# Inverse: "class_of_2027" → "Class of 2027"
cohort_label_from_slug <- function(slug) {
  paste(
    "Class",
    "of",
    sub(".*_of_([0-9]{4})$", "\\1", slug)
  )
}

# -----------------------------------------------------------------------------
# Multi-component course detection (spec §Important Notes)
# -----------------------------------------------------------------------------
# A course whose `course_name` ends in " A"/" B"/" C"/" D" is one component
# of an aggregated course (e.g., VETM 804 A/B/C/D — Clinical Logic in Doctoring).
# These route to generate_aggregated_assessment_plots() instead of the
# single-course variant.
is_multi_component_course <- function(course_name) {
  grepl("\\s[A-D]$", course_name)
}

# Strip " A"/" B" suffix to get the base course name.
base_course_name <- function(course_name) {
  sub("\\s[A-D]$", "", course_name)
}

# VETM 817 Selectives: a meta-course aggregating SEL 001–036.
is_selectives_meta <- function(course_code) {
  !is.null(course_code) && trimws(course_code) == "VETM 817"
}

# -----------------------------------------------------------------------------
# Date helpers
# -----------------------------------------------------------------------------
# If a curriculum period's finish_date is in the future, cvm.course.portfolio
# needs `use_end_date = Sys.Date()` to treat the period as valid. Otherwise
# the default (Sys.Date()) already works.
pick_use_end_date <- function(period_finish_date, today = Sys.Date()) {
  d <- tryCatch(as.Date(period_finish_date), error = function(e) NA)
  if (length(d) == 0 || is.na(d[[1]])) return(today)
  if (d > today) today else d
}

# Human-friendly "3 days ago" / "in 2 hours".
relative_time <- function(ts, now = Sys.time()) {
  if (is.null(ts) || is.na(ts)) return("never")
  secs <- as.numeric(difftime(now, ts, units = "secs"))
  if (is.na(secs)) return("unknown")
  abs_secs <- abs(secs)
  suffix <- if (secs >= 0) "ago" else "from now"
  unit <- if (abs_secs < 60) {
    sprintf("%.0f seconds", abs_secs)
  } else if (abs_secs < 3600) {
    sprintf("%.0f minutes", abs_secs / 60)
  } else if (abs_secs < 86400) {
    sprintf("%.1f hours", abs_secs / 3600)
  } else if (abs_secs < 86400 * 14) {
    sprintf("%.0f days", abs_secs / 86400)
  } else if (abs_secs < 86400 * 60) {
    sprintf("%.0f weeks", abs_secs / (86400 * 7))
  } else {
    sprintf("%.0f months", abs_secs / (86400 * 30))
  }
  paste(unit, suffix)
}

# Staleness classification for refresh badges.
# Fresh (<= 8 days), stale (8-14 days), expired (> 14 days).
cache_freshness <- function(ts, now = Sys.time()) {
  if (is.null(ts) || is.na(ts)) return("expired")
  age_days <- as.numeric(difftime(now, ts, units = "days"))
  if (age_days <= 8) "fresh"
  else if (age_days <= 14) "stale"
  else "expired"
}

# -----------------------------------------------------------------------------
# File-path helpers
# -----------------------------------------------------------------------------
project_root <- function() {
  # Resolves to the dashboard project dir when sourced from app.R or scripts.
  r <- getwd()
  # If running from scripts/ via Rscript, walk up once.
  if (basename(r) == "scripts") r <- dirname(r)
  r
}

cache_dir <- function(root = project_root()) file.path(root, "data", "cache")
course_ids_dir <- function(root = project_root()) file.path(root, "data", "course_ids")
outputs_dir <- function(root = project_root()) file.path(root, "outputs")
logs_dir <- function(root = project_root()) file.path(root, "logs")

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

cache_path <- function(cohort_label, course_code, file = NULL, root = project_root()) {
  slug <- cohort_slug(cohort_label)
  safe_code <- gsub("[^A-Za-z0-9_-]", "_", course_code)
  dir <- file.path(cache_dir(root), slug, safe_code)
  if (!is.null(file)) file.path(dir, file) else dir
}

#' Sanitize a course name the same way course_folder() does, so the per-
#' course directory name composed by surveys_dir / feedback_dir / downloads_dir
#' lines up with the dashboard's main folder (outputs/{code}_{name}/).
.safe_course_segment <- function(course_name) {
  safe_name <- gsub("[^A-Za-z0-9]+", "_", course_name)
  sub("_+$", "", safe_name)
}

#' Per-class surveys folder path. New convention (one folder per course):
#'   outputs/{course_code}_{course_name}/surveys/{class_slug}/
#' PDFs are dropped here manually; the render ensures the directory exists
#' once the cohort's latest period_finish_date + 2 days has passed.
surveys_dir <- function(course_code, course_name, cohort_label,
                        root = outputs_dir()) {
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  safe_name <- .safe_course_segment(course_name)
  slug <- cohort_slug(cohort_label)
  file.path(root, sprintf("%s_%s", safe_code, safe_name), "surveys", slug)
}

#' Per-class student-feedback folders (mid-course + delta). New convention:
#'   outputs/{course_code}_{course_name}/feedback/{class_slug}/midcourse/
#'   outputs/{course_code}_{course_name}/feedback/{class_slug}/delta/
#' PDFs are dropped manually; the render only ensures the folders exist.
feedback_dir <- function(course_code, course_name, cohort_label, kind,
                         root = outputs_dir()) {
  stopifnot(kind %in% c("midcourse", "delta"))
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  safe_name <- .safe_course_segment(course_name)
  slug <- cohort_slug(cohort_label)
  file.path(root, sprintf("%s_%s", safe_code, safe_name),
            "feedback", slug, kind)
}

#' Course-level general downloads folder. Anything Ish drops here is
#' auto-listed in the Downloads tab — no schema, no naming convention. PDFs,
#' XLSX, PPTX, CSV, ZIP, etc. are all surfaced.
#'   outputs/{course_code}_{course_name}/downloads/
downloads_dir <- function(course_code, course_name,
                          root = outputs_dir()) {
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  safe_name <- .safe_course_segment(course_name)
  file.path(root, sprintf("%s_%s", safe_code, safe_name), "downloads")
}

# Root of the existing Box-hosted course portfolio folder. Used ONLY to
# scan for already-uploaded survey PDFs and copy them into the new render's
# surveys/ folder. The render NEVER writes back into this path.
CVM_BOX_PORTFOLIO_ROOT <- path.expand(file.path(
  "~/Library/CloudStorage/Box-Box",
  "DS and Kadian",
  "A-COMPLETE-TENURE-WORK-DATA-Shripad-Sinari",
  "CVM", "projects", "Curriculum_Evaluation_Committee",
  "course-portfolios"
))

#' Scan the existing Box course-portfolio folder for survey PDFs matching
#' this course's numeric code and copy them into the new surveys dir.
#'
#' Matching rule: filename (case-insensitive) contains the course's digit
#' code (e.g. "801" for VETM 801) AND the substring "survey". A file path
#' additionally has to include the cohort year (e.g. "2026") somewhere in
#' its ancestor path to be attributed to that cohort — otherwise the same
#' PDF would be copied into every cohort's folder.
#'
#' @param course_code       e.g. "VETM 801"
#' @param cohort_label      e.g. "Class of 2026"
#' @param target_dir        destination (typically surveys_dir(...))
#' @param box_root          override for testing
#' @param logger            function(msg, level) for render_log integration;
#'                          defaults to message() so helper works standalone.
#' @return                  character vector of destination paths copied.
copy_box_survey_pdfs <- function(course_code, cohort_label, target_dir,
                                 box_root = CVM_BOX_PORTFOLIO_ROOT,
                                 logger = NULL) {
  if (is.null(logger)) logger <- function(msg, level = "INFO") message(msg)

  if (!dir.exists(box_root)) {
    logger(sprintf("Box portfolio root not found: %s (skipping survey scan)",
                   box_root), "INFO")
    return(character(0))
  }

  digits <- regmatches(course_code, regexpr("[0-9]+", course_code))
  if (length(digits) == 0 || !nzchar(digits[[1]])) {
    logger(sprintf("course_code '%s' has no numeric portion — skipping survey scan",
                   course_code), "WARN")
    return(character(0))
  }
  code_digits <- digits[[1]]

  # Year substring used to filter by cohort (takes last 2 or 4 digits).
  year_full <- sub("^Class of ", "", cohort_label)
  year_short <- substr(year_full, nchar(year_full) - 1L, nchar(year_full))

  # Enumerate every PDF under the box root (recursive). This is cheaper than
  # trying to guess the subfolder structure since the Box layout can vary.
  all_pdfs <- list.files(box_root,
                         pattern = "\\.pdf$",
                         full.names = TRUE,
                         recursive = TRUE,
                         ignore.case = TRUE)
  if (length(all_pdfs) == 0) return(character(0))

  filename_matches <- grepl(code_digits, basename(all_pdfs), fixed = TRUE) &
                      grepl("survey", basename(all_pdfs), ignore.case = TRUE)
  # Cohort year must appear somewhere in the full path (folder or filename).
  path_has_year  <- grepl(year_full,  all_pdfs, fixed = TRUE) |
                    grepl(sprintf("(?<!\\d)%s(?!\\d)", year_short),
                          all_pdfs, perl = TRUE)

  matches <- all_pdfs[filename_matches & path_has_year]
  if (length(matches) == 0) return(character(0))

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  copied <- character(0)
  for (src in matches) {
    dst <- file.path(target_dir, basename(src))
    # Skip if same size already there (cheap de-dup; full checksum not worth
    # the IO for weekly renders).
    if (file.exists(dst) && file.info(dst)$size == file.info(src)$size) next
    ok <- suppressWarnings(file.copy(src, dst, overwrite = TRUE))
    if (isTRUE(ok)) {
      logger(sprintf("survey PDF found -> copied to surveys/ : %s",
                     basename(dst)))
      copied <- c(copied, dst)
    } else {
      logger(sprintf("survey PDF copy FAILED: %s -> %s",
                     src, dst), "WARN")
    }
  }

  # Per-cohort dedup pass: Box occasionally surfaces the same survey under
  # two filenames (renamed copies). Group PDFs by size ±1KB; within each
  # group keep the shortest-named file and delete the rest.
  all_after <- list.files(target_dir, pattern = "\\.pdf$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(all_after) > 1) {
    sizes <- vapply(all_after, function(p) file.info(p)$size, numeric(1))
    ord <- order(sizes)
    all_after <- all_after[ord]
    sizes     <- sizes[ord]
    kept_size <- NA_real_
    kept_path <- NULL
    for (i in seq_along(all_after)) {
      p <- all_after[[i]]; s <- sizes[[i]]
      if (is.na(kept_size) || abs(s - kept_size) > 1024) {
        kept_size <- s; kept_path <- p; next
      }
      if (nchar(basename(p)) < nchar(basename(kept_path))) {
        # current is shorter-named — keep current, delete previous keeper.
        suppressWarnings(file.remove(kept_path))
        logger(sprintf("duplicate survey PDF removed: %s",
                       basename(kept_path)))
        kept_path <- p
      } else {
        suppressWarnings(file.remove(p))
        logger(sprintf("duplicate survey PDF removed: %s",
                       basename(p)))
      }
    }
  }
  copied
}

#' Extract every text page out of a survey PDF and concatenate. Wraps
#' `pdftools::pdf_text()` with a tryCatch so a bad / encrypted / image-only
#' PDF doesn't kill the render — we just emit a warning and return NULL.
#' Used by `analyse_survey_trends()` for the Survey Trends tab.
extract_survey_text <- function(pdf_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    warning("pdftools not installed; cannot extract survey text")
    return(NULL)
  }
  tryCatch({
    text <- pdftools::pdf_text(pdf_path)
    paste(text, collapse = "\n")
  }, error = function(e) {
    warning(sprintf("Could not read PDF: %s — %s",
                    basename(pdf_path), conditionMessage(e)))
    NULL
  })
}

#' Strip noisy header / metadata lines from a survey PDF text dump before
#' showing it on the dashboard. The cvm.gradebook survey export prepends
#' "Report download date:", "Report Start Date:", "Report End Date:",
#' "Completed Forms on …" and a "Form:" line that have no analytic value.
clean_survey_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(text)
  text <- gsub("Report download date:[^\n]*\n?", "", text, ignore.case = TRUE)
  text <- gsub("Report Start Date:[^\n]*\n?",   "", text, ignore.case = TRUE)
  text <- gsub("Report End Date:[^\n]*\n?",     "", text, ignore.case = TRUE)
  text <- gsub("Completed Forms on[^\n]*\n?",   "", text, ignore.case = TRUE)
  text <- gsub("Form:[^\n]*\n?",                "", text, ignore.case = TRUE)
  text <- gsub("\\s{3,}", "  ", text)
  trimws(text)
}

#' Human-readable bytes.
fmt_bytes <- function(n) {
  if (is.null(n) || is.na(n)) return("—")
  u <- c("B", "KB", "MB", "GB", "TB")
  i <- max(0, min(length(u) - 1, floor(log(max(n, 1), 1024))))
  sprintf("%.1f %s", n / 1024^i, u[i + 1])
}

# -----------------------------------------------------------------------------
# Null-coalesce and misc
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Deprecated-taxonomy filter.
#' Tag strings starting with "Do not use" (any casing) are retired taxonomy
#' entries that should never appear on a faculty-facing dashboard. Applied
#' at every surface: bar chart, coverage %, uncovered list, change narrative.
is_deprecated_tag <- function(x) {
  if (length(x) == 0) return(logical(0))
  grepl("^do not use", as.character(x), ignore.case = TRUE)
}

#' Classify a free-text assessment name into a broad category for the
#' breakdown table (VMH gradebook doesn't expose the meta type directly
#' through cvm.gradebook::gradebook_data — we pattern-match instead).
#'
#' Ordered from most specific → most general. Preserves original capitalization
#' style ("Final Exam", not "final exam") so sorted output is tidy.
assessment_type_from_name <- function(name) {
  if (length(name) == 0) return(character(0))
  n <- tolower(as.character(name))
  out <- rep("Other", length(n))
  out[grepl("weighted.*overall|individual.*weighted", n)]      <- "Overall"
  out[out == "Other" & grepl("\\bfinal\\b", n)]                <- "Final Exam"
  out[out == "Other" & grepl("midterm|mid[-\\s]?term", n)]     <- "Midterm"
  out[out == "Other" & grepl("\\bexam\\b", n)]                 <- "Exam"
  out[out == "Other" & grepl("\\birat\\b", n)]                 <- "IRAT"
  out[out == "Other" & grepl("\\bgrat\\b", n)]                 <- "GRAT"
  out[out == "Other" & grepl("\\btbl\\b|team[-\\s]?based",  n)] <- "TBL"
  out[out == "Other" & grepl("quiz", n)]                       <- "Quiz"
  out[out == "Other" & grepl("homework|\\bhw\\b",  n)]         <- "Homework"
  out[out == "Other" & grepl("lab report|lab\\b", n)]          <- "Lab"
  out[out == "Other" & grepl("case[-\\s]?stud(y|ies)", n)]     <- "Case study"
  out[out == "Other" & grepl("practical", n)]                  <- "Practical"
  out[out == "Other" & grepl("participation|attendance", n)]   <- "Participation"
  out[out == "Other" & grepl("project|paper|essay", n)]        <- "Paper / project"
  out[out == "Other" & grepl("presentation", n)]               <- "Presentation"
  out
}

safe_pct <- function(x, y, digits = 1) {
  if (is.null(y) || is.na(y) || y == 0) return(NA_real_)
  round(100 * x / y, digits)
}

# Pretty-format a numeric for KPI display.
fmt_num <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
  if (abs(x - round(x)) < 1e-8) format(round(x), big.mark = ",")
  else formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_ts <- function(ts) {
  if (is.null(ts) || length(ts) == 0 || is.na(ts)) return("never")
  format(ts, "%Y-%m-%d %H:%M")
}
