# R/data_layer.R
# -----------------------------------------------------------------------------
# Data access: course ID CSVs (local) + VMH pool connection.
#
# SOURCE VALIDATION NOTES (verified against actual package tarballs, 2026-04-22)
# -----------------------------------------------------------------------------
# * resources::get_curriculum_courses(con) lives in resources_0.0.2.tar.gz,
#   R/funcs.R — NOT inside cvmverse or cvm.course.portfolio. It returns a
#   tibble with columns {id, name, code} (column names de-prefixed from
#   course_id/course_name/course_code).
# * vmh::start_connection(db = "me", ...) returns a {pool} object and opens
#   the `elentra_me` schema. Credentials come from the VMH_KEYRING_SERVICE
#   keyring entry (not env vars). VMH_SERVER env var provides the hostname.
# * vmh::stop_connection(con) calls pool::poolClose. Safe to re-call.
# * cvm.gradebook::gradebook() INTERNALLY calls vmh::start_connection(...) and
#   stop_connection(), so it ignores any shared pool we pass above. This is a
#   package-level concern; we pass the pool to create_course_portfolio() so the
#   cps/events/courses queries use it, but gradebook pulls are on their own
#   connections regardless.
# * cvm.course.portfolio::create_course_portfolio(con, ...) accepts either a
#   pool or a raw DBI connection (it just passes `con` to downstream calls).
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Course ID CSVs (offline-safe master list)
# -----------------------------------------------------------------------------

#' Read all class_of_XXXX_course_ids.csv files under data/course_ids/.
#' Returns a single tibble with one row per (cperiod_id, course_id) and an
#' added `cohort_label` column ("Class of 2027" etc.) derived from the filename.
#' Only cohorts in active_cohorts(today) are included.
load_course_ids <- function(dir = course_ids_dir(), today = Sys.Date()) {
  files <- list.files(dir, pattern = "^class_of_[0-9]{4}_course_ids\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) {
    warning("No course ID CSVs found in ", dir)
    return(tibble::tibble())
  }

  active <- active_cohorts(today)

  rows <- lapply(files, function(f) {
    slug <- sub("_course_ids\\.csv$", "", basename(f))
    label <- cohort_label_from_slug(slug)
    if (!label %in% active) return(NULL)
    df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
    df$cohort_label <- label
    df
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(tibble::tibble())

  out <- dplyr::bind_rows(rows)
  # The CSVs ship dates as Unix epoch seconds (integer). Convert to Date.
  out$period_start_date  <- epoch_to_date(out$period_start_date)
  out$period_finish_date <- epoch_to_date(out$period_finish_date)
  out
}

#' Parse a vector that may be either unix-epoch seconds (numeric) or an
#' ISO-date string into a Date. Preserves NAs.
epoch_to_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (is.numeric(x) || all(grepl("^-?[0-9]+$", trimws(as.character(x[!is.na(x)]))))) {
    as.Date(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  } else {
    suppressWarnings(as.Date(as.character(x)))
  }
}

#' Collapse the per-period CSV rows into a course master list:
#' unique (course_id, course_code, course_name), with cohorts aggregated.
course_master_list <- function(course_rows) {
  if (nrow(course_rows) == 0) return(tibble::tibble())
  course_rows |>
    dplyr::filter(.data$course_active == 1L | is.na(.data$course_active)) |>
    dplyr::group_by(.data$course_id, .data$course_code, .data$course_name) |>
    dplyr::summarise(
      cohorts = list(sort(unique(.data$cohort_label))),
      n_periods = dplyr::n(),
      earliest  = min(.data$period_start_date, na.rm = TRUE),
      latest    = max(.data$period_finish_date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$course_code)
}

#' Filter course_rows to a specific course and a set of cohorts.
periods_for_course_cohorts <- function(course_rows, course_id, cohorts) {
  course_rows |>
    dplyr::filter(
      .data$course_id == !!course_id,
      .data$cohort_label %in% cohorts
    ) |>
    dplyr::arrange(.data$cohort_label, .data$cperiod_id)
}

# -----------------------------------------------------------------------------
# VMH connection
# -----------------------------------------------------------------------------
#' Attempt to open a VMH pool. Returns NULL on failure (no crash); the caller
#' is expected to inspect the result and display an offline banner.
try_vmh_connect <- function(db = "me") {
  tryCatch({
    if (!requireNamespace("vmh", quietly = TRUE)) {
      return(list(pool = NULL, error = "vmh package not installed"))
    }
    # vmh::check_vmh_env() raises on missing env vars; we tolerate that.
    ok_env <- tryCatch({ vmh::check_vmh_env(); TRUE },
                       error = function(e) conditionMessage(e))
    if (!isTRUE(ok_env)) return(list(pool = NULL, error = ok_env))

    pool <- vmh::start_connection(db = db)
    if (is.null(pool)) {
      return(list(pool = NULL, error = "vmh::start_connection returned NULL"))
    }
    list(pool = pool, error = NULL)
  }, error = function(e) {
    list(pool = NULL, error = conditionMessage(e))
  })
}

#' Close a pool safely (no-op if NULL).
safe_vmh_disconnect <- function(pool) {
  if (is.null(pool)) return(invisible(NULL))
  tryCatch(vmh::stop_connection(pool),
           error = function(e) message("Pool close error: ", conditionMessage(e)))
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Course metadata from VMH (optional — CSVs already carry this)
# -----------------------------------------------------------------------------
#' Pull the canonical course list from VMH via resources::get_curriculum_courses.
#' Returns NULL if the call fails (used only for cross-checks / diagnostics).
try_vmh_courses <- function(pool) {
  if (is.null(pool)) return(NULL)
  tryCatch(
    resources::get_curriculum_courses(pool),
    error = function(e) {
      message("VMH course pull failed: ", conditionMessage(e))
      NULL
    }
  )
}
