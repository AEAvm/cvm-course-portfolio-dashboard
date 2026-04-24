#!/usr/bin/env Rscript
# render_all.R — render every course for ONE graduating class.
#
# Usage:
#   Rscript render_all.R --class "Class of 2027"
#   Rscript render_all.R --class "Class of 2027" --with-exports --skip-narrative
#
# Intentionally scoped to a SINGLE class per invocation. If you want all
# courses × all classes rendered, wrap this script in a shell loop — do not
# add cross-class iteration here. Rendering one class at a time is the
# contract with the user (manageable runtime, manageable Box footprint).
#
# For each course in the class's CSV this script invokes render_dashboard.R
# as a separate process, so a single course's failure does not block others.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(glue)
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

for (f in c("utils.R", "render_helpers.R")) source(file.path("R", f))

args <- parse_cli_args()
class_arg <- args$class
if (is.null(class_arg)) {
  cat("Usage: Rscript render_all.R --class \"Class of YYYY\" [--with-exports] [--skip-narrative] [--include-combined]\n")
  quit(save = "no", status = 2)
}

include_combined <- isTRUE(args[["include-combined"]] %in% c("TRUE", "true", ""))
extra_flags <- c(
  if (isTRUE(args[["with-exports"]]   %in% c("TRUE", "true", ""))) "--with-exports"   else NULL,
  if (isTRUE(args[["skip-narrative"]] %in% c("TRUE", "true", ""))) "--skip-narrative" else NULL
)

slug <- cohort_slug(class_arg)
csv_path <- file.path("data", "course_ids", sprintf("%s_course_ids.csv", slug))
if (!file.exists(csv_path)) {
  render_log(sprintf("CSV not found: %s", csv_path), "ERROR")
  quit(save = "no", status = 1)
}

courses <- suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE)) |>
  dplyr::filter(.data$course_active == 1) |>
  dplyr::distinct(.data$course_id, .data$course_code, .data$course_name)

render_log(sprintf("BATCH start  class=%s  n_courses=%d", class_arg, nrow(courses)))

rscript <- file.path(R.home("bin"), "Rscript")
success <- 0L; failure <- 0L; skipped <- 0L

for (i in seq_len(nrow(courses))) {
  cid  <- as.integer(courses$course_id[[i]])
  code <- courses$course_code[[i]]
  name <- courses$course_name[[i]]

  render_log(sprintf("  [%d/%d] %s %s  (id=%d)", i, nrow(courses), code, name, cid))

  year_args <- c(
    "render_dashboard.R",
    "--course_id", as.character(cid),
    "--class",     class_arg,
    extra_flags
  )
  year_status <- system2(rscript, year_args,
                         stdout = file.path("logs", "render.log"),
                         stderr = file.path("logs", "render.log"))
  if (identical(year_status, 0L)) {
    success <- success + 1L
  } else {
    failure <- failure + 1L
    render_log(sprintf("    %s / %s  EXIT %s", code, class_arg, year_status), "ERROR")
  }

  if (include_combined) {
    cmb_args <- c("render_combined.R", "--course_id", as.character(cid), extra_flags)
    cmb_status <- system2(rscript, cmb_args,
                          stdout = file.path("logs", "render.log"),
                          stderr = file.path("logs", "render.log"))
    if (identical(cmb_status, 0L)) success <- success + 1L else failure <- failure + 1L
  }
}

render_log(sprintf("BATCH done  class=%s  success=%d  failure=%d  skipped=%d",
                   class_arg, success, failure, skipped))
quit(save = "no", status = if (failure > 0) 1L else 0L)
