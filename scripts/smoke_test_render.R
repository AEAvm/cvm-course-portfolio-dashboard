#!/usr/bin/env Rscript
# scripts/smoke_test_render.R
# -----------------------------------------------------------------------------
# Integration smoke test: renders both .qmd templates against each captured
# pipeline-result fixture in tests/fixtures/, asserting the output HTML is
# well-formed and contains the widgets and content we expect.
#
# Offline by design — no VMH, no OpenAI, no Box. A fixture is a canned RDS
# saved by a previous real render via `--save-fixture`.
#
# Usage:
#   Rscript scripts/smoke_test_render.R
#   Rscript scripts/smoke_test_render.R --fixture VETM801_class_of_2026
#   Rscript scripts/smoke_test_render.R --combined-only
#
# Exit 0 = all renders OK + all assertions pass. 1 = any failure.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(quarto); library(htmltools)
})

# ---- Resolve project root ---------------------------------------------------
script_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = TRUE),
  error = function(e) {
    a <- commandArgs(trailingOnly = FALSE)
    f <- a[grep("^--file=", a)]
    if (length(f)) normalizePath(dirname(sub("^--file=", "", f[[1]]))) else getwd()
  }
)
proj_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
setwd(proj_root)

# ---- Load the R layer (so the .qmd set-up chunks don't explode later) -------
for (f in c("utils.R", "data_layer.R", "portfolio_runner.R",
            "comparison_engine.R", "ai_interpret.R",
            "plot_helpers.R", "html_components.R", "render_helpers.R")) {
  source(file.path("R", f))
}

# ---- CLI args ---------------------------------------------------------------
args <- parse_cli_args()
filter_name   <- args$fixture
combined_only <- isTRUE(args[["combined-only"]] %in% c("TRUE", "true", ""))
year_only     <- isTRUE(args[["year-only"]]     %in% c("TRUE", "true", ""))

fixtures_dir <- file.path(proj_root, "tests", "fixtures")
if (!dir.exists(fixtures_dir)) dir.create(fixtures_dir, recursive = TRUE)

fixture_paths <- list.files(fixtures_dir, pattern = "\\.rds$", full.names = TRUE)
# Ignore the narrative sidecars — they're loaded alongside the main fixture.
fixture_paths <- fixture_paths[!grepl("_narrative\\.rds$", fixture_paths)]

if (!is.null(filter_name)) {
  fixture_paths <- fixture_paths[grepl(filter_name, basename(fixture_paths), fixed = TRUE)]
}

if (length(fixture_paths) == 0) {
  cat(
    "No fixtures found in ", fixtures_dir, "\n",
    "\n",
    "Capture a fixture first with:\n",
    "  Rscript render_dashboard.R --course_id 5 --class \"Class of 2026\" --skip-narrative --save-fixture\n",
    "\n",
    "Or drop any pipeline-result .rds into tests/fixtures/ manually.\n",
    sep = ""
  )
  quit(save = "no", status = 2)
}

# ---- Test runner ------------------------------------------------------------
tmp_root <- tempfile("smoke_")
dir.create(tmp_root)
on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

# Assertion helpers — collect failures, don't abort on first.
failures <- character()
assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) {
    cat("  FAIL: ", msg, "\n", sep = "")
    failures <<- c(failures, msg)
  } else {
    cat("  ok  : ", msg, "\n", sep = "")
  }
}

# ---- Render one fixture through a template ---------------------------------
render_fixture <- function(fixture_path, template, param_name,
                           output_html, as_all_data = FALSE) {
  cat("\n--- ", basename(fixture_path), " -> ", basename(template), " ---\n", sep = "")

  # Copy the fixture into the smoke-test workdir. The .qmd templates look for
  # a matching _narrative.rds sidecar; copy that too if it exists.
  work <- file.path(tmp_root, tools::file_path_sans_ext(basename(fixture_path)))
  ensure_dir(work)

  work_rds <- file.path(work, "fixture.rds")

  if (as_all_data) {
    # Combined template expects a list of pipeline results. Wrap the single-
    # course fixture into a length-1 list if necessary.
    raw <- readRDS(fixture_path)
    payload <- if (is.list(raw) && !is.null(raw$course_id)) list(raw) else raw
    saveRDS(payload, work_rds)
  } else {
    file.copy(fixture_path, work_rds)
  }

  narr_src <- sub("\\.rds$", "_narrative.rds", fixture_path)
  if (file.exists(narr_src)) {
    file.copy(narr_src, sub("\\.rds$", "_narrative.rds", work_rds))
  } else {
    # Seed a placeholder narrative so the template's file.exists() branch finds
    # something sane to read.
    saveRDS(list(available = FALSE,
                 narrative = "Smoke test — narrative unavailable.",
                 flags = empty_flags_df(),
                 questions = character(0),
                 generated_at = Sys.time(),
                 model = "smoke-test"),
            sub("\\.rds$", "_narrative.rds", work_rds))
  }

  # Render.
  stage_theme_for_quarto(proj_root)  # templates/lib/custom.scss
  params <- setNames(list(normalizePath(work_rds, mustWork = TRUE)), param_name)
  params$render_ts <- "smoke-test"

  render_ok <- tryCatch({
    quarto::quarto_render(
      input = template,
      output_file = output_html,
      execute_params = params,
      quiet = TRUE
    )
    TRUE
  }, error = function(e) {
    cat("  FAIL: quarto_render threw: ", conditionMessage(e), "\n", sep = "")
    failures <<- c(failures, sprintf("render(%s): %s", basename(template),
                                     conditionMessage(e)))
    FALSE
  })

  # Quarto writes next to the .qmd; move into work dir.
  src_html <- file.path(dirname(template), output_html)
  dst_html <- file.path(work, output_html)
  if (file.exists(src_html)) file.rename(src_html, dst_html)
  # Sidecar _files dir — discover and move (same pattern as render_dashboard.R).
  for (fd in list.files(dirname(template),
                        pattern = "_files$",
                        full.names = TRUE, include.dirs = TRUE)) {
    if (!dir.exists(fd)) next
    target <- file.path(work, basename(fd))
    if (dir.exists(target)) unlink(target, recursive = TRUE)
    file.rename(fd, target)
  }

  if (!render_ok) return(invisible(FALSE))

  assert_true(file.exists(dst_html),
              sprintf("%s created", output_html))

  if (!file.exists(dst_html)) return(invisible(FALSE))
  size <- file.info(dst_html)$size
  assert_true(size > 50 * 1024,
              sprintf("%s > 50 KB (actual: %s)",
                      output_html,
                      utils:::format.object_size(size, "auto")))

  html <- readLines(dst_html, warn = FALSE, encoding = "UTF-8")
  html <- paste(html, collapse = "\n")

  assert_true(grepl("plotly", html, fixed = TRUE),
              "HTML contains plotly widget markup")
  assert_true(!grepl("cannot coerce type 'object'", html, fixed = TRUE),
              "HTML free of S7-coercion error text")
  assert_true(!grepl("Error in `", html, fixed = TRUE),
              "HTML free of backticked R error markers")
  # Our sidebar helper emits the course_code verbatim — the raw dashes/spaces
  # survive htmltools escaping. Spot-check it shows up.
  result <- readRDS(work_rds)
  course_token <- if (is.list(result) && !is.null(result$course_id)) {
    result$course_code
  } else if (is.list(result) && length(result) > 0 && !is.null(result[[1]]$course_code)) {
    result[[1]]$course_code
  } else NA_character_
  if (!is.na(course_token)) {
    assert_true(grepl(course_token, html, fixed = TRUE),
                sprintf("course code %s appears in HTML", course_token))
  }

  invisible(TRUE)
}

# ---- Run the matrix ---------------------------------------------------------
year_template     <- file.path(proj_root, "templates", "course_year_dashboard.qmd")
combined_template <- file.path(proj_root, "templates", "course_combined_dashboard.qmd")

for (fx in fixture_paths) {
  tag <- tools::file_path_sans_ext(basename(fx))
  if (!combined_only) {
    render_fixture(
      fixture_path = fx,
      template     = year_template,
      param_name   = "result_path",
      output_html  = sprintf("smoke_year_%s.html", tag),
      as_all_data  = FALSE
    )
  }
  if (!year_only) {
    render_fixture(
      fixture_path = fx,
      template     = combined_template,
      param_name   = "all_data_path",
      output_html  = sprintf("smoke_combined_%s.html", tag),
      as_all_data  = TRUE
    )
  }
}

# ---- Report -----------------------------------------------------------------
cat("\n")
if (length(failures) == 0) {
  cat(sprintf("ALL GREEN — %d fixture(s) rendered cleanly.\n", length(fixture_paths)))
  quit(save = "no", status = 0L)
} else {
  cat(sprintf("FAIL — %d assertion(s) failed:\n", length(failures)))
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(save = "no", status = 1L)
}
