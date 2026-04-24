# R/render_helpers.R
# -----------------------------------------------------------------------------
# Shared helpers used by render_dashboard.R, render_combined.R, render_all.R.
#   * course_folder() — resolves outputs/{course_code}_{safe_name}/ path
#   * write_shared_assets() — stages lib/custom.scss and lib/fonts/ for the
#     Quarto renders to pick up via the YAML theme: block
#   * consolidate_libs() — post-render step: moves per-file _files/libs/*
#     into a single course-folder lib/ and rewrites HTML refs
#   * stage_rds_inputs() — writes the result + narrative RDS payloads that
#     the .qmd reads via params$result_path
# -----------------------------------------------------------------------------

#' Resolve a course folder name like "VETM801_Foundations". Stable across
#' renders for the same course. Non-alphanumeric chars in code/name are
#' collapsed to underscores.
course_folder <- function(course_code, course_name, root = outputs_dir()) {
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  safe_name <- gsub("[^A-Za-z0-9]+", "_", course_name)
  safe_name <- sub("_+$", "", safe_name)
  file.path(root, sprintf("%s_%s", safe_code, safe_name))
}

#' Dashboard HTML filename for a single class render.
year_html_name <- function(course_code, cohort_label) {
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  safe_class <- gsub("[^A-Za-z0-9]+", "_", cohort_label)
  sprintf("%s_%s.html", safe_code, safe_class)
}

combined_html_name <- function(course_code) {
  safe_code <- gsub("[^A-Za-z0-9]", "", course_code)
  sprintf("%s_COMBINED.html", safe_code)
}

#' Move the rendered HTML + all `*_files/` sidecar directories from
#' templates/ into the final course folder. Quarto names each `_files/` dir
#' after the INPUT .qmd stem (e.g., `course_year_dashboard_files/`), NOT the
#' output_file name — so we can't compute the dir name from `output_html`.
#' Discover any `_files/` dir that exists in templates/ and move all of them.
#'
#' Returns invisibly the full list of artifacts moved. Logs each move so a
#' future silent mismatch surfaces in logs/render.log instead of producing
#' a dashboard with broken asset references.
move_quarto_outputs <- function(output_html, course_dir,
                                templates_dir = file.path(project_root(), "templates")) {
  moved <- character()

  # 1. The HTML itself — named after output_file.
  src_html <- file.path(templates_dir, output_html)
  if (file.exists(src_html)) {
    dst_html <- file.path(course_dir, output_html)
    if (file.exists(dst_html)) file.remove(dst_html)
    file.rename(src_html, dst_html)
    moved <- c(moved, dst_html)
    render_log(sprintf("moved HTML -> %s", dst_html))
  } else {
    render_log(sprintf("expected HTML not found at %s", src_html), "WARN")
  }

  # 2. Every *_files/ sidecar dir Quarto created in templates/.
  files_dirs <- list.files(templates_dir, pattern = "_files$",
                           full.names = TRUE, include.dirs = TRUE)
  files_dirs <- files_dirs[dir.exists(files_dirs)]
  for (fd in files_dirs) {
    target <- file.path(course_dir, basename(fd))
    if (dir.exists(target)) unlink(target, recursive = TRUE)
    # file.rename can fail across filesystem boundaries; fall back to copy+remove.
    ok <- suppressWarnings(file.rename(fd, target))
    if (!isTRUE(ok)) {
      file.copy(fd, course_dir, recursive = TRUE)
      unlink(fd, recursive = TRUE)
    }
    moved <- c(moved, target)
    render_log(sprintf("moved _files dir -> %s", target))
  }

  if (length(files_dirs) == 0) {
    render_log(sprintf("no *_files/ dirs found in %s (did quarto_render succeed?)",
                       templates_dir), "WARN")
  }

  invisible(moved)
}

#' Stage the SCSS theme file at templates/lib/custom.scss so that Quarto's
#' YAML (`theme: [default, lib/custom.scss]`) resolves at render time.
#' Quarto looks up theme files relative to the .qmd's directory, NOT the
#' cwd knitr chunks see, and NOT the final output dir. So the theme has
#' to live next to the template at render time; the consolidate_libs()
#' step later handles the compiled Bootstrap CSS in the output dir.
stage_theme_for_quarto <- function(root = project_root()) {
  dst_dir <- file.path(root, "templates", "lib")
  if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  src <- file.path(root, "www", "custom.scss")
  if (file.exists(src)) {
    file.copy(src, file.path(dst_dir, "custom.scss"), overwrite = TRUE)
  } else {
    warning("www/custom.scss not found — Quarto will render with default theme only.")
  }
  invisible(dst_dir)
}

#' Copy the project's shared CSS + fonts into the course folder's lib/ so
#' the Quarto YAML (theme: lib/custom.scss) can resolve relative paths.
#' Idempotent — safe to call repeatedly for the same course folder.
#'
#' NOTE: the `root` parameter is deliberately NOT named `project_root` — using
#' the same name as the function `project_root()` creates a recursive default
#' argument reference ("promise already under evaluation") because the default
#' expression resolves `project_root` to the parameter itself, not the fn.
write_shared_assets <- function(course_dir, root = project_root()) {
  lib_dir <- file.path(course_dir, "lib")
  ensure_dir(lib_dir)

  src_css <- file.path(root, "www", "custom.scss")
  if (file.exists(src_css)) {
    file.copy(src_css, file.path(lib_dir, "custom.scss"), overwrite = TRUE)
  }

  src_plain_css <- file.path(root, "www", "custom.css")
  if (file.exists(src_plain_css)) {
    file.copy(src_plain_css, file.path(lib_dir, "custom.css"), overwrite = TRUE)
  }

  src_fonts <- file.path(root, "www", "fonts")
  if (dir.exists(src_fonts)) {
    dst_fonts <- file.path(lib_dir, "fonts")
    ensure_dir(dst_fonts)
    for (f in list.files(src_fonts, full.names = TRUE)) {
      file.copy(f, file.path(dst_fonts, basename(f)), overwrite = TRUE)
    }
  }

  src_logo <- file.path(root, "www", "cvm_logo.png")
  if (file.exists(src_logo)) {
    file.copy(src_logo, file.path(lib_dir, "cvm_logo.png"), overwrite = TRUE)
  }
  invisible(lib_dir)
}

#' Stage the RDS inputs the .qmd will read at render time. Returns the
#' result_path the Quarto render should pass via --execute-params.
#'
#' Layout under the course dir (all transient — removed after render):
#'   course_dir/.render_cache/{tag}_result.rds
#'   course_dir/.render_cache/{tag}_result_narrative.rds
#' The .qmd reads {tag}_result_narrative.rds by subbing \\.rds$ → _narrative.rds
#' on the result_path param, so the two files must share a stem.
stage_rds_inputs <- function(course_dir, tag, result, narrative = NULL) {
  staging <- file.path(course_dir, ".render_cache")
  ensure_dir(staging)
  res_path <- file.path(staging, sprintf("%s_result.rds", tag))
  saveRDS(result, res_path)
  narr_path <- sub("\\.rds$", "_narrative.rds", res_path)
  saveRDS(narrative, narr_path)
  res_path
}

clean_render_cache <- function(course_dir) {
  staging <- file.path(course_dir, ".render_cache")
  if (dir.exists(staging)) unlink(staging, recursive = TRUE)
  invisible(NULL)
}

#' Recursively MERGE src into dst — copying every file with overwrite.
#' Unlike file.copy(recursive = TRUE, overwrite = TRUE), which has surprising
#' semantics when dst already exists as a directory, this walks src and lands
#' each file at the correct relative path. Used by consolidate_libs().
merge_copy <- function(src, dst) {
  if (!dir.exists(src)) {
    ensure_dir(dirname(dst))
    file.copy(src, dst, overwrite = TRUE)
    return(invisible())
  }
  ensure_dir(dst)
  src_abs <- normalizePath(src, mustWork = TRUE)
  files <- list.files(src_abs, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  for (f in files) {
    rel <- sub(paste0("^", src_abs, "/?"), "", normalizePath(f, mustWork = TRUE))
    target <- file.path(dst, rel)
    ensure_dir(dirname(target))
    file.copy(f, target, overwrite = TRUE)
  }
  invisible()
}

#' Post-render: collapse all {html_stem}_files/libs/* subtrees under
#' course_dir into a single course_dir/lib/ and rewrite the HTML references.
#'
#' Quarto writes each render's htmlwidgets / bootstrap / plotly assets into
#' a sibling {stem}_files/libs/ dir. Every per-year dashboard for a course
#' ships the same ~5 MB of JS; consolidating into one lib/ per course folder
#' drops total Box footprint from ~3 GB to ~200-300 MB across all courses.
#'
#' CRITICAL: uses merge_copy() rather than skip-if-exists. Every Quarto
#' render produces CSS files with content-hashed names (e.g.
#' bootstrap-e8cf07...min.css). On a second render that hash changes. A
#' naive skip leaves the FIRST render's hash in lib/ while the new HTML
#' references the NEW hash — 404s on its own stylesheet, page renders
#' unstyled. Merging preserves every hash from every render so the HTML
#' always finds its file.
consolidate_libs <- function(course_dir) {
  if (!dir.exists(course_dir)) return(invisible(NULL))

  shared_lib <- file.path(course_dir, "lib")
  ensure_dir(shared_lib)

  file_dirs <- list.files(course_dir, pattern = "_files$", full.names = TRUE,
                          include.dirs = TRUE)
  file_dirs <- file_dirs[dir.exists(file_dirs)]

  rewrites <- list()

  for (fd in file_dirs) {
    stem <- sub("_files$", "", basename(fd))
    libs_sub <- file.path(fd, "libs")
    if (!dir.exists(libs_sub)) next

    # Merge every sub-library (bootstrap-*, plotly-binding-*, htmlwidgets-*,
    # quarto-html, ...) into the shared lib/. New hash'd files land
    # alongside existing ones so old HTMLs still work.
    for (sub in list.files(libs_sub, full.names = TRUE, include.dirs = TRUE)) {
      target <- file.path(shared_lib, basename(sub))
      merge_copy(sub, target)
    }

    rewrites[[stem]] <- list(
      from = sprintf("%s_files/libs/", stem),
      to   = "lib/"
    )
  }

  # Rewrite each HTML in the course dir.
  html_files <- list.files(course_dir, pattern = "\\.html$", full.names = TRUE)
  for (h in html_files) {
    txt <- readLines(h, warn = FALSE)
    orig <- paste(txt, collapse = "\n")
    new <- orig
    # Any {stem}_files/libs/ → lib/. Fixed-string replace keeps perf predictable.
    for (rw in rewrites) {
      new <- gsub(rw$from, rw$to, new, fixed = TRUE)
    }
    # Also catch the site_libs pattern Quarto sometimes uses for nav/theme.
    new <- gsub("site_libs/", "lib/", new, fixed = TRUE)
    if (!identical(new, orig)) {
      writeLines(new, h, useBytes = TRUE)
    }
  }

  # Delete the now-redundant {stem}_files/libs subdirs. Keep {stem}_files
  # itself if it holds anything else (e.g., figure-html/).
  for (fd in file_dirs) {
    libs_sub <- file.path(fd, "libs")
    if (dir.exists(libs_sub)) unlink(libs_sub, recursive = TRUE)
    # If the _files dir is now empty, remove it too.
    remaining <- list.files(fd, all.files = FALSE, recursive = FALSE)
    if (length(remaining) == 0) unlink(fd, recursive = TRUE)
  }

  invisible(shared_lib)
}

#' CLI arg parser — tiny, tolerant. Accepts --key=value and --key value.
#' Returns a named list of strings.
parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[[i]]
    if (grepl("^--", a)) {
      if (grepl("=", a)) {
        kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
        out[[kv[[1]]]] <- paste(kv[-1], collapse = "=")
        i <- i + 1
      } else {
        key <- sub("^--", "", a)
        val <- if (i + 1 <= length(args) && !grepl("^--", args[[i + 1]])) {
          i <- i + 1; args[[i]]
        } else "TRUE"
        out[[key]] <- val
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  out
}

#' Render-level logger — writes to logs/render.log with timestamps.
render_log <- function(msg, level = "INFO") {
  log_render_step(msg, level = level)
  cat(sprintf("[%s] %s  %s\n",
              format(Sys.time(), "%H:%M:%S"), level, msg))
}

#' Build the export artifacts (PPTX + zipped Excel workbooks) that the
#' Downloads tab links to. Runs inside tryCatch — a failure here does NOT
#' block the HTML render; it just means the Downloads tab shows placeholders.
build_export_artifacts <- function(pool, result, course_dir) {
  if (is.null(pool)) {
    render_log("skipping PPTX/XLSX (no VMH pool)", "WARN"); return(invisible(NULL))
  }
  pptx_template <- file.path(project_root(), "inst", "pptx_template",
                             "course-review-presentation-template.pptx")
  if (!file.exists(pptx_template)) {
    render_log(sprintf("PPTX template missing at %s", pptx_template), "WARN")
  }

  # PPTX — create_course_portfolio writes into the working directory.
  tryCatch({
    withr::with_dir(course_dir, {
      cvm.course.portfolio::create_course_portfolio(
        con = pool,
        course_ID = result$course_id,
        rgx = TRUE,
        outdir = ".",
        use_end_date = result$use_end_date,
        pptx_template = if (file.exists(pptx_template)) normalizePath(pptx_template) else NULL,
        n_periods = length(result$periods)
      )
    })
    render_log("PPTX generated")
  }, error = function(e) render_log(sprintf("PPTX FAILED: %s", conditionMessage(e)), "ERROR"))

  # Excel workbooks → zip.
  tryCatch({
    wb_dir <- file.path(course_dir, ".workbooks")
    ensure_dir(wb_dir)
    cvm.course.portfolio::create_workbooks(
      con = pool,
      data_list = result$data_list,
      outdir_list = stats::setNames(
        as.list(file.path(wb_dir, paste0("period_", names(result$data_list)))),
        names(result$data_list)
      ),
      aggregation = is_multi_component_course(result$course_name)
    )
    zip_file <- file.path(course_dir,
                          sprintf("%s_%s_workbooks.zip",
                                  gsub("[^A-Za-z0-9]", "", result$course_code),
                                  cohort_slug(result$cohort_label)))
    withr::with_dir(course_dir, {
      files_to_zip <- list.files(".workbooks", recursive = TRUE, full.names = TRUE)
      if (length(files_to_zip) > 0) {
        utils::zip(basename(zip_file), files = files_to_zip, flags = "-r9Xq")
      }
    })
    unlink(wb_dir, recursive = TRUE)
    render_log("Excel zip generated")
  }, error = function(e) render_log(sprintf("XLSX FAILED: %s", conditionMessage(e)), "ERROR"))
}
