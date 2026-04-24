# install_packages.R
#
# Installs the CVM in-house R packages from their local Box-synced tarballs.
# These packages are not on CRAN — they must be installed from the tarballs
# shipped by the CVM AEA team.
#
# Usage:
#   Rscript install_packages.R
#
# Preconditions:
#   * Box Drive (or Box Sync) is installed and signed in on this machine.
#   * The tarballs exist under the path below.
#   * CRAN packages that these tarballs depend on are already installed
#     (run `renv::restore()` first).

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
PKG_DIR <- file.path(
  "~/Library/CloudStorage/Box-Box",
  "DS and Kadian",
  "A-COMPLETE-TENURE-WORK-DATA-Shripad-Sinari",
  "CVM", "projects", "r-packages"
)

# Install order matters: dependencies first. Leaf packages last.
# Versions reflect the tarballs present in the Box folder as of 2026-04-22.
PKG_TARBALLS <- c(
  "resources_0.0.2.tar.gz",
  "curriculum.tag.maps_0.0.2.tar.gz",
  "cvm.courseEventFilter_0.0.1.tar.gz",
  "cvm.users_0.0.2.tar.gz",
  "cvm.gradebook_0.0.1.tar.gz",
  "event.curriculum.tags_0.0.1.tar.gz",
  "vmh_0.0.3.tar.gz",
  "cvm.openAI_0.0.1.tar.gz",
  "cvm.course.portfolio_0.0.2.tar.gz",
  "cvmverse_0.0.1.tar.gz"
)

# ------------------------------------------------------------------------------
# CRAN dependencies the CVM tarballs need at install / runtime.
# ------------------------------------------------------------------------------
# These are the non-obvious packages — things the CVM tarball DESCRIPTION files
# declare as Imports but that don't always get picked up by renv::restore() or
# the local install_packages path. `dbplyr` in particular is a silent hard
# requirement: it backs every `dplyr::tbl(pool, ...) |> collect()` call across
# event.curriculum.tags, resources, and cvm.gradebook.
CRAN_DEPS <- c(
  "dbplyr",       # required by dplyr::tbl() against any DBI/pool backend
  "DBI", "RMariaDB", "pool",
  "tibble", "tidyr", "dplyr", "readr", "purrr",
  "ggplot2", "plotly", "DT", "htmltools", "glue",
  "lubridate", "scales", "stringr", "tidyselect",
  "janitor", "readxl", "openxlsx", "officer",
  "commonmark", "jsonlite",
  "keyring", "cli", "usethis", "withr",
  "quarto",
  "openai",       # cvm.openAI dep
  "boxr"          # event.curriculum.tags::get_co2pig_map dep
)

ensure_cran <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing CRAN deps: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  } else {
    message("All CRAN deps already installed.")
  }
}
ensure_cran(CRAN_DEPS)

# ------------------------------------------------------------------------------
# CVM tarballs (Box-synced, not CRAN)
# ------------------------------------------------------------------------------
pkg_dir <- normalizePath(PKG_DIR, mustWork = FALSE)
if (!dir.exists(pkg_dir)) {
  stop(
    "CVM package directory not found:\n  ", pkg_dir, "\n",
    "Make sure Box Drive is installed and you are signed in."
  )
}

install_one <- function(tarball) {
  path <- file.path(pkg_dir, tarball)
  if (!file.exists(path)) {
    # Try to find any version that matches by package-name prefix
    prefix <- sub("_.*$", "", tarball)
    candidates <- list.files(pkg_dir, pattern = paste0("^", prefix, "_.*\\.tar\\.gz$"), full.names = TRUE)
    if (length(candidates) == 0) {
      stop("Tarball not found and no fallback available: ", tarball)
    }
    path <- candidates[[which.max(file.info(candidates)$mtime)]]
    message(sprintf("Note: using fallback tarball %s for %s", basename(path), prefix))
  }
  message(sprintf("Installing %s ...", basename(path)))
  install.packages(path, repos = NULL, type = "source")
}

for (tb in PKG_TARBALLS) {
  install_one(tb)
}

message("\nAll CRAN deps + CVM packages installed. You can now render a dashboard with:")
message("  Rscript render_dashboard.R --course_id 5 --class \"Class of 2026\"")
