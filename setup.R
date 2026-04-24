# setup.R
# -----------------------------------------------------------------------------
# First-time setup for the HTML dashboard. Walks the user through:
#   1. VMH keyring credentials
#   2. OPENAI_API_KEY in .Renviron (optional — for Tab 4 narrative)
#   3. Quarto availability check
#
# No scheduler — rendering is on-demand, not cron-driven.
#
# Usage:
#   Rscript setup.R            (interactive)
#   source("setup.R")          (from inside RStudio)
# -----------------------------------------------------------------------------

source(file.path("R", "utils.R"))

suppressPackageStartupMessages({
  library(cli); library(keyring)
})

cli::cli_h1("CVM Course Portfolio HTML Dashboard — Setup")

yn <- function(prompt, default = "n") {
  ans <- readline(prompt)
  if (!nzchar(ans)) ans <- default
  tolower(substring(ans, 1, 1)) == "y"
}

# -----------------------------------------------------------------------------
# 1. VMH keyring credentials
# -----------------------------------------------------------------------------
cli::cli_h2("1. VMH database credentials")
if (yn("Configure VMH credentials now? [y/N]: ")) {
  server <- readline("VMH hostname (e.g. vmh-db.cvm.arizona.edu): ")
  if (nzchar(server)) {
    tryCatch(vmh::set_vmh_env(server, overwrite = TRUE),
             error = function(e) cli::cli_alert_danger("set_vmh_env failed: {conditionMessage(e)}"))
  }
  svc <- readline("VMH keyring service name [vmh_db]: ")
  if (!nzchar(svc)) svc <- "vmh_db"
  tryCatch(vmh::set_vmh_keyring_env(svc, overwrite = TRUE),
           error = function(e) cli::cli_alert_danger("set_vmh_keyring_env failed: {conditionMessage(e)}"))
  user <- readline("VMH username: ")
  pwd  <- if (requireNamespace("getPass", quietly = TRUE)) {
    getPass::getPass("VMH password: ")
  } else {
    readline("VMH password (visible — consider installing {getPass}): ")
  }
  tryCatch(vmh::set_keyring(svc, user, pwd),
           error = function(e) cli::cli_alert_danger("set_keyring failed: {conditionMessage(e)}"))
  cli::cli_alert_success("VMH credentials stored.")
}

# -----------------------------------------------------------------------------
# 2. OpenAI API key
# -----------------------------------------------------------------------------
cli::cli_h2("2. OpenAI API key (optional — Tab 4 narrative)")
if (yn("Add OPENAI_API_KEY to .Renviron now? [y/N]: ")) {
  key <- readline("Paste OpenAI key (starts with sk-): ")
  if (nzchar(key)) {
    renv <- path.expand("~/.Renviron")
    lines <- if (file.exists(renv)) readLines(renv) else character()
    lines <- lines[!grepl("^OPENAI_API_KEY=", lines)]
    lines <- c(lines, paste0("OPENAI_API_KEY=", key))
    writeLines(lines, renv)
    cli::cli_alert_success("OPENAI_API_KEY written to {.file ~/.Renviron}. Restart R.")
  }
}

# -----------------------------------------------------------------------------
# 3. Quarto check
# -----------------------------------------------------------------------------
cli::cli_h2("3. Quarto availability")
quarto_path <- Sys.which("quarto")
if (nzchar(quarto_path)) {
  v <- tryCatch(system2("quarto", "--version", stdout = TRUE, stderr = TRUE),
                error = function(e) "")
  cli::cli_alert_success("Found Quarto at {.path {quarto_path}} (version {v})")
} else {
  cli::cli_alert_warning(paste(
    "Quarto not on PATH. Install from {.url https://quarto.org/docs/get-started/}",
    "before running render_dashboard.R."
  ))
}

cli::cli_h1("Setup complete")
cli::cli_alert_info("Render one course with: {.code Rscript render_dashboard.R --course_id 5 --class \"Class of 2026\"}")
