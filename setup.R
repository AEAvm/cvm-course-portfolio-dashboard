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
# 3. Feedback widget — Google Form sink (optional)
# -----------------------------------------------------------------------------
# The dashboards include a "Give Feedback" widget on every tab. It POSTs to a
# Google Form's formResponse endpoint via a hidden HTML form — no API key,
# no sheet permissions, no OAuth. Anyone with the form's URL can submit.
#
# One-time setup:
#   1. Go to https://docs.google.com/forms/create
#   2. Create a form with 4 fields:
#        - Tab          (dropdown)
#        - Course Code  (short text)
#        - Feedback     (paragraph)
#        - Timestamp    (short text)
#   3. Click the three-dot menu -> "Get pre-filled link"
#   4. Fill dummy values and click "Get link" — the URL contains entry.NNNNNNNNNN
#      IDs, one per question.
#   5. Set these in ~/.Renviron (and restart R):
#        CVM_FEEDBACK_FORM_ID         = <id between /forms/d/e/ and /viewform>
#        CVM_FEEDBACK_ENTRY_TAB       = entry.NNNNNNNNNN
#        CVM_FEEDBACK_ENTRY_COURSE    = entry.NNNNNNNNNN
#        CVM_FEEDBACK_ENTRY_FEEDBACK  = entry.NNNNNNNNNN
#        CVM_FEEDBACK_ENTRY_TIMESTAMP = entry.NNNNNNNNNN
#
# When unset, the widget still appears but its submit button is disabled
# and the panel shows the setup hint above.
cli::cli_h2("3. Feedback widget — Google Form (optional)")
if (yn("Configure Google Form env vars now? [y/N]: ")) {
  fid     <- readline("CVM_FEEDBACK_FORM_ID (between /forms/d/e/ and /viewform): ")
  e_tab   <- readline("CVM_FEEDBACK_ENTRY_TAB       (entry.NNNNNNNNNN): ")
  e_crs   <- readline("CVM_FEEDBACK_ENTRY_COURSE    (entry.NNNNNNNNNN): ")
  e_fb    <- readline("CVM_FEEDBACK_ENTRY_FEEDBACK  (entry.NNNNNNNNNN): ")
  e_ts    <- readline("CVM_FEEDBACK_ENTRY_TIMESTAMP (entry.NNNNNNNNNN): ")
  if (nzchar(fid) && nzchar(e_tab)) {
    renv <- path.expand("~/.Renviron")
    lines <- if (file.exists(renv)) readLines(renv) else character()
    lines <- lines[!grepl(paste0(
      "^(CVM_FEEDBACK_FORM_ID|",
      "CVM_FEEDBACK_ENTRY_TAB|CVM_FEEDBACK_ENTRY_COURSE|",
      "CVM_FEEDBACK_ENTRY_FEEDBACK|CVM_FEEDBACK_ENTRY_TIMESTAMP|",
      # Drop legacy Sheets API vars from earlier dashboards.
      "CVM_FEEDBACK_SHEET_ID|CVM_GOOGLE_API_KEY)="), lines)]
    lines <- c(lines,
               paste0("CVM_FEEDBACK_FORM_ID=",         fid),
               paste0("CVM_FEEDBACK_ENTRY_TAB=",       e_tab),
               paste0("CVM_FEEDBACK_ENTRY_COURSE=",    e_crs),
               paste0("CVM_FEEDBACK_ENTRY_FEEDBACK=",  e_fb),
               paste0("CVM_FEEDBACK_ENTRY_TIMESTAMP=", e_ts))
    writeLines(lines, renv)
    cli::cli_alert_success("Feedback widget vars written to {.file ~/.Renviron}. Restart R.")
  } else {
    cli::cli_alert_warning("Skipped — at minimum CVM_FEEDBACK_FORM_ID and CVM_FEEDBACK_ENTRY_TAB are required.")
  }
} else {
  cli::cli_alert_info("Feedback widget will show a setup hint and disable submit until configured.")
}

# -----------------------------------------------------------------------------
# 4. Quarto check
# -----------------------------------------------------------------------------
cli::cli_h2("4. Quarto availability")
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
