# layout_fixes.R — runs after every quarto_render() call
# Reads the rendered HTML, applies all byte-level inline-style patches,
# injects the comprehensive professional CSS block, injects the Plotly
# interactivity JS, and saves the file back.
# Called from render_dashboard.R after every render.

apply_layout_fixes <- function(html_path) {
  if (!file.exists(html_path)) {
    warning(sprintf("layout_fixes: file not found: %s", html_path))
    return(invisible(FALSE))
  }

  html_raw <- paste(readLines(html_path, encoding = "UTF-8", warn = FALSE),
                    collapse = "\n")

  # Check if already applied
  if (grepl('id="cvm-layout-fixes"', html_raw, fixed = TRUE)) {
    message("layout_fixes: already applied, skipping")
    return(invisible(TRUE))
  }

  # ── Counters for the log ────────────────────────────────────────────────
  counts <- list()

  # ── Helper: count-and-patch ─────────────────────────────────────────────
  patch <- function(text, pattern, replacement, label, use_perl = FALSE,
                    fixed = FALSE) {
    n <- if (fixed) {
      lengths(regmatches(text, gregexpr(pattern, text, fixed = TRUE)))
    } else {
      lengths(regmatches(text, gregexpr(pattern, text, perl = use_perl)))
    }
    counts[[label]] <<- n
    if (fixed) {
      gsub(pattern, replacement, text, fixed = TRUE)
    } else {
      gsub(pattern, replacement, text, perl = use_perl)
    }
  }

  # ── Layer B patches ─────────────────────────────────────────────────────

  # B1. Histogram card outer div: old hardcoded 360px+overflow:hidden.
  #     Also catches any residual min-height values below the 500px floor.
  html_raw <- patch(html_raw,
    "height:360px;overflow:hidden",
    "min-height:500px;height:auto;overflow:visible",
    "histogram_360_hidden", fixed = TRUE)

  # B2. Histogram card outer div: min-height:340px wrapper (ui_plot_box output)
  html_raw <- patch(html_raw,
    'style="min-height:340px;width:100%;"',
    'style="min-height:460px;width:100%;"',
    "plotbox_340", fixed = TRUE)

  # B3. Plotly widget inner div: width:100%;height:320px (from ggplotly height=320)
  html_raw <- patch(html_raw,
    'style="width:100%;height:320px;"',
    'style="width:100%;height:460px;min-height:460px;"',
    "plotly_320", fixed = TRUE)

  # B4. Embedded QMD style block: grid-auto-rows: 360px !important
  #     (the inline style block injected before </body> by the QMD's raw HTML)
  html_raw <- patch(html_raw,
    "grid-auto-rows: 360px !important",
    "grid-auto-rows: auto !important",
    "grid_auto_rows_360_important", fixed = TRUE)

  # B5. bslib grid row squashing — minmax(3em, 1fr) collapses rows.
  html_raw <- patch(html_raw,
    'grid-template-rows:\\s*minmax\\(3em,\\s*1fr\\)',
    'grid-template-rows: auto',
    "bslib_3em_single", use_perl = TRUE)

  html_raw <- patch(html_raw,
    'grid-template-rows:\\s*repeat\\([0-9]+,\\s*minmax\\(3em,\\s*1fr\\)\\)',
    'grid-template-rows: auto',
    "bslib_3em_repeat", use_perl = TRUE)

  # B6. The long multi-value grid-template-rows emitted by bslib for the
  #     Downloads tab: "auto minmax(3em, 1fr) minmax(3em, 1fr) ..."
  html_raw <- patch(html_raw,
    'grid-template-rows:\\s*auto(?:\\s+minmax\\(3em,\\s*1fr\\))+',
    'grid-template-rows: auto',
    "bslib_3em_multi", use_perl = TRUE)

  html_raw <- patch(html_raw,
    'grid-auto-rows:\\s*minmax\\(0,\\s*1fr\\)',
    'grid-auto-rows: auto',
    "bslib_0_1fr", use_perl = TRUE)

  # B7. Plotly JSON heights: "height":320 serialized inside widget JSON
  html_raw <- patch(html_raw,
    '"height":320',
    '"height":460',
    "plotly_json_320", fixed = TRUE)

  # ── CSS injection before </head> ────────────────────────────────────────
  style_block <- '
<style id="cvm-layout-fixes">
/* === CVM Layout Fixes — injected post-render === */
* { box-sizing: border-box !important; }

/* ── Spacing ─────────────────────────────────────────────────────────── */
p { margin-bottom: 16px !important; line-height: 1.7 !important; }
li { margin-bottom: 8px !important; line-height: 1.6 !important; }
td, th { padding: 10px 14px !important; }
.card-body, .bslib-card-body { padding: 24px !important; }
.card { margin-bottom: 24px !important; }
.cvm-card {
  padding: 28px !important;
  margin-bottom: 40px !important;
  overflow: visible !important;
  height: auto !important;
  min-height: 0 !important;
}
.cvm-card-title {
  margin-bottom: 8px !important;
  font-size: 16px !important;
  font-weight: 600 !important;
  color: #003F6B !important;
  display: block !important;
  white-space: normal !important;
  width: 100% !important;
}
.cvm-card-desc {
  margin-bottom: 20px !important;
  color: #5A6472 !important;
  font-size: 13px !important;
  line-height: 1.5 !important;
  display: block !important;
  white-space: normal !important;
  width: 100% !important;
}
.cvm-narrative p { margin-bottom: 20px !important; line-height: 1.8 !important; }
.cvm-narrative { overflow-y: visible !important; height: auto !important; max-height: none !important; }
.cvm-coverage-row { padding: 14px 0 !important; }
.cvm-chip { padding: 6px 14px !important; margin: 4px 3px !important; }
.cvm-question-card { padding: 16px 20px !important; margin-bottom: 12px !important; }
.cvm-flag-card { padding: 18px 20px !important; margin-bottom: 16px !important; }
.cvm-pdf-row { padding: 14px 0 !important; gap: 16px !important; }
h2, h3, h4 {
  margin-bottom: 12px !important;
  margin-top: 24px !important;
  white-space: normal !important;
  overflow: visible !important;
  word-wrap: break-word !important;
}
section { margin-bottom: 32px !important; }
.tab-content { padding-top: 16px !important; }
.quarto-dashboard .card { margin-bottom: 24px !important; }
.dataTables_wrapper { padding: 8px 0 !important; }

/* ── Overflow unlock ─────────────────────────────────────────────────── */
.cvm-card, .tab-pane { overflow: visible !important; height: auto !important; }

/* ── Histogram grid — PERMANENT FIX ─────────────────────────────────── */
/* These rules must have higher specificity than the QMD-embedded style    */
/* block which emits grid-auto-rows: 360px. The post-render byte patch     */
/* rewrites that value to "auto" in the HTML; these rules are the belt     */
/* over the braces.                                                         */
.cvm-histogram-grid {
  display: grid !important;
  grid-template-columns: repeat(3, 1fr) !important;
  grid-auto-rows: auto !important;
  gap: 20px !important;
}
.cvm-histogram-card {
  min-height: 500px !important;
  height: auto !important;
  overflow: visible !important;
}
.cvm-histogram-card .js-plotly-plot,
.cvm-histogram-card .plotly.html-widget {
  min-height: 460px !important;
  height: auto !important;
  width: 100% !important;
}
/* Inner plotly sizing wrapper — override inline style="min-height:340px" */
.cvm-histogram-card .html-fill-container[style*="min-height"] {
  min-height: 460px !important;
}

@media (max-width: 1100px) {
  .cvm-histogram-grid { grid-template-columns: repeat(2, 1fr) !important; }
}
@media (max-width: 700px) {
  .cvm-histogram-grid { grid-template-columns: 1fr !important; }
}

/* ── Responsive ─────────────────────────────────────────────────────── */
img, iframe, svg, canvas { max-width: 100% !important; }
.plotly.html-widget { width: 100% !important; min-width: 0 !important; }
.dataTables_wrapper { overflow-x: auto !important; width: 100% !important; }
.dataTable { width: 100% !important; min-width: 600px; }

/* ── bslib grid row unlock ──────────────────────────────────────────── */
/* Belt-and-braces: byte patches above rewrite the inline style values,   */
/* but these CSS rules handle any bslib grid the byte patch might miss.   */
.bslib-grid[style*="grid-template-rows"],
.html-fill-container[style*="minmax(3em"] {
  grid-template-rows: auto !important;
  grid-auto-rows: auto !important;
}

/* ── Navbar tabs responsive ─────────────────────────────────────────── */
.quarto-dashboard .navbar-nav .nav-link {
  white-space: nowrap !important;
  font-size: 13px !important;
  padding: 8px 10px !important;
}
@media (max-width: 1200px) {
  .quarto-dashboard .navbar-nav .nav-link {
    font-size: 12px !important;
    padding: 8px 8px !important;
  }
}
@media (max-width: 900px) {
  .quarto-dashboard .navbar-nav { flex-wrap: wrap !important; }
}

/* ── Tag signal cards two-column ────────────────────────────────────── */
div[style*="grid-template-columns:1fr 1fr"] {
  display: grid !important;
  grid-template-columns: 1fr 1fr !important;
  gap: 24px !important;
}
@media (max-width: 800px) {
  div[style*="grid-template-columns:1fr 1fr"] {
    grid-template-columns: 1fr !important;
  }
}

/* ── Survey/feedback card grid ──────────────────────────────────────── */
div[style*="minmax(420px"] {
  display: grid !important;
  grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)) !important;
  gap: 20px !important;
}

/* ── Table headers navy ─────────────────────────────────────────────── */
.cvm-card table.dataTable thead th {
  background-color: #003F6B !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
}
.cvm-card table.dataTable tbody tr:nth-child(even) > td {
  background-color: #F9FAFB !important;
}

/* ── Auto-spacing ───────────────────────────────────────────────────── */
.cvm-filterable { transition: none !important; }
.cvm-filterable[style*="display: none"] { margin: 0 !important; padding: 0 !important; }
.tab-pane > * { height: auto !important; }

/* ── Tag-set section headings (Curriculum coverage cards) ───────────── */
[id^="tagset-"] .cvm-card-title {
  font-size: 22px !important;
  font-weight: 800 !important;
  color: #003F6B !important;
  border-bottom: 3px solid #C9A84C !important;
  padding-bottom: 12px !important;
  margin-bottom: 20px !important;
  display: block !important;
  width: 100% !important;
}

/* ── Sidebar tag-set anchors ────────────────────────────────────────── */
.cvm-sidebar a { color: #C9A84C !important; text-decoration: none !important; }
.cvm-sidebar a:hover { color: #FFFFFF !important; text-decoration: underline !important; }

/* Downloads tab hidden — re-enable when PDFs are added by Yashvi */
.quarto-dashboard .navbar-nav .nav-link[data-bs-target*="downloads"],
.quarto-dashboard .navbar-nav .nav-link[href*="downloads"],
.quarto-dashboard .navbar-nav .nav-item:has(a[href*="downloads"]) {
  display: none !important;
}
</style>'

  # Inject before </head>
  if (grepl("</head>", html_raw, fixed = TRUE)) {
    html_raw <- sub("</head>", paste0(style_block, "\n</head>"), html_raw, fixed = TRUE)
    counts[["css_injected"]] <- 1L
  } else {
    warning("layout_fixes: </head> not found — appending CSS to end of document")
    html_raw <- paste0(html_raw, "\n", style_block)
    counts[["css_injected"]] <- 1L
  }

  # ── Plotly interactivity JS — inject before </body> ─────────────────────
  plotly_js <- '
<script id="cvm-plotly-unlock">
/* CVM Plotly unlock — removes fixedrange so all charts are pan/zoom-able */
(function() {
  function unlockPlotly() {
    if (typeof Plotly === "undefined") return;
    document.querySelectorAll(".js-plotly-plot").forEach(function(el) {
      try {
        var layout = el._fullLayout;
        if (!layout) return;
        var update = {};
        if (layout.xaxis  && layout.xaxis.fixedrange)  update["xaxis.fixedrange"]  = false;
        if (layout.yaxis  && layout.yaxis.fixedrange)  update["yaxis.fixedrange"]  = false;
        if (layout.xaxis2 && layout.xaxis2.fixedrange) update["xaxis2.fixedrange"] = false;
        if (layout.yaxis2 && layout.yaxis2.fixedrange) update["yaxis2.fixedrange"] = false;
        if (Object.keys(update).length > 0) Plotly.relayout(el, update);
      } catch(e) { /* ignore */ }
    });
  }
  /* Run once on DOMContentLoaded, then again after a short delay for
     lazy-rendered Plotly widgets that initialize after page load. */
  document.addEventListener("DOMContentLoaded", function() {
    unlockPlotly();
    setTimeout(unlockPlotly, 1200);
    setTimeout(unlockPlotly, 3000);
  });
})();
</script>'

  if (grepl("</body>", html_raw, fixed = TRUE)) {
    html_raw <- sub("</body>", paste0(plotly_js, "\n</body>"), html_raw, fixed = TRUE)
    counts[["js_injected"]] <- 1L
  } else {
    warning("layout_fixes: </body> not found — appending JS to end of document")
    html_raw <- paste0(html_raw, "\n", plotly_js)
    counts[["js_injected"]] <- 1L
  }

  writeLines(html_raw, html_path, useBytes = FALSE)

  # ── Log summary ──────────────────────────────────────────────────────────
  message(sprintf("layout_fixes: applied to %s", basename(html_path)))
  message(sprintf(
    "  patches — histogram_360_hidden:%d  plotbox_340:%d  plotly_320:%d  grid_auto_rows_360:%d",
    counts[["histogram_360_hidden"]] %||% 0L,
    counts[["plotbox_340"]]          %||% 0L,
    counts[["plotly_320"]]           %||% 0L,
    counts[["grid_auto_rows_360_important"]] %||% 0L
  ))
  message(sprintf(
    "  patches — bslib_3em_single:%d  bslib_3em_repeat:%d  bslib_3em_multi:%d  bslib_0_1fr:%d  plotly_json_320:%d",
    counts[["bslib_3em_single"]] %||% 0L,
    counts[["bslib_3em_repeat"]] %||% 0L,
    counts[["bslib_3em_multi"]]  %||% 0L,
    counts[["bslib_0_1fr"]]      %||% 0L,
    counts[["plotly_json_320"]]  %||% 0L
  ))
  message(sprintf(
    "  injections — CSS:%d  JS:%d",
    counts[["css_injected"]] %||% 0L,
    counts[["js_injected"]]  %||% 0L
  ))

  return(invisible(TRUE))
}
