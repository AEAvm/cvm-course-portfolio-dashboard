# R/html_components.R
# -----------------------------------------------------------------------------
# Pure-htmltools UI components. Every visual element returns an htmltools tag
# object with inline `style=` attributes — ZERO dependency on Quarto's
# Bootstrap / dashboard classes (they own the card shells, we own what's
# inside). Brand tokens hardcoded per-component so moving a component doesn't
# drag SCSS variables with it.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(htmltools))

# Brand tokens — aligned to the CEO+AVMA design pass.
.CVM <- list(
  navy       = "#003F6B",
  navy_dark  = "#002D4D",
  navy_soft  = "#EEF4FB",      # light tint for info banners / active states
  gold       = "#AB8B00",
  gold_soft  = "rgba(171,139,0,0.20)",
  white      = "#FFFFFF",
  bg         = "#F5F6FA",
  text       = "#1A1A2E",
  muted      = "#5A6472",
  border     = "#E0E4EC",
  success    = "#2E7D32",
  warning    = "#F57C00",
  alert      = "#C62828"
)

.css <- function(...) paste(unlist(list(...), use.names = FALSE), collapse = "")

# ---------------------------------------------------------------------------
# KPI stat cards
# ---------------------------------------------------------------------------
# 32px padding, 48px gold number, 11px muted uppercase label, 12px radius,
# 4px gold left border, subtle shadow, 12px muted subvalue.
# data-kpi identifies which JS function updates the value on filter change.

#' @param size "lg" (default, 48px value) or "sm" (36px value — use when the
#'   card's value is naturally small, e.g. single-digit counts, to keep the
#'   row visually balanced next to wider 3-digit numbers).
ui_kpi_card <- function(label, value, subvalue = NULL, kpi_key = NULL,
                        size = c("lg", "sm")) {
  size <- match.arg(size)
  value_font_size <- if (size == "sm") "36px" else "48px"
  tags$div(
    class = "cvm-kpi",
    `data-kpi` = kpi_key,
    style = .css(
      "background:", .CVM$white, ";",
      "border:1px solid ", .CVM$border, ";",
      "border-left:4px solid ", .CVM$gold, ";",
      "border-radius:12px;",
      "padding:32px;",
      "flex:1 1 200px;",
      "min-width:180px;",
      "box-shadow:0 2px 12px rgba(0,0,0,0.07);",
      "display:flex; flex-direction:column; justify-content:center;",
      "min-height:160px;"
    ),
    tags$div(
      class = "cvm-kpi-label",
      style = .css(
        "color:", .CVM$navy, ";",
        "font-size:11px; font-weight:600;",
        "text-transform:uppercase; letter-spacing:0.08em;",
        "margin-bottom:14px; line-height:1.2;"
      ),
      label
    ),
    tags$div(
      class = "cvm-kpi-value",
      style = .css(
        "color:", .CVM$gold, ";",
        "font-size:", value_font_size, "; font-weight:700; line-height:1;"
      ),
      value
    ),
    if (!is.null(subvalue)) {
      tags$div(
        class = "cvm-kpi-sub",
        style = .css(
          "color:", .CVM$muted, ";",
          "font-size:12px; margin-top:10px; line-height:1.4;"
        ),
        subvalue
      )
    }
  )
}

ui_kpi_row <- function(...) {
  # CSS grid for predictable 4-up (or 2x2) layout — immune to Quarto's
  # html-fill-container flex override that clobbers explicit flex-basis.
  tags$div(
    class = "cvm-kpi-row",
    style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:24px;margin:0 0 24px 0;",
    ...
  )
}

# ---------------------------------------------------------------------------
# Section headers
# ---------------------------------------------------------------------------

ui_section_header <- function(text, level = 3) {
  fn <- switch(as.character(level), "2" = tags$h2, "3" = tags$h3, "4" = tags$h4, tags$h3)
  fn(
    style = .css(
      "font-size:11px;",
      "text-transform:uppercase; letter-spacing:0.08em;",
      "color:", .CVM$muted, ";",
      "font-weight:600;",
      "border-left:2px solid ", .CVM$gold, ";",
      "padding-left:12px;",
      "margin:8px 0 16px 0;"
    ),
    text
  )
}

ui_chart_title <- function(text) {
  tags$h4(
    style = .css(
      "font-size:14px; font-weight:600;",
      "color:", .CVM$navy, ";",
      "margin:0 0 12px 0; letter-spacing:0.01em;"
    ),
    text
  )
}

# ---------------------------------------------------------------------------
# Coverage strip — redesigned: 48px navy number, gold pct, 8px progress bar
# ---------------------------------------------------------------------------

ui_coverage_strip <- function(covered, total, pct, label = "covered") {
  pct_display <- if (is.null(pct) || is.na(pct)) "—" else sprintf("%.1f%%", pct)
  pct_width   <- if (is.null(pct) || is.na(pct)) 0 else max(0, min(100, as.numeric(pct)))

  tags$div(
    style = "margin:12px 0 24px 0; padding:16px 0;",
    tags$div(
      style = "display:flex; align-items:baseline; gap:8px; flex-wrap:wrap;",
      tags$span(
        style = .css(
          "font-size:48px; font-weight:700; color:", .CVM$navy, "; line-height:1;"
        ),
        covered
      ),
      tags$span(
        style = .css("font-size:18px; color:", .CVM$muted, "; font-weight:400;"),
        sprintf("/ %d", total)
      ),
      tags$span(
        style = .css(
          "margin-left:auto; font-size:20px; font-weight:600; color:", .CVM$gold, ";"
        ),
        pct_display
      )
    ),
    tags$div(
      style = .css(
        "font-size:11px; text-transform:uppercase; letter-spacing:0.08em;",
        "color:", .CVM$muted, "; font-weight:500; margin:10px 0 12px 0;"
      ),
      label
    ),
    tags$div(
      style = .css(
        "background:", .CVM$border, ";",
        "height:8px; border-radius:4px; overflow:hidden; width:100%;"
      ),
      tags$div(
        style = sprintf(
          "background:%s; height:100%%; width:%.1f%%; border-radius:4px;",
          .CVM$gold, pct_width
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Tag chips — flex-wrap row, 20px radius, navy-outline pills
# ---------------------------------------------------------------------------

ui_tag_chips <- function(tags_vec, collapse_after = 12L) {
  if (length(tags_vec) == 0) {
    return(tags$p(
      style = .css(
        "color:", .CVM$success, "; font-weight:500; margin:12px 0 0 0;"
      ),
      "Every tag is covered at least once."
    ))
  }

  chip_style <- .css(
    "display:inline-block;",
    "padding:6px 12px;",
    "border:1px solid ", .CVM$navy, ";",
    "border-radius:20px;",
    "color:", .CVM$navy, ";",
    "background:", .CVM$white, ";",
    "font-size:12px; font-weight:500;",
    "line-height:1.4;",
    "white-space:nowrap;"
  )
  chips <- lapply(tags_vec, function(t) tags$span(style = chip_style, t))

  needs_toggle <- length(tags_vec) > collapse_after
  collapsed_h  <- 100

  container_style <- if (needs_toggle) {
    sprintf(
      "display:flex; flex-wrap:wrap; gap:8px; max-height:%dpx; overflow:hidden; transition:max-height 200ms ease;",
      collapsed_h
    )
  } else {
    "display:flex; flex-wrap:wrap; gap:8px;"
  }

  show_all_label  <- sprintf("Show all (%d)", length(tags_vec))
  show_less_label <- "Show less"
  onclick_js <- sprintf(
    paste0(
      "var c=this.previousElementSibling;",
      "var expanded=c.style.maxHeight==='none';",
      "c.style.maxHeight = expanded ? '%dpx' : 'none';",
      "this.textContent = expanded ? '%s' : '%s';"
    ),
    collapsed_h, show_all_label, show_less_label
  )

  tags$div(
    style = "margin-top:4px;",
    do.call(tags$div, c(list(style = container_style), chips)),
    if (needs_toggle) {
      tags$button(
        onclick = onclick_js, type = "button",
        style = .css(
          "margin-top:12px; background:transparent; border:none;",
          "color:", .CVM$navy, ";",
          "font-size:13px; font-weight:500; cursor:pointer; padding:0;",
          "border-bottom:1px dashed ", .CVM$muted, ";"
        ),
        show_all_label
      )
    }
  )
}

ui_deprecated_note <- function(n) {
  if (is.null(n) || n <= 0) return(NULL)
  tags$div(
    style = .css(
      "color:", .CVM$muted, "; font-size:11px; font-style:italic; margin-top:10px;"
    ),
    "Deprecated tags excluded."
  )
}

# ---------------------------------------------------------------------------
# Sidebar — inverted: navy bg, white text, gold accents
# ---------------------------------------------------------------------------

ui_sidebar <- function(course_code,
                       course_name,
                       metadata = character(),
                       tagsets  = character(),
                       extra_sections = list()) {

  heading_style <- .css(
    "font-size:10px; font-weight:600;",
    "text-transform:uppercase; letter-spacing:0.1em;",
    "color:", .CVM$gold, ";",
    "margin:24px 0 10px 0; padding-top:16px;",
    "border-top:1px solid rgba(255,255,255,0.15);"
  )

  bullet_list <- function(items) {
    tags$ul(
      style = "list-style:none; padding-left:0; margin:0;",
      lapply(items, function(t) {
        tags$li(
          style = .css(
            "padding:8px 0 8px 8px;",
            "font-size:13px;",
            "color:", .CVM$white, ";",
            "line-height:1.4; cursor:default;",
            "transition:color 120ms;"
          ),
          onmouseover = sprintf("this.style.color='%s'", .CVM$gold),
          onmouseout  = sprintf("this.style.color='%s'", .CVM$white),
          tags$span(
            style = .css(
              "color:", .CVM$gold, "; font-weight:700; margin-right:10px;"
            ),
            "•"
          ),
          t
        )
      })
    )
  }

  tags$div(
    class = "cvm-sidebar",
    style = .css(
      "background:", .CVM$navy, ";",
      "color:", .CVM$white, ";",
      "padding:20px 18px;",
      "min-height:100vh;",
      "border-radius:0;"
    ),
    # Dynamic per-tab table of contents — populated by the cvm-toc-script
    # block injected into <head> by apply_layout_fixes(). Empty placeholder
    # at render time; client-side JS fills it on DOMContentLoaded and on
    # every Bootstrap tab switch.
    tags$div(id = "cvm-page-toc",
             style = "margin-bottom:24px;"),
    tags$div(
      style = .css(
        "font-size:20px; font-weight:700;",
        "color:", .CVM$gold, ";",
        "letter-spacing:0.02em; line-height:1.1;"
      ),
      course_code
    ),
    tags$div(
      style = .css(
        "font-size:15px; font-weight:400;",
        "color:", .CVM$white, ";",
        "margin-top:4px; line-height:1.3;"
      ),
      course_name
    ),
    tags$div(
      style = .css("height:1px; background:rgba(255,255,255,0.15); margin:18px 0;")
    ),
    if (length(metadata) > 0) {
      tags$div(
        style = .css(
          "font-size:11px; color:rgba(255,255,255,0.75); line-height:1.7; font-weight:400;"
        ),
        lapply(metadata, function(m) tags$div(m))
      )
    },
    if (length(tagsets) > 0) {
      tagList(
        tags$div(style = heading_style, "Tag sets available"),
        bullet_list(tagsets)
      )
    },
    if (length(extra_sections) > 0) {
      lapply(extra_sections, function(s) {
        tagList(
          tags$div(style = heading_style, s$title),
          bullet_list(s$items)
        )
      })
    }
  )
}

# ---------------------------------------------------------------------------
# Period-chip data for the sticky filter bar (Phase C populates the bar)
# ---------------------------------------------------------------------------

#' Build a per-period index from an all_data list (one result per cohort).
#' Returns a list of descriptors ready for jsonlite::toJSON(auto_unbox = TRUE).
#' Each entry has: id, label ("GC26 · Fall Y1"), cohort_label, cohort_slug,
#' cperiod_id, year_semester, median, students, n_tag_sets, gradebook_ok.
build_period_index <- function(all_data) {
  out <- list()
  for (r in all_data) {
    if (is.null(r)) next
    meta <- r$period_meta %||% tibble::tibble()
    cohort_slug_local <- cohort_slug(r$cohort_label)
    # "Class of 2026" -> "GC26" (last two digits of the year per spec)
    yr_full <- sub("^Class of ", "", r$cohort_label)
    yr_short <- substr(yr_full, max(1, nchar(yr_full) - 1), nchar(yr_full))
    gc_label <- paste0("GC", yr_short)

    for (p in r$periods) {
      meta_row <- if (nrow(meta) > 0) meta[meta$cperiod_id == p, , drop = FALSE] else data.frame()
      year_sem <- if (nrow(meta_row) > 0) as.character(meta_row$curriculum_type_name[[1]]) else ""
      short_sem <- format_semester_short(year_sem)
      chip_label <- if (nzchar(short_sem)) sprintf("%s · %s", gc_label, short_sem) else gc_label

      gb <- r$grades_by_period[[as.character(p)]]
      pmed <- if (!is.null(gb) && is.data.frame(gb) && nrow(gb) > 0) {
        stats::median(gb$grade, na.rm = TRUE)
      } else NA_real_
      pstud <- if (!is.null(gb) && is.data.frame(gb) && nrow(gb) > 0) {
        dplyr::n_distinct(gb$id)
      } else 0L

      out[[length(out) + 1]] <- list(
        id            = sprintf("%d_%s", as.integer(p), cohort_slug_local),
        label         = chip_label,
        cohort_label  = r$cohort_label,
        cohort_slug   = cohort_slug_local,
        cperiod_id    = as.integer(p),
        year_semester = year_sem,
        median        = if (is.finite(pmed)) round(pmed, 2) else NULL,
        students      = as.integer(pstud),
        n_tag_sets    = as.integer(r$kpi$n_tag_sets %||% 0L),
        gradebook_ok  = isTRUE(r$gradebook_ok)
      )
    }
  }
  out
}

#' "Year 1 - Fall" → "Fall Y1"; "Year 2 - Spring" → "Spring Y2"; unknown left alone.
format_semester_short <- function(s) {
  if (length(s) == 0 || is.na(s) || !nzchar(s)) return("")
  m <- regmatches(s, regexec("Year\\s*(\\d)\\s*-\\s*(\\S+)", s))[[1]]
  if (length(m) == 3) sprintf("%s Y%s", m[[3]], m[[2]]) else s
}

# ---------------------------------------------------------------------------
# Download row — icon + filename + size + navy outline button
# ---------------------------------------------------------------------------

ui_download_row <- function(label, href, filename, size_bytes, icon = "📄") {
  # CSS grid (not flex) — immune to Quarto's html-fill-container override.
  # Columns: icon | filename+label | size | button.
  tags$div(
    style = .css(
      "display:grid;",
      "grid-template-columns:40px 1fr auto auto;",
      "column-gap:16px;",
      "align-items:center;",
      "padding:14px 18px;",
      "background:", .CVM$white, ";",
      "border:1px solid ", .CVM$border, ";",
      "border-radius:8px;"
    ),
    tags$div(
      style = .css(
        "width:32px; height:32px;",
        "display:inline-flex; align-items:center; justify-content:center;",
        "background:", .CVM$navy_soft, ";",
        "color:", .CVM$navy, ";",
        "border-radius:8px; font-size:16px; font-weight:700;"
      ),
      icon
    ),
    tags$div(
      style = "min-width:0;",
      # Filename — WRAP at word boundaries, no more ellipsis clipping.
      tags$div(
        style = .css(
          "font-weight:600; color:", .CVM$navy, ";",
          "font-size:14px; line-height:1.3;",
          "word-break:break-all; overflow-wrap:anywhere;"
        ),
        filename
      ),
      tags$div(
        style = .css(
          "font-size:11px; color:", .CVM$muted, ";",
          "text-transform:uppercase; letter-spacing:0.05em; margin-top:4px;"
        ),
        label
      )
    ),
    tags$div(
      style = .css(
        "font-size:12px; color:", .CVM$muted, ";",
        "white-space:nowrap; font-variant-numeric:tabular-nums;"
      ),
      fmt_bytes(size_bytes)
    ),
    tags$a(
      href = href, target = "_blank", download = filename,
      style = .css(
        "display:inline-flex; align-items:center; gap:6px;",
        "padding:8px 14px;",
        "background:", .CVM$white, ";",
        "color:", .CVM$navy, ";",
        "border:1px solid ", .CVM$navy, ";",
        "border-radius:6px;",
        "font-size:12px; font-weight:500;",
        "text-decoration:none;",
        "transition:background 120ms, color 120ms;"
      ),
      onmouseover = sprintf("this.style.background='%s';this.style.color='%s'", .CVM$navy, .CVM$white),
      onmouseout  = sprintf("this.style.background='%s';this.style.color='%s'", .CVM$white, .CVM$navy),
      "Download"
    )
  )
}

# ---------------------------------------------------------------------------
# Banners — redesigned: light-blue info per user spec, navy border/icon
# ---------------------------------------------------------------------------

ui_banner <- function(text, variant = c("info", "success", "warn", "alert"), icon = NULL) {
  variant <- match.arg(variant)
  palette <- switch(variant,
    info    = list(bg = .CVM$navy_soft, bar = .CVM$navy,    text = .CVM$navy,  ico = "i"),
    success = list(bg = "#E8F5E9",       bar = .CVM$success, text = "#1B5E20",   ico = "✓"),
    warn    = list(bg = "#FFF3E0",       bar = .CVM$warning, text = "#8A4A00",   ico = "!"),
    alert   = list(bg = "#FFEBEE",       bar = .CVM$alert,   text = "#8E1A1A",   ico = "!")
  )
  tags$div(
    style = .css(
      "display:flex; align-items:flex-start; gap:12px;",
      "padding:14px 18px;",
      "background:", palette$bg, ";",
      "border-left:4px solid ", palette$bar, ";",
      "color:", palette$text, ";",
      "border-radius:8px; font-size:14px; line-height:1.5;",
      "margin:12px 0;"
    ),
    tags$span(
      style = .css(
        "font-size:16px; font-weight:700; line-height:1; flex:0 0 20px;",
        "width:20px; height:20px; border-radius:50%;",
        "background:", palette$bar, "; color:", palette$bg, ";",
        "display:flex; align-items:center; justify-content:center;"
      ),
      icon %||% palette$ico
    ),
    tags$div(style = "flex:1;", HTML(text))
  )
}

# ---------------------------------------------------------------------------
# Plot widget wrapper — forces minimum height
# ---------------------------------------------------------------------------
#
# Quarto's dashboard layout injects `html-fill-container`/`html-fill-item`
# classes onto every htmltools element. When a plotly widget lands inside a
# flex-fill container without a clearly sized ancestor, it can collapse to
# zero height at runtime — the chart is in the DOM but invisible.
#
# ui_plot_box(w, height = 420) wraps the widget in a plain <div> with an
# explicit min-height so the plot always has room to render, even if the
# ancestor chain has a flex-fill that would otherwise zero it out.
ui_plot_box <- function(widget, height = 420) {
  tags$div(
    style = sprintf("min-height:%dpx;width:100%%;", as.integer(height)),
    widget
  )
}
