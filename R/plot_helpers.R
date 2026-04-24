# R/plot_helpers.R
# -----------------------------------------------------------------------------
# Rendering-time plot helpers shared by both Quarto templates.
# These functions return ggplot/plotly objects that htmlwidgets can inline.
# -----------------------------------------------------------------------------

#' ggplot2 theme used for our own (non-cvm.course.portfolio) plots.
cvm_ggplot_theme <- function() {
  ggplot2::theme_minimal(base_family = "") +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major  = ggplot2::element_line(color = "#E4E7EC", linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(color = CVM_PALETTE$text, size = 11),
      axis.title        = ggplot2::element_text(color = CVM_PALETTE$text, size = 12),
      plot.title        = ggplot2::element_text(color = CVM_PALETTE$navy, size = 14, face = "bold"),
      legend.text       = ggplot2::element_text(color = CVM_PALETTE$text, size = 11),
      legend.title      = ggplot2::element_text(color = CVM_PALETTE$text, size = 11, face = "bold")
    )
}

#' Interactive median-trend chart with a dashed-gold 75th-percentile line.
#' Navy line, gold confidence band at 20% opacity per the design spec.
#'
#' Multi-cohort usage: pass `cohort_color = "#2E7D32"` to recolor the line,
#' markers, and confidence ribbon for a specific cohort. Default NULL keeps
#' the single-cohort navy/gold scheme — existing callers unchanged.
trend_plotly <- function(trend_df, title = "Median grade trend", height = 380,
                         cohort_color = NULL) {
  line_color <- cohort_color %||% CVM_PALETTE$navy
  # Confidence ribbon — 15% opacity fill in cohort color (spec) or 20% gold
  # if no cohort_color (unchanged default).
  band_fill <- if (!is.null(cohort_color)) {
    hex_rgba(cohort_color, 0.15)
  } else {
    "rgba(171,139,0,0.20)"
  }
  marker_color <- line_color
  marker_border <- CVM_PALETTE$white
  empty <- function() plotly::plot_ly() |>
    plotly::layout(
      title = list(text = paste(title, "— no data")),
      paper_bgcolor = "white", plot_bgcolor = "white",
      height = height
    )
  if (is.null(trend_df) || nrow(trend_df) == 0) return(empty())
  trend_df$cperiod_id <- as.character(trend_df$cperiod_id)
  plotly::plot_ly(
    trend_df, x = ~cperiod_id,
    hovertemplate = "Period: %{x}<br>Median: %{y:.1f}<br>N students: %{customdata}<extra></extra>",
    customdata = ~n,
    height = height
  ) |>
    # Confidence band — color from cohort_color (or gold default).
    plotly::add_ribbons(
      ymin = ~q25, ymax = ~q75,
      fillcolor = band_fill,
      line = list(color = "transparent"),
      name = "25th–75th percentile", hoverinfo = "skip"
    ) |>
    # Trend line — color from cohort_color (or navy default).
    plotly::add_lines(
      y = ~median,
      line = list(color = line_color, width = 3),
      name = "Median"
    ) |>
    plotly::add_markers(
      y = ~median,
      marker = list(color = marker_color, size = 8,
                    line = list(color = marker_border, width = 2)),
      name = "Median", showlegend = FALSE
    ) |>
    # Dashed gold 75% reference
    plotly::add_lines(
      y = rep(75, nrow(trend_df)),
      line = list(color = CVM_PALETTE$gold, dash = "dash", width = 2),
      name = "75% reference", hoverinfo = "skip"
    ) |>
    plotly::layout(
      title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
      xaxis = list(title = "Curriculum period", gridcolor = "#E4E7EC"),
      yaxis = list(title = "Grade (%)", range = c(0, 100), gridcolor = "#E4E7EC"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
      legend = list(orientation = "h", y = -0.25),
      margin = list(t = 50, b = 80, l = 60, r = 40),
      autosize = TRUE
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

#' Horizontal bar chart of a single tag set's counts. Filters out deprecated
#' taxonomy entries ("Do not use — …"). Returns a list with:
#'   $plot             — the plotly widget, or NULL if no tags remain
#'   $deprecated_count — integer count of tags hidden by the filter
#' so the template can render the "X deprecated tags hidden" note.
tag_bar_plotly <- function(tagset_df, title = "Tag coverage", height = NULL) {
  # Dynamic height based on number of tags (auto-size to avoid squashing).
  empty <- function(msg) list(
    plot = plotly::plot_ly() |>
      plotly::layout(
        title = list(text = paste(title, msg)),
        height = height %||% 380
      ),
    deprecated_count = 0L
  )
  if (is.null(tagset_df) || !is.data.frame(tagset_df) || nrow(tagset_df) == 0) {
    return(empty("— no data"))
  }
  tag_cols <- setdiff(names(tagset_df), c("event_title", "class"))
  if (length(tag_cols) == 0) return(empty("— no tags"))

  deprecated_count <- sum(is_deprecated_tag(tag_cols))
  tag_cols <- tag_cols[!is_deprecated_tag(tag_cols)]
  if (length(tag_cols) == 0) return(empty("— all tags deprecated"))

  totals <- vapply(tagset_df[tag_cols], function(x) sum(x, na.rm = TRUE), numeric(1))
  df <- data.frame(tag = names(totals), count = totals, stringsAsFactors = FALSE)
  df <- df[order(df$count), ]
  df$tag <- factor(df$tag, levels = df$tag)

  # ~22px per tag row + margins, floor 380px, ceiling 1200px
  h <- height %||% max(380, min(1200, 80 + 22 * nrow(df)))

  list(
    plot = plotly::plot_ly(
      df, x = ~count, y = ~tag, type = "bar", orientation = "h",
      marker = list(color = CVM_PALETTE$navy),
      height = h
    ) |>
      plotly::layout(
        title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
        xaxis = list(title = "Event count", gridcolor = "#E4E7EC"),
        yaxis = list(title = "", automargin = TRUE),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin = list(l = 240, r = 40, t = 50, b = 80),
        font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
        autosize = TRUE
      ) |>
      plotly::config(displaylogo = FALSE, responsive = TRUE),
    deprecated_count = deprecated_count
  )
}

#' Faceted histogram of individual student grades by period. Max 3 columns,
#' wraps cleanly. scale_x_continuous uses explicit limits (not coord_equal).
#' Multi-cohort callers pass `cohort_color` to recolor the bars; single-
#' cohort callers unchanged (default navy).
distribution_plot <- function(grades_df, title = "Grade distribution by period",
                              cohort_color = NULL) {
  fill_color <- cohort_color %||% CVM_PALETTE$navy
  if (is.null(grades_df) || nrow(grades_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::ggtitle(paste(title, "— no data")) +
             cvm_ggplot_theme())
  }
  df <- grades_df |>
    dplyr::filter(.data$assessment %in% c("weighted_overall_grade",
                                          "individual_weighted_overall"))
  if (nrow(df) == 0) df <- grades_df
  ggplot2::ggplot(df, ggplot2::aes(x = .data$grade)) +
    ggplot2::geom_histogram(bins = 20, fill = fill_color, color = "white", alpha = 0.85) +
    ggplot2::facet_wrap(~ .data$cperiod_id, scales = "free_y", ncol = 3) +
    ggplot2::labs(title = title, x = "Grade (%)", y = "Students") +
    ggplot2::scale_x_continuous(limits = c(0, 105)) +
    cvm_ggplot_theme()
}

#' Short tag-set labels suitable for Quarto sub-tab pills. The full tag-set
#' identifiers produce tabs like "CBVE Subcompetencies" that wrap badly in
#' the navbar — use these compact forms instead.
PRETTY_TAGSET_LABELS <- c(
  "disciplines"         = "Disciplines",
  "species"             = "Species",
  "objectives"          = "Objectives",
  "pigs"                = "PIGs",
  "avma_competencies"   = "AVMA",
  "avma"                = "AVMA",
  "epas"                = "EPAs",
  "cbve_domains"        = "CBVE-Dom",
  "cbve_competencies"   = "CBVE-Comp",
  "cbve_subcompetencies" = "CBVE-Sub"
)

pretty_tagset_name <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  key <- tolower(trimws(x))
  mapped <- PRETTY_TAGSET_LABELS[key]
  # Fallback for any key not in the table — title-case gsub.
  fallback <- vapply(x, function(xi) tools::toTitleCase(gsub("_", " ", xi)), character(1))
  out <- ifelse(is.na(mapped), fallback, mapped)
  unname(out)
}

#' Minimal markdown → HTML conversion for the narrative pane. Uses commonmark
#' when available, else falls back to paragraph splits.
markdown_to_html <- function(md) {
  if (is.null(md) || !nzchar(md)) return("")
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(commonmark::markdown_html(md))
  }
  paragraphs <- strsplit(md, "\n\n+")[[1]]
  paste0("<p>", paste(paragraphs, collapse = "</p><p>"), "</p>")
}

# ---------------------------------------------------------------------------
# Hex color helpers
# ---------------------------------------------------------------------------

#' Convert "#RRGGBB" + alpha (0–1) → "rgba(r,g,b,a)" string usable by plotly.
hex_rgba <- function(hex, alpha = 1) {
  if (is.null(hex) || !nzchar(hex)) return("rgba(0,0,0,0)")
  h <- sub("^#", "", hex)
  if (nchar(h) == 3L) h <- paste0(substr(h, 1, 1), substr(h, 1, 1),
                                  substr(h, 2, 2), substr(h, 2, 2),
                                  substr(h, 3, 3), substr(h, 3, 3))
  r <- strtoi(substr(h, 1, 2), 16L)
  g <- strtoi(substr(h, 3, 4), 16L)
  b <- strtoi(substr(h, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, alpha)
}

# ---------------------------------------------------------------------------
# Multi-cohort plot wrappers
# ---------------------------------------------------------------------------
# These produce ONE plotly per tab with multiple traces — one per cohort in
# its assigned color. The all-years template uses these for the overview
# tabs. Filter-bar JS toggles traces via Plotly.restyle on chip click.

#' Multi-cohort median-grade trend chart.
#' @param cohort_trends  named list: cohort_label -> tibble(cperiod_id, median, q25, q75, n)
#' @param cohort_colors  named character: cohort_label -> hex color
#' @param period_labels  optional named character: cperiod_id -> pretty label
#'   (e.g. "1" -> "GC23 · Fall Y1"). When supplied, the x-axis shows labels
#'   rotated -45 degrees to prevent overlap. NULL keeps raw cperiod_id.
#' @param height         integer px
trend_plotly_multi <- function(cohort_trends, cohort_colors, title = "Median grade trend",
                               height = 380, period_labels = NULL) {
  cohort_labels <- names(cohort_trends)
  cohort_labels <- cohort_labels[vapply(cohort_trends[cohort_labels],
                                        function(d) !is.null(d) && nrow(d) > 0,
                                        logical(1))]
  if (length(cohort_labels) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(
        title = list(text = paste(title, "— no data"), font = list(color = CVM_PALETTE$navy)),
        paper_bgcolor = "white", plot_bgcolor = "white", height = height
      ))
  }

  # Helper — swap cperiod_id ("34") for its pretty label ("GC26 · Fall Y1").
  relabel <- function(ids) {
    if (is.null(period_labels)) return(as.character(ids))
    vapply(as.character(ids), function(x) {
      out <- period_labels[[x]]
      if (is.null(out) || !nzchar(out)) x else out
    }, character(1))
  }

  p <- plotly::plot_ly(height = height)
  max_n_periods <- 0
  all_x_ids <- character()
  for (lab in cohort_labels) {
    df <- cohort_trends[[lab]]
    df$cperiod_id <- as.character(df$cperiod_id)
    df$x_label    <- relabel(df$cperiod_id)
    if (nrow(df) > max_n_periods) max_n_periods <- nrow(df)
    all_x_ids <- unique(c(all_x_ids, df$cperiod_id))
    col <- unname(cohort_colors[[lab]] %||% CVM_PALETTE$navy)
    ribbon_fill <- hex_rgba(col, 0.15)

    p <- p |>
      plotly::add_ribbons(
        data = df, x = ~x_label,
        ymin = ~q25, ymax = ~q75,
        fillcolor = ribbon_fill,
        line = list(color = "transparent"),
        legendgroup = lab, showlegend = FALSE,
        name = sprintf("%s band", lab),
        hoverinfo = "skip"
      ) |>
      plotly::add_trace(
        data = df, x = ~x_label, y = ~median,
        type = "scatter", mode = "lines+markers",
        line = list(color = col, width = 3),
        marker = list(color = col, size = 8,
                      line = list(color = CVM_PALETTE$white, width = 2)),
        customdata = ~n,
        hovertemplate = sprintf(
          "%s<br>Period: %%{x}<br>Median: %%{y:.1f}<br>N students: %%{customdata}<extra></extra>",
          lab
        ),
        legendgroup = lab, name = lab
      )
  }
  # Dashed gold 75% reference — drawn across the full x-axis.
  if (max_n_periods > 0) {
    ref_x <- relabel(all_x_ids)
    p <- p |>
      plotly::add_trace(
        x = ref_x, y = rep(75, length(ref_x)),
        type = "scatter", mode = "lines",
        line = list(color = CVM_PALETTE$gold, dash = "dash", width = 2),
        name = "75% reference", hoverinfo = "skip", showlegend = TRUE
      )
  }
  p |>
    plotly::layout(
      title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
      xaxis = list(
        title = "Curriculum period",
        gridcolor = "#E4E7EC",
        tickangle = if (!is.null(period_labels)) -45 else 0,
        type = "category",
        automargin = TRUE
      ),
      yaxis = list(title = "Grade (%)", range = c(0, 100), gridcolor = "#E4E7EC"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
      legend = list(orientation = "h", y = -0.3),
      margin = list(t = 50, b = 130, l = 60, r = 40),
      autosize = TRUE
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

#' Multi-cohort "counts below X%" grouped bar chart.
#' @param cohort_thresholds  named list: cohort_label -> tibble(threshold, count)
#'   where threshold is "< 70%" / "< 80%" / "< 90%" per the existing shape.
thresholds_plotly_multi <- function(cohort_thresholds, cohort_colors,
                                    title = "Students below thresholds",
                                    height = 380) {
  labels <- names(cohort_thresholds)
  labels <- labels[vapply(cohort_thresholds[labels],
                          function(d) !is.null(d) && nrow(d) > 0, logical(1))]
  if (length(labels) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(title = list(text = paste(title, "— no data")),
                     paper_bgcolor = "white", plot_bgcolor = "white",
                     height = height))
  }
  p <- plotly::plot_ly(height = height)
  for (lab in labels) {
    df <- cohort_thresholds[[lab]]
    col <- unname(cohort_colors[[lab]] %||% CVM_PALETTE$navy)
    p <- p |>
      plotly::add_bars(
        data = df, x = ~threshold, y = ~count,
        marker = list(color = col),
        name = lab, legendgroup = lab,
        hovertemplate = sprintf("%s<br>%%{x}: %%{y}<extra></extra>", lab)
      )
  }
  p |>
    plotly::layout(
      title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
      xaxis = list(title = "Threshold", gridcolor = "#E4E7EC",
                   type = "category", automargin = TRUE),
      yaxis = list(title = "Students", gridcolor = "#E4E7EC", rangemode = "tozero"),
      barmode = "group",
      bargap = 0.3, bargroupgap = 0.05,
      paper_bgcolor = "white", plot_bgcolor = "white",
      font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
      # Extra bottom margin so a 6-cohort legend doesn't collide with ticks.
      legend = list(orientation = "h", y = -0.28),
      margin = list(t = 50, b = 120, l = 60, r = 40),
      autosize = TRUE
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

#' Multi-cohort grouped horizontal bar chart of tag counts for one tag set.
#' @param cohort_counts named list: cohort_label -> named integer(tag = count)
tag_bar_plotly_multi <- function(cohort_counts, cohort_colors,
                                 title = "Tag coverage",
                                 height = NULL) {
  labels <- names(cohort_counts)
  labels <- labels[vapply(cohort_counts[labels],
                          function(x) !is.null(x) && length(x) > 0, logical(1))]

  empty <- function(msg) list(
    plot = plotly::plot_ly() |>
      plotly::layout(title = list(text = paste(title, msg)),
                     paper_bgcolor = "white", plot_bgcolor = "white",
                     height = height %||% 380),
    deprecated_count = 0L
  )
  if (length(labels) == 0) return(empty("— no data"))

  # Union of tags. Drop deprecated.
  all_tags <- unique(unlist(lapply(cohort_counts[labels], names)))
  if (length(all_tags) == 0) return(empty("— no tags"))
  dep_mask <- is_deprecated_tag(all_tags)
  deprecated_count <- sum(dep_mask)
  all_tags <- all_tags[!dep_mask]
  if (length(all_tags) == 0) return(empty("— all tags deprecated"))

  # Order tags by max count across cohorts (ascending so the biggest is on top).
  score <- vapply(all_tags, function(t) {
    max(vapply(cohort_counts[labels],
               function(x) if (t %in% names(x)) as.numeric(x[[t]]) else 0,
               numeric(1)))
  }, numeric(1))
  all_tags <- all_tags[order(score)]

  h <- height %||% max(380, min(1400, 100 + 24 * length(all_tags)))
  p <- plotly::plot_ly(height = h)
  for (lab in labels) {
    x <- cohort_counts[[lab]]
    y_vals <- vapply(all_tags, function(t) {
      if (t %in% names(x)) as.numeric(x[[t]]) else 0
    }, numeric(1))
    col <- unname(cohort_colors[[lab]] %||% CVM_PALETTE$navy)
    p <- p |>
      plotly::add_bars(
        x = y_vals, y = factor(all_tags, levels = all_tags),
        orientation = "h", marker = list(color = col),
        name = lab, legendgroup = lab,
        hovertemplate = sprintf("%s<br>%%{y}: %%{x}<extra></extra>", lab)
      )
  }
  list(
    plot = p |>
      plotly::layout(
        title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
        xaxis = list(title = "Event count", gridcolor = "#E4E7EC"),
        yaxis = list(title = "", automargin = TRUE),
        barmode = "group",
        paper_bgcolor = "white", plot_bgcolor = "white",
        font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
        margin = list(l = 240, r = 40, t = 50, b = 80),
        legend = list(orientation = "h", y = -0.2),
        autosize = TRUE
      ) |>
      plotly::config(displaylogo = FALSE, responsive = TRUE),
    deprecated_count = deprecated_count
  )
}

#' Multi-cohort tag coverage heatmap.
#' Columns = cohorts; rows = tags; cell = count for that cohort/tag.
#' Each column renders with a separate trace using a white-to-cohort-color
#' scale — so reading down a column shows that cohort's relative coverage in
#' its own brand color. One cohort = falls back to white-to-navy gradient.
#' @param cohort_counts named list: cohort_label -> named integer(tag = count)
tag_heatmap_plotly_multi <- function(cohort_counts, cohort_colors,
                                     title = "Coverage heatmap",
                                     height = NULL) {
  labels <- names(cohort_counts)
  labels <- labels[vapply(cohort_counts[labels],
                          function(x) !is.null(x) && length(x) > 0, logical(1))]
  if (length(labels) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(title = list(text = paste(title, "— no data")),
                     paper_bgcolor = "white", plot_bgcolor = "white",
                     height = height %||% 380))
  }

  all_tags <- unique(unlist(lapply(cohort_counts[labels], names)))
  all_tags <- all_tags[!is_deprecated_tag(all_tags)]
  if (length(all_tags) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(title = list(text = paste(title, "— no tags")),
                     paper_bgcolor = "white", plot_bgcolor = "white",
                     height = height %||% 380))
  }

  h <- height %||% max(420, min(1400, 80 + 24 * length(all_tags)))
  p <- plotly::plot_ly(height = h)
  for (i in seq_along(labels)) {
    lab <- labels[[i]]
    x <- cohort_counts[[lab]]
    z_vals <- vapply(all_tags, function(t) {
      if (t %in% names(x)) as.numeric(x[[t]]) else 0
    }, numeric(1))
    col <- unname(cohort_colors[[lab]] %||% CVM_PALETTE$navy)
    colorscale <- list(list(0, CVM_PALETTE$white), list(1, col))
    p <- p |>
      plotly::add_heatmap(
        x = rep(lab, length(all_tags)),
        y = all_tags,
        z = z_vals,
        colorscale = colorscale,
        showscale = (i == 1),  # single scale is misleading cross-column;
                                # we surface the numbers via hover instead
        xgap = 2, ygap = 1,
        hovertemplate = sprintf("%s<br>%%{y}<br>count: %%{z}<extra></extra>", lab)
      )
  }
  p |>
    plotly::layout(
      title = list(text = title, font = list(color = CVM_PALETTE$navy, size = 14)),
      xaxis = list(title = "", tickangle = 0, automargin = TRUE),
      yaxis = list(title = "", automargin = TRUE, categoryorder = "category ascending"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      font = list(color = CVM_PALETTE$text, family = "Inter, sans-serif"),
      margin = list(l = 240, r = 60, t = 50, b = 80),
      autosize = TRUE
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

#' Convert per-cohort tag_coverage[[ts]] data.frames into the named-integer
#' shape expected by tag_bar_plotly_multi() and tag_heatmap_plotly_multi().
#' Input: tagset_df with event_title column + tag columns. Row with event_title
#' == "Total" is used if present; otherwise columns are summed.
tagset_counts_vec <- function(tagset_df) {
  if (is.null(tagset_df) || !is.data.frame(tagset_df) || nrow(tagset_df) == 0) {
    return(stats::setNames(integer(0), character(0)))
  }
  tag_cols <- setdiff(names(tagset_df), c("event_title", "class"))
  if (length(tag_cols) == 0) return(stats::setNames(integer(0), character(0)))
  total_row <- if ("event_title" %in% names(tagset_df)) {
    tagset_df[tagset_df$event_title == "Total", tag_cols, drop = FALSE]
  } else tagset_df[, tag_cols, drop = FALSE]
  if (nrow(total_row) == 0) total_row <- tagset_df[, tag_cols, drop = FALSE]
  as.integer(vapply(total_row, function(x) sum(as.numeric(x), na.rm = TRUE), numeric(1))) |>
    stats::setNames(tag_cols)
}
