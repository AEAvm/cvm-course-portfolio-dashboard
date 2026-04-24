# CVM Course Portfolio HTML Dashboard

Renders **self-contained Quarto HTML dashboards** for each course — one per
graduating class plus a `COMBINED` dashboard covering that course's full
history. Wraps the existing CVM course portfolio R packages without
modifying them. Every render is ahead-of-time: data is pulled from Elentra
when you run the render script, then the resulting HTML file is static and
puts **zero load on the database** when faculty open it.

## Why this architecture

An earlier iteration (kept at `cvm-course-portfolio-dashboard-archived/`
alongside this repo for reference) attempted a live-query approach. That put
real load on the Elentra production database — a persistent connection pool,
concurrent heavy queries from `cvm.gradebook::gradebook()` during meetings,
and structural 30–90 s latency that made it unusable during live reviews.
The weekly-refreshed static HTML approach replaces it:

- **Render once before a meeting**, share the HTML via Box — zero DB load during use.
- **Each course has its own folder** with 6 per-year HTMLs + one `COMBINED.html`.
- **Shared `lib/` folder per course** (plotly, DT, Bootstrap assets) dedupes
  ~5 MB of JS across the 7 dashboards in each folder — total footprint is
  ~200–300 MB across all 88 courses, not ~3 GB.

## Layout

```
outputs/
├── VETM801_Foundations/
│   ├── VETM801_Class_of_2023.html
│   ├── VETM801_Class_of_2024.html
│   ├── ...
│   ├── VETM801_COMBINED.html
│   ├── VETM801_class_of_2026_workbooks.zip   (if --with-exports)
│   ├── course-review-presentation-*.pptx      (if --with-exports)
│   └── lib/                                   (shared assets for this course)
│       ├── plotly-binding-*/
│       ├── htmlwidgets-*/
│       ├── bootstrap-5.x/
│       ├── quarto-html/
│       └── custom.scss
└── VETM802_Professional_Skills/
    └── ...
```

## Prerequisites

- **R ≥ 4.2** (tested on 4.5.1)
- **Quarto ≥ 1.4** — https://quarto.org/docs/get-started/
- **VMH database access** via `vmh` keyring
- **OpenAI API key** (optional, for Tab 4 narrative)
- **Box Drive** signed in (for access to package tarballs + course-ID CSVs,
  and for writing final HTMLs to the shared output folder if desired)

## Installation

```r
# 1. Install CRAN deps
install.packages("renv")
renv::restore()

# 2. Install the CVM in-house packages from Box-synced tarballs
source("install_packages.R")

# 3. First-time setup — keyring + OpenAI key + Quarto check
source("setup.R")
```

## Usage

Render **one course, one class**:

```bash
Rscript render_dashboard.R --course_id 5 --class "Class of 2026"
Rscript render_dashboard.R --course_id 5 --class "Class of 2026" --with-exports
Rscript render_dashboard.R --course_id 5 --class "Class of 2026" --skip-narrative
```

Render **one course, all active classes** (COMBINED view):

```bash
Rscript render_combined.R --course_id 5
```

Render **every course for a single class**:

```bash
Rscript render_all.R --class "Class of 2027"
Rscript render_all.R --class "Class of 2027" --include-combined --with-exports
```

`render_all.R` is intentionally scoped to **one class per invocation**. It
does not iterate across classes, and it does not parallelize. If you want
all 88 × 7 dashboards, wrap a shell loop around it and walk away for a
while — do not try to batch everything at once.

## How shared `lib/` works

Each `render_*.R` script:

1. Runs `run_course_pipeline()` against VMH, captures the result to a
   staging RDS under `outputs/{course}/.render_cache/`.
2. Calls `quarto::quarto_render()` with `embed-resources: false`. Quarto
   writes the HTML plus a sibling `{stem}_files/libs/` tree with ~5 MB of
   JS assets.
3. Calls `consolidate_libs()` which **moves** each `{stem}_files/libs/*`
   subtree into a single `outputs/{course}/lib/` and rewrites every HTML in
   the course dir so references go to `lib/` instead of `{stem}_files/libs/`.

The first dashboard rendered for a course pays the ~5 MB cost of copying
plotly / Bootstrap / htmlwidgets into `lib/`. Every subsequent render in
that course folder reuses those assets. The resulting HTML files are
~50–200 KB each and rely on `lib/` being present alongside them — so move
the **whole course folder** when you share.

## Gradebook latency — degraded-data mode

`cvm.gradebook::gradebook()` is slow (30–90 s per period) and emits failures
via `message()` rather than `stop()`. The pipeline wraps it with
`withCallingHandlers`:

- If gradebook data comes back: dashboard renders normally.
- If gradebook data fails or is empty: `result$gradebook_ok = FALSE`, the
  Overview tab shows a warning banner, assessment plots show a "no data"
  placeholder, and the tag-coverage / cross-year tabs render normally.

Check `logs/render.log` after a render for timestamps on every pipeline step.

## Offline fonts

`www/custom.scss` references the Inter font family, falling back to the
system sans-serif stack (SF Pro / Segoe UI / Roboto). The fallback looks
great without any setup. For true offline rendering with Inter specifically:

1. Download Inter woff2 files from https://rsms.me/inter/ into `www/fonts/`.
2. Uncomment the `@font-face` blocks at the top of `www/custom.scss`.
3. Re-render — fonts get copied into each course's `lib/fonts/` automatically.

## Cohort gating

`R/utils.R::active_cohorts()` filters `COHORT_START_DATES` against
`Sys.Date()`. Class of 2029 appears automatically on **2026-08-01**; until
then it is filtered out of `render_combined.R` and `render_all.R`. Add a
new class by appending a row to `COHORT_START_DATES` and dropping the
matching CSV into `data/course_ids/`.

## Output destination

Renders write to `outputs/` by default. To publish to Box manually:

```bash
cp -r outputs/VETM801_Foundations \
  "$HOME/Library/CloudStorage/Box-Box/DS and Kadian/cvm-course-portfolio-dashboard-outputs/"
```

The render scripts never touch Box automatically — that is a deliberate
guardrail. Copy when ready.

## Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `quarto not found` | Quarto not on PATH | Install from https://quarto.org |
| `VMH connect FAILED` | VPN off or `VMH_SERVER` unset | `source("setup.R")` and reconnect VPN |
| Tab 4 shows placeholder | No OpenAI key | Add `OPENAI_API_KEY` to `~/.Renviron` |
| HTML opens but plots missing | Course folder copied without its `lib/` | Copy the whole course folder, including `lib/` |
| Rendering hangs for >3 min | `cvm.gradebook::gradebook()` query | Look in `logs/render.log` for the `gradebook_data ... start` line that has no matching completion line |
| `Gradebook data unavailable` banner | Normal — see degraded-data mode above | Tag coverage tabs still work; rerun with fresh VMH session |

## Architecture

- `R/utils.R`              — cohort gating, palette, date helpers
- `R/data_layer.R`         — course CSV loader + VMH pool connect
- `R/portfolio_runner.R`   — `run_course_pipeline()`, withCallingHandlers for gradebook
- `R/comparison_engine.R`  — cross-cohort overlay, deltas, change narrative
- `R/ai_interpret.R`       — cvm.openAI JSON-prompt wrapper → narrative/flags/questions
- `R/plot_helpers.R`       — plotly/ggplot2 builders
- `R/render_helpers.R`     — course_folder(), consolidate_libs(), CLI parser
- `templates/course_year_dashboard.qmd`     — per-year Quarto dashboard
- `templates/course_combined_dashboard.qmd` — combined all-years dashboard
- `render_dashboard.R`     — one course × one class
- `render_combined.R`      — one course × all classes
- `render_all.R`           — every course × one class

## License

MIT — see `LICENSE`.
