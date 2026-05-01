# Survey Responses

This folder feeds the **Survey Trends** tab on every course dashboard.

## Expected file layout

One CSV per course, named after the course code (no spaces, no punctuation):

```
data/survey_responses/VETM801.csv
data/survey_responses/VETM802.csv
...
```

The dashboard auto-discovers a CSV whose stem matches the course's
sanitized code (e.g. `VETM 801` → `VETM801.csv`).

## Required CSV schema

The CSV MUST contain these four columns, exactly named:

| column           | type      | description                                        |
|------------------|-----------|----------------------------------------------------|
| `cohort`         | character | "Class of YYYY" — must match a dashboard cohort   |
| `question_text`  | character | The survey question prompt as it appears on the form |
| `mean_score`     | numeric   | Cohort mean response on whatever Likert scale was used |
| `n_respondents`  | integer   | Number of students who answered that question      |

One row per (cohort, question) pair. A question is plotted only when at
least two cohorts have answered it.

## How the chart is built

* x-axis = cohort year (parsed from `cohort`)
* y-axis = `mean_score`
* one line per `question_text`
* line colors: each cohort's tick uses the dashboard cohort color

If no CSV exists for the course, the tab shows an instructional message.
If survey PDFs exist but no CSV does, the tab nudges the user to extract
question-level responses into a CSV using this schema.

## Extraction tips

Survey PDFs land in `outputs/{course_code}_{class_slug}/surveys/`. To
populate the trend chart, transcribe each numeric question into a row of
the per-course CSV. A spreadsheet with one column per cohort and one row
per question, exported to long form, also works fine.
