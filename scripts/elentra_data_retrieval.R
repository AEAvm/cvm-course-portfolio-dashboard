###############################################################################
#  elentra_data_retrieval.R
#  -------------------------------------------------------------------------
#  Full SQL-based retrieval of 6 Elentra report types via the vmh R package.
#  Connects directly to the elentra_me MariaDB on AWS RDS.
#
#  Report types covered:
#    1. ITER Form Response Report
#    2. EPA Form Response Report
#    3. End-of-Course Survey
#    4. VETM 840 Gradebook
#    5. Final Exams – Item Response Report
#    6. Course Reports (any form-based report by form_id)
#
#  Prerequisites:
#    - vmh package installed (library(vmh))
#    - keyring credentials stored for service "vmh_db"
#      (run keyring::key_set("vmh_db", username = "<your_user>") once)
#    - VPN connected OR on UA network
#
#  Usage:
#    Source the whole file, then call individual functions.
#    Each function returns a tibble (data frame) you can inspect / export.
#
#  Author : Ndey Isatou JOobe
#  Created: 2026-05-07
###############################################################################

library(vmh)
library(DBI)
library(dplyr)
library(tibble)

# =============================================================================
# 0.  CONNECTION HELPERS
# =============================================================================

#' Open a connection to elentra_me (production)
open_elentra_me <- function(...) {
  vmh::start_connection(db = "me", ...)
}

#' Open a connection to elentra_auth (for user/student identity lookups)
open_elentra_auth <- function(...) {
  vmh::start_connection(db = "mea", ...)
}

#' Shortcut: run a parameterised query and return a tibble
#' Uses dbGetQuery() with params= so it works with pool objects returned
#' by vmh::start_connection(). dbSendQuery() + dbBind() is NOT supported
#' on pool objects and will throw "Not supported for pool objects".
run_query <- function(con, sql, params = NULL) {
  if (!is.null(params)) {
    result <- tibble::as_tibble(DBI::dbGetQuery(con, sql, params = params))
  } else {
    result <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
  }
  return(result)
}


# =============================================================================
# 1.  DISCOVERY HELPERS  — find form_ids, course_ids, etc.
# =============================================================================
#
#  You may not know the exact form_id or course_id for the report you want.
#  These helpers let you search by keyword so you can find the right IDs first.
#

#' Search for forms by title keyword
#' @param con  elentra_me connection
#' @param keyword  text to match (case-insensitive LIKE)
#' @return tibble with form_id, title, description, created_date
find_forms <- function(con, keyword) {
  sql <- "
    SELECT f.form_id,
           f.title,
           f.description,
           FROM_UNIXTIME(f.created_date) AS created_date
      FROM cbl_assessments_lu_forms AS f
     WHERE f.title LIKE ?
       AND f.deleted_date IS NULL
     ORDER BY f.form_id DESC
  "
  run_query(con, sql, params = list(paste0("%", keyword, "%")))
}

#' Search for courses by keyword
#' @param con  elentra_me connection
#' @param keyword  text to match against course_name or course_code
#' @return tibble with course_id, course_name, course_code, organisation_id
find_courses <- function(con, keyword) {
  sql <- "
    SELECT c.course_id,
           c.course_name,
           c.course_code,
           c.organisation_id
      FROM courses AS c
     WHERE (c.course_name LIKE ? OR c.course_code LIKE ?)
       AND c.course_active = 1
     ORDER BY c.course_name
  "
  run_query(con, sql, params = list(
    paste0("%", keyword, "%"),
    paste0("%", keyword, "%")
  ))
}

#' List all distributions (assessment events) for a given form
#' @param con      elentra_me connection
#' @param form_id  the form identifier
#' @return tibble of distribution info
find_distributions_for_form <- function(con, form_id) {
  sql <- "
    SELECT da.dassessment_id,
           da.form_id,
           da.adistribution_id,
           da.assessor_type,
           da.assessor_value,
           FROM_UNIXTIME(da.start_date)   AS start_date,
           FROM_UNIXTIME(da.end_date)     AS end_date,
           FROM_UNIXTIME(da.created_date) AS created_date
      FROM cbl_distribution_assessments AS da
     WHERE da.form_id = ?
       AND da.deleted_date IS NULL
     ORDER BY da.created_date DESC
  "
  run_query(con, sql, params = list(form_id))
}


# =============================================================================
# 2.  FORM METADATA  — structure of any form (items, rubrics, scales)
# =============================================================================

#' Get all elements of a form in display order
#' @param con      elentra_me connection
#' @param form_id  the form identifier
#' @return tibble: afelement_id, element_id, element_type, element_text,
#'         rubric_id, order, allow_comments, etc.
get_form_elements <- function(con, form_id) {
  sql <- "
    SELECT e.afelement_id,
           e.form_id,
           e.element_id,
           e.element_type,
           e.element_text,
           e.rubric_id,
           e.`order`,
           e.allow_comments,
           e.enable_flagging
      FROM cbl_assessment_form_elements AS e
     WHERE e.form_id = ?
       AND e.deleted_date IS NULL
     ORDER BY e.`order`
  "
  run_query(con, sql, params = list(form_id))
}

#' Get rubric metadata (title, description)
#' @param con        elentra_me connection
#' @param rubric_id  rubric identifier
get_rubric <- function(con, rubric_id) {
  sql <- "
    SELECT r.rubric_id,
           r.rubric_title,
           r.rubric_description
      FROM cbl_assessments_lu_rubrics AS r
     WHERE r.rubric_id = ?
  "
  run_query(con, sql, params = list(rubric_id))
}

#' Get all items inside a rubric, with their item metadata
#' @param con        elentra_me connection
#' @param rubric_id  rubric identifier
#' @return tibble with item_id, item_text, item_code, itemtype_id, rating_scale_id, order
get_rubric_items <- function(con, rubric_id) {
  sql <- "
    SELECT ri.arubric_item_id,
           ri.rubric_id,
           ri.item_id,
           ri.`order`,
           i.item_text,
           i.item_code,
           i.itemtype_id,
           i.rating_scale_id,
           i.comment_type
      FROM cbl_assessment_rubric_items AS ri
      JOIN cbl_assessments_lu_items    AS i ON i.item_id = ri.item_id
     WHERE ri.rubric_id = ?
       AND ri.deleted_date IS NULL
     ORDER BY ri.`order`
  "
  run_query(con, sql, params = list(rubric_id))
}

#' Get item metadata (for standalone items not inside rubrics)
#' @param con      elentra_me connection
#' @param item_id  item identifier
get_item <- function(con, item_id) {
  sql <- "
    SELECT i.item_id,
           i.item_text,
           i.item_code,
           i.itemtype_id,
           i.rating_scale_id,
           i.comment_type
      FROM cbl_assessments_lu_items AS i
     WHERE i.item_id = ?
  "
  run_query(con, sql, params = list(item_id))
}

#' Get the rating scale definition
#' @param con              elentra_me connection
#' @param rating_scale_id  scale identifier
get_rating_scale <- function(con, rating_scale_id) {
  sql <- "
    SELECT s.rating_scale_id,
           s.rating_scale_title,
           s.rating_scale_type
      FROM cbl_assessment_rating_scale AS s
     WHERE s.rating_scale_id = ?
  "
  run_query(con, sql, params = list(rating_scale_id))
}

#' Get all response options for a rating scale (the actual choices)
#' @param con              elentra_me connection
#' @param rating_scale_id  scale identifier
#' @return tibble with `order` (the value recorded), `text` (label), etc.
get_rating_scale_responses <- function(con, rating_scale_id) {
  sql <- "
    SELECT r.rating_scale_response_id,
           r.rating_scale_id,
           r.`text`,
           r.`order`,
           r.flag_response
      FROM cbl_assessment_rating_scale_responses AS r
     WHERE r.rating_scale_id = ?
     ORDER BY r.`order`
  "
  run_query(con, sql, params = list(rating_scale_id))
}

#' Get possible response options for a specific item
#' @param con      elentra_me connection
#' @param item_id  item identifier
get_item_responses <- function(con, item_id) {
  sql <- "
    SELECT ir.iresponse_id,
           ir.item_id,
           ir.`text`,
           ir.`order`,
           ir.flag_response
      FROM cbl_assessments_lu_item_responses AS ir
     WHERE ir.item_id = ?
     ORDER BY ir.`order`
  "
  run_query(con, sql, params = list(item_id))
}


# =============================================================================
# 3.  CORE: GET FORM RESPONSES  (works for ITER, EPA, End-of-Course, etc.)
# =============================================================================
#
#  This is the main workhorse function. Every form-based report in Elentra
#  (ITER, EPA, Course Eval, Plus/Delta, etc.) stores data the same way:
#
#    cbl_distribution_assessments  (which assessments use this form)
#      -> cbl_assessment_progress  (one row per completed form submission)
#        -> cbl_assessment_progress_responses  (individual answers)
#          -> cbl_assessments_lu_item_responses (what the answer text/value is)
#
#  The function below pulls ALL completed responses for a given form_id,
#  optionally filtered by date range, and joins in human-readable labels.
#

#' Retrieve all completed form responses for a given form
#'
#' @param con        elentra_me connection
#' @param form_id    the form identifier (find it with find_forms())
#' @param date_from  (optional) UNIX timestamp or "YYYY-MM-DD" — only include
#'                   submissions on or after this date
#' @param date_to    (optional) UNIX timestamp or "YYYY-MM-DD" — only include
#'                   submissions on or before this date
#' @param status     progress status filter: "complete" (default), "inprogress", or "all"
#' @return tibble with one row per response per submission, including:
#'         aprogress_id, assessor_value, target_record_id, form element info,
#'         selected response text/order, and any comments
get_form_responses <- function(con, form_id,
                               date_from = NULL, date_to = NULL,
                               status = "complete") {

  # -- Build the date filter clause dynamically --
  date_clause <- ""
  if (!is.null(date_from)) {
    if (is.character(date_from)) {
      date_clause <- paste0(date_clause,
                            " AND p.created_date >= UNIX_TIMESTAMP('", date_from, "')")
    } else {
      date_clause <- paste0(date_clause,
                            " AND p.created_date >= ", as.integer(date_from))
    }
  }
  if (!is.null(date_to)) {
    if (is.character(date_to)) {
      date_clause <- paste0(date_clause,
                            " AND p.created_date <= UNIX_TIMESTAMP('", date_to, "')")
    } else {
      date_clause <- paste0(date_clause,
                            " AND p.created_date <= ", as.integer(date_to))
    }
  }

  # -- Status filter --
  status_clause <- ""
  if (status == "complete") {
    status_clause <- " AND p.progress_value = 'complete'"
  } else if (status == "inprogress") {
    status_clause <- " AND p.progress_value = 'inprogress'"
  }
  # status == "all" => no filter

  sql <- paste0("
    SELECT
        p.aprogress_id,
        p.dassessment_id,
        p.assessor_type,
        p.assessor_value,
        p.target_record_id,
        p.target_type,
        p.progress_value                           AS submission_status,
        FROM_UNIXTIME(p.created_date)              AS submitted_at,

        -- Form element info
        fe.afelement_id,
        fe.`order`                                 AS element_order,
        fe.element_text                            AS element_label,
        fe.rubric_id,

        -- The item that was answered
        i.item_id,
        i.item_text                                AS question_text,
        i.itemtype_id,

        -- The response the user selected
        ir.iresponse_id,
        ir.`text`                                  AS response_text,
        ir.`order`                                 AS response_value,

        -- Free-text comments (if any)
        pr.comments                                AS response_comments

    FROM cbl_distribution_assessments AS da

    JOIN cbl_assessment_progress AS p
      ON p.dassessment_id = da.dassessment_id

    JOIN cbl_assessment_progress_responses AS pr
      ON pr.aprogress_id = p.aprogress_id

    JOIN cbl_assessments_lu_item_responses AS ir
      ON ir.iresponse_id = pr.iresponse_id

    JOIN cbl_assessments_lu_items AS i
      ON i.item_id = ir.item_id

    LEFT JOIN cbl_assessment_form_elements AS fe
      ON fe.form_id = da.form_id
     AND (   fe.element_id = i.item_id
          OR fe.element_id = pr.afelement_id)
     AND fe.deleted_date IS NULL

    WHERE da.form_id = ", form_id, "
      AND da.deleted_date IS NULL
      AND p.deleted_date IS NULL",
                status_clause,
                date_clause, "

    ORDER BY p.aprogress_id, fe.`order`, ir.`order`
  ")

  run_query(con, sql)
}


# =============================================================================
# 4.  REPORT TYPE 1: ITER FORM RESPONSES
# =============================================================================
#
#  ITERs (In-Training Evaluation Reports) are assessment forms filled out by
#  clinical supervisors about students during rotations.
#
#  Step 1: Find the ITER form_id(s) using find_forms()
#  Step 2: Pull all responses with get_form_responses()
#

#' Retrieve ITER form responses
#'
#' @param con        elentra_me connection
#' @param date_from  optional start date "YYYY-MM-DD"
#' @param date_to    optional end date "YYYY-MM-DD"
#' @return tibble of all ITER responses
get_iter_responses <- function(con, date_from = NULL, date_to = NULL) {

  iter_forms <- find_forms(con, "ITER")
  cat("Found", nrow(iter_forms), "ITER form(s):\n")
  print(iter_forms[, c("form_id", "title")])
  cat("\n")

  if (nrow(iter_forms) == 0) {
    warning("No ITER forms found. Try find_forms(con, 'ITER') to search manually.")
    return(tibble())
  }

  all_responses <- lapply(iter_forms$form_id, function(fid) {
    cat("  Pulling responses for form_id =", fid, "...\n")
    df <- get_form_responses(con, form_id = fid,
                             date_from = date_from, date_to = date_to)
    df$form_id    <- fid
    df$form_title <- iter_forms$title[iter_forms$form_id == fid]
    return(df)
  })

  result <- bind_rows(all_responses)
  cat("Total ITER responses:", nrow(result), "\n")
  return(result)
}


# =============================================================================
# 5.  REPORT TYPE 2: EPA FORM RESPONSES
# =============================================================================
#
#  EPAs (Entrustable Professional Activities) track student competency
#  in clinical skills. Same form-response structure as ITERs.
#

#' Retrieve EPA form responses
#'
#' @param con        elentra_me connection
#' @param date_from  optional start date "YYYY-MM-DD"
#' @param date_to    optional end date "YYYY-MM-DD"
#' @return tibble of all EPA responses
get_epa_responses <- function(con, date_from = NULL, date_to = NULL) {

  epa_forms <- find_forms(con, "EPA")
  cat("Found", nrow(epa_forms), "EPA form(s):\n")
  print(epa_forms[, c("form_id", "title")])
  cat("\n")

  if (nrow(epa_forms) == 0) {
    warning("No EPA forms found. Try find_forms(con, 'EPA') to search manually.")
    return(tibble())
  }

  all_responses <- lapply(epa_forms$form_id, function(fid) {
    cat("  Pulling responses for form_id =", fid, "...\n")
    df <- get_form_responses(con, form_id = fid,
                             date_from = date_from, date_to = date_to)
    df$form_id    <- fid
    df$form_title <- epa_forms$title[epa_forms$form_id == fid]
    return(df)
  })

  result <- bind_rows(all_responses)
  cat("Total EPA responses:", nrow(result), "\n")
  return(result)
}


# =============================================================================
# 6.  REPORT TYPE 3: END-OF-COURSE SURVEY / COURSE EVALUATIONS
# =============================================================================
#
#  Course evaluations (e.g., "801 End of Course Survey", "801 Foundations
#  Course Eval") are also stored as form responses. Search for them by keyword.
#

#' Retrieve end-of-course survey responses
#'
#' @param con        elentra_me connection
#' @param keyword    search term (default "Course Eval" — also try "End of Course")
#' @param date_from  optional start date "YYYY-MM-DD"
#' @param date_to    optional end date "YYYY-MM-DD"
#' @return tibble of all matching survey responses
get_course_eval_responses <- function(con, keyword = "Course Eval",
                                      date_from = NULL, date_to = NULL) {

  eval_forms <- find_forms(con, keyword)
  cat("Found", nrow(eval_forms), "form(s) matching '", keyword, "':\n")
  print(eval_forms[, c("form_id", "title")])
  cat("\n")

  if (nrow(eval_forms) == 0) {
    warning("No forms found. Try different keywords like 'End of Course', 'Survey', etc.")
    return(tibble())
  }

  all_responses <- lapply(eval_forms$form_id, function(fid) {
    cat("  Pulling responses for form_id =", fid, "...\n")
    df <- get_form_responses(con, form_id = fid,
                             date_from = date_from, date_to = date_to)
    df$form_id    <- fid
    df$form_title <- eval_forms$title[eval_forms$form_id == fid]
    return(df)
  })

  result <- bind_rows(all_responses)
  cat("Total survey responses:", nrow(result), "\n")
  return(result)
}


# =============================================================================
# 7.  REPORT TYPE 4: GRADEBOOK (e.g., VETM 840)
# =============================================================================
#
#  Gradebook data in this Elentra instance uses:
#    - assessments          (assessment definitions per course — name, type,
#                            grade_weighting, order, due_date, cperiod_id)
#    - assessment_grades    (one row per student per assessment — the score)
#    - curriculum_periods   (maps cperiod_id to cohort/class year label)
#
#  NOTE: `gradebook_assessments` does NOT exist in this instance.
#        `cvm_course_grades` exists but has no rows for VETM 840 (course_id=30).
#        The correct path is assessments -> assessment_grades.
#
#  NOTE: grade_weighting = 0 for all VETM 840 assessments — weights are not
#        stored in Elentra for this course. grade_value is the raw score.
#
#  NOTE: The gradebook URL pattern is:
#    /admin/gradebook?section=view&id=<course_id>
#  So VETM840 at id=30 means course_id = 30.
#

#' Get all gradebook entries for a course
#'
#' @param con        elentra_me connection
#' @param course_id  the course identifier (from the Elentra URL: ?id=30)
#' @return tibble with one row per student per assessment, including
#'         assessment_name, assessment_type, grade_weighting, period_title,
#'         proxy_id, grade_value, and updated_at
get_gradebook <- function(con, course_id) {

  # -- Step 1: Verify the course exists --
  course_info <- run_query(con, paste0(
    "SELECT course_id, course_name, course_code
       FROM courses
      WHERE course_id = ", course_id
  ))
  if (nrow(course_info) == 0) {
    warning("Course ID ", course_id, " not found.")
    return(tibble())
  }
  cat("Gradebook for:", course_info$course_name, "(", course_info$course_code, ")\n\n")

  # -- Step 2: Get all active assessments for this course --
  #    Joins curriculum_periods to get a human-readable cohort/period label.
  #    period_title will be NA for assessments with no cperiod_id assigned.
  sql_assessments <- paste0("
    SELECT a.assessment_id,
           a.name                              AS assessment_name,
           a.type                              AS assessment_type,
           a.grade_weighting,
           a.cohort,
           a.cperiod_id,
           cp.curriculum_period_title          AS period_title,
           a.`order`                           AS display_order,
           FROM_UNIXTIME(a.due_date)           AS due_date
      FROM assessments AS a
      LEFT JOIN curriculum_periods AS cp
        ON cp.cperiod_id = a.cperiod_id
     WHERE a.course_id = ", course_id, "
       AND a.active = 1
     ORDER BY a.`order`
  ")
  assessments <- run_query(con, sql_assessments)
  cat("Found", nrow(assessments), "assessment(s).\n")

  if (nrow(assessments) == 0) {
    warning("No assessments found for course_id ", course_id)
    return(tibble())
  }

  # -- Step 3: Get all student grades for those assessments --
  assessment_ids <- paste(assessments$assessment_id, collapse = ",")
  sql_grades <- paste0("
    SELECT ag.grade_id,
           ag.assessment_id,
           a.name                              AS assessment_name,
           a.type                              AS assessment_type,
           a.grade_weighting,
           a.cohort,
           cp.curriculum_period_title          AS period_title,
           ag.proxy_id,
           ag.value                            AS grade_value,
           ag.threshold_notified,
           FROM_UNIXTIME(ag.updated_date)      AS updated_at
      FROM assessment_grades AS ag
      JOIN assessments AS a
        ON a.assessment_id = ag.assessment_id
      LEFT JOIN curriculum_periods AS cp
        ON cp.cperiod_id = a.cperiod_id
     WHERE ag.assessment_id IN (", assessment_ids, ")
     ORDER BY ag.proxy_id, a.`order`
  ")
  grades <- run_query(con, sql_grades)
  cat("Total grade records:", nrow(grades), "\n")

  return(grades)
}

#' Convenience wrapper: Get VETM 840 gradebook (course_id = 30)
get_vetm840_gradebook <- function(con) {
  get_gradebook(con, course_id = 30)
}


# =============================================================================
# 8.  REPORT TYPE 5: FINAL EXAMS — ITEM RESPONSE REPORT
# =============================================================================
#
#  Exam data in Elentra uses the quiz/exam system:
#    - exam_posts              (an exam event — links exam to a course/schedule)
#    - exams                   (exam definition)
#    - exam_elements           (questions in the exam, in order)
#    - exam_questions          (the question text, type, points)
#    - exam_question_answers   (answer options for each question)
#    - exam_progress           (one row per student attempt)
#    - exam_progress_responses (one row per student answer)
#

#' Get exam item responses for a specific exam or all exams in a course
#'
#' @param con        elentra_me connection
#' @param exam_id    (optional) specific exam_id. If NULL, pulls all exams for course_id.
#' @param course_id  (optional) pull all exams for this course. Ignored if exam_id is set.
#' @return tibble with student responses per question, including correct answer flag
get_exam_item_responses <- function(con, exam_id = NULL, course_id = NULL) {

  if (is.null(exam_id) && is.null(course_id)) {
    stop("Provide either exam_id or course_id.")
  }

  if (!is.null(exam_id)) {
    exam_filter <- paste0(" AND e.exam_id = ", exam_id)
  } else {
    exam_filter <- paste0(" AND ev.course_id = ", course_id)
  }

  # -- Step 1: List exams --
  # exam_posts has no course_id column — course is resolved by joining
  # exam_posts (target_type = 'event') -> events -> course_id
  sql_exams <- paste0("
    SELECT DISTINCT
           e.exam_id,
           e.title                             AS exam_title,
           ev.course_id,
           FROM_UNIXTIME(ep.start_date)        AS exam_start,
           FROM_UNIXTIME(ep.end_date)          AS exam_end
      FROM exams AS e
      JOIN exam_posts AS ep
        ON ep.exam_id = e.exam_id
       AND ep.target_type = 'event'
      JOIN events AS ev
        ON ev.event_id = ep.target_id
     WHERE e.deleted_date IS NULL
       AND ep.deleted_date IS NULL",
                      exam_filter, "
     ORDER BY ep.start_date DESC
  ")
  exams <- run_query(con, sql_exams)
  cat("Found", nrow(exams), "exam(s):\n")
  print(exams[, c("exam_id", "exam_title")])
  cat("\n")

  if (nrow(exams) == 0) {
    warning("No exams found. Check your exam_id or course_id.")
    return(tibble())
  }

  # -- Step 2: For each exam, get student responses per question --
  all_responses <- lapply(exams$exam_id, function(eid) {
    cat("  Pulling item responses for exam_id =", eid, "...\n")

    sql <- paste0("
      SELECT
          epr.exam_progress_id,
          epr.exam_progress_response_id,
          prog.proxy_id,
          prog.exam_points                     AS student_total_points,
          prog.exam_value                      AS student_total_pct,
          prog.progress_value                  AS attempt_status,
          FROM_UNIXTIME(prog.submission_date)  AS submitted_at,

          ee.exam_element_id,
          ee.`order`                           AS question_order,
          eq.exam_question_id,
          eq.question_text,
          eq.question_type,
          eq.points                            AS question_points,

          eqa_selected.answer_text             AS student_answer,
          epr.score                            AS points_earned,
          eqa_correct.answer_text              AS correct_answer

      FROM exam_progress AS prog

      JOIN exam_progress_responses AS epr
        ON epr.exam_progress_id = prog.exam_progress_id

      JOIN exam_elements AS ee
        ON ee.exam_element_id = epr.exam_element_id

      JOIN exam_questions AS eq
        ON eq.exam_question_id = ee.element_id
       AND ee.element_type = 'question'

      LEFT JOIN exam_question_answers AS eqa_selected
        ON eqa_selected.exam_question_answer_id = epr.exam_question_answer_id

      LEFT JOIN exam_question_answers AS eqa_correct
        ON eqa_correct.exam_question_id = eq.exam_question_id
       AND eqa_correct.correct = 1

      WHERE prog.exam_id = ", eid, "
        AND prog.deleted_date IS NULL
        AND prog.progress_value = 'submitted'

      ORDER BY prog.proxy_id, ee.`order`
    ")

    df <- run_query(con, sql)
    df$exam_id    <- eid
    df$exam_title <- exams$exam_title[exams$exam_id == eid]
    return(df)
  })

  result <- bind_rows(all_responses)
  cat("Total item responses:", nrow(result), "\n")
  return(result)
}
# =============================================================================
# 9.  REPORT TYPE 6: GENERIC COURSE REPORTS (any form by form_id)
# =============================================================================
#
#  This covers any of the forms visible on the Course Reports page, e.g.:
#    - 801 End of Course Survey v1/v2/v3
#    - 801 Foundations Course Eval
#    - P1 Session Changes
#    - Plus Delta Survey
#
#  Just use find_forms() + get_form_responses() with the right form_id.
#

#' Pull responses for any form by its exact form_id
#'
#' @param con        elentra_me connection
#' @param form_id    the form_id (find it with find_forms())
#' @param date_from  optional start date "YYYY-MM-DD"
#' @param date_to    optional end date "YYYY-MM-DD"
#' @return tibble of responses
get_course_report <- function(con, form_id,
                              date_from = NULL, date_to = NULL) {

  form_info <- run_query(con, paste0(
    "SELECT form_id, title FROM cbl_assessments_lu_forms WHERE form_id = ", form_id
  ))
  if (nrow(form_info) > 0) {
    cat("Pulling:", form_info$title, "(form_id =", form_id, ")\n")
  }

  get_form_responses(con, form_id = form_id,
                     date_from = date_from, date_to = date_to)
}


# =============================================================================
# 10. STUDENT IDENTITY LOOKUP  (join proxy_id -> real student info)
# =============================================================================
#
#  Most response data uses proxy_id or target_record_id instead of names.
#  You need the elentra_auth database to resolve these to actual people.
#

#' Map proxy_ids to student names and numbers
#'
#' @param auth_con   elentra_auth connection (use open_elentra_auth())
#' @param proxy_ids  vector of proxy_id values to look up
#' @return tibble with proxy_id, firstname, lastname, email, student_number
resolve_proxy_ids <- function(auth_con, proxy_ids) {
  ids <- paste(unique(proxy_ids), collapse = ",")
  sql <- paste0("
    SELECT u.id          AS proxy_id,
           u.firstname,
           u.lastname,
           u.email,
           u.number      AS student_number
      FROM user_data AS u
     WHERE u.id IN (", ids, ")
  ")
  run_query(auth_con, sql)
}


# =============================================================================
# 11. RUN ALL RETRIEVALS & SAVE AS DATA FRAMES + CSV
# =============================================================================
#
#  BEFORE RUNNING: Adjust the four config values below to match what you need.
#
#  Inputs you control:
#    date_from / date_to      — filter window for all form-based reports
#                               (ITER, EPA, course evals, surveys)
#                               format: "YYYY-MM-DD"
#    gradebook_course_id      — course_id for the gradebook pull
#                               read from Elentra URL: /gradebook?id=XX
#    exam_course_id           — course_id for the exam item response pull
#                               same URL pattern as above
#

# ---- Configuration ----
date_from           <- "2025-08-16"
date_to             <- "2026-05-07"
gradebook_course_id <- 30           # VETM 840
exam_course_id      <- 30           # VETM 840

# Output directory — where CSVs will be saved
output_dir <- file.path(getwd(), "elentra_output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("============================================================\n")
cat("  ELENTRA DATA RETRIEVAL — FULL RUN\n")
cat("  Date range:", date_from, "to", date_to, "\n")
cat("  Output dir:", output_dir, "\n")
cat("============================================================\n\n")

# ---- 0. Connect ----
cat(">> Opening connections...\n")
em   <- open_elentra_me()
auth <- open_elentra_auth()

if (is.null(em))   stop("Failed to connect to elentra_me. Check VPN and credentials.")
if (is.null(auth)) stop("Failed to connect to elentra_auth. Check VPN and credentials.")
cat("   Connected successfully.\n\n")

# ---- 0b. Discovery ----
cat(">> Discovery: listing available ITER forms...\n")
df_iter_forms <- find_forms(em, "ITER")
cat("\n>> Discovery: listing available EPA forms...\n")
df_epa_forms <- find_forms(em, "EPA")
cat("\n>> Discovery: listing available Course Eval forms...\n")
df_eval_forms <- find_forms(em, "Course Eval")
cat("\n>> Discovery: listing available End of Course Survey forms...\n")
df_survey_forms <- find_forms(em, "End of Course")
cat("\n>> Discovery: listing available Plus Delta forms...\n")
df_plusdelta_forms <- find_forms(em, "Plus Delta")
cat("\n>> Discovery: searching for courses matching 'VETM'...\n")
df_courses <- find_courses(em, "VETM")
cat("\n")

write.csv(df_iter_forms,      file.path(output_dir, "discovery_iter_forms.csv"),      row.names = FALSE)
write.csv(df_epa_forms,       file.path(output_dir, "discovery_epa_forms.csv"),       row.names = FALSE)
write.csv(df_eval_forms,      file.path(output_dir, "discovery_eval_forms.csv"),      row.names = FALSE)
write.csv(df_survey_forms,    file.path(output_dir, "discovery_survey_forms.csv"),    row.names = FALSE)
write.csv(df_plusdelta_forms, file.path(output_dir, "discovery_plusdelta_forms.csv"), row.names = FALSE)
write.csv(df_courses,         file.path(output_dir, "discovery_courses.csv"),         row.names = FALSE)
cat("   Discovery CSVs saved.\n\n")


# ==========================================================================
# REPORT 1: ITER FORM RESPONSES
# ==========================================================================
cat("============================================================\n")
cat("  REPORT 1: ITER FORM RESPONSES\n")
cat("============================================================\n")
df_iter_responses <- get_iter_responses(em, date_from = date_from, date_to = date_to)
write.csv(df_iter_responses, file.path(output_dir, "report_iter_responses.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_iter_responses), "rows -> report_iter_responses.csv\n\n")


# ==========================================================================
# REPORT 2: EPA FORM RESPONSES
# ==========================================================================
cat("============================================================\n")
cat("  REPORT 2: EPA FORM RESPONSES\n")
cat("============================================================\n")
df_epa_responses <- get_epa_responses(em, date_from = date_from, date_to = date_to)
write.csv(df_epa_responses, file.path(output_dir, "report_epa_responses.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_epa_responses), "rows -> report_epa_responses.csv\n\n")


# ==========================================================================
# REPORT 3: END-OF-COURSE SURVEYS & COURSE EVALUATIONS
# ==========================================================================
cat("============================================================\n")
cat("  REPORT 3a: COURSE EVALUATIONS\n")
cat("============================================================\n")
df_course_evals <- get_course_eval_responses(em, keyword = "Course Eval",
                                             date_from = date_from, date_to = date_to)
write.csv(df_course_evals, file.path(output_dir, "report_course_evals.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_course_evals), "rows -> report_course_evals.csv\n\n")

cat("============================================================\n")
cat("  REPORT 3b: END-OF-COURSE SURVEYS\n")
cat("============================================================\n")
df_end_of_course <- get_course_eval_responses(em, keyword = "End of Course",
                                              date_from = date_from, date_to = date_to)
write.csv(df_end_of_course, file.path(output_dir, "report_end_of_course_surveys.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_end_of_course), "rows -> report_end_of_course_surveys.csv\n\n")

cat("============================================================\n")
cat("  REPORT 3c: PLUS DELTA SURVEYS\n")
cat("============================================================\n")
df_plus_delta <- get_course_eval_responses(em, keyword = "Plus Delta",
                                           date_from = date_from, date_to = date_to)
write.csv(df_plus_delta, file.path(output_dir, "report_plus_delta_surveys.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_plus_delta), "rows -> report_plus_delta_surveys.csv\n\n")


# ==========================================================================
# REPORT 4: VETM 840 GRADEBOOK
# ==========================================================================
cat("============================================================\n")
cat("  REPORT 4: GRADEBOOK (course_id =", gradebook_course_id, ")\n")
cat("============================================================\n")
df_gradebook <- get_gradebook(em, course_id = gradebook_course_id)
write.csv(df_gradebook, file.path(output_dir, "report_gradebook.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_gradebook), "rows -> report_gradebook.csv\n\n")


# ==========================================================================
# REPORT 5: FINAL EXAM ITEM RESPONSES
# ==========================================================================
cat("============================================================\n")
cat("  REPORT 5: EXAM ITEM RESPONSES (course_id =", exam_course_id, ")\n")
cat("============================================================\n")
df_exam_items <- get_exam_item_responses(em, course_id = exam_course_id)
write.csv(df_exam_items, file.path(output_dir, "report_exam_item_responses.csv"), row.names = FALSE)
cat("   Saved:", nrow(df_exam_items), "rows -> report_exam_item_responses.csv\n\n")


# ==========================================================================
# REPORT 6: COURSE REPORT FORMS (generic — pull by specific form_id)
# ==========================================================================
#
#  Use the discovery data frames above (df_eval_forms, df_survey_forms, etc.)
#  to find the exact form_id you need. Then pull each one individually.
#
cat("============================================================\n")
cat("  REPORT 6: COURSE REPORT — SPECIFIC FORM(S)\n")
cat("============================================================\n")
#
# Uncomment and adjust form_id as needed:
# df_course_report_281 <- get_course_report(em, form_id = 281,
#                                           date_from = date_from, date_to = date_to)
# write.csv(df_course_report_281, file.path(output_dir, "report_course_form_281.csv"), row.names = FALSE)
# cat("   Saved:", nrow(df_course_report_281), "rows -> report_course_form_281.csv\n\n")
#
# To pull ALL discovered eval/survey/plusdelta forms at once:
# all_form_ids <- unique(c(df_eval_forms$form_id, df_survey_forms$form_id, df_plusdelta_forms$form_id))
# df_all_course_reports <- bind_rows(lapply(all_form_ids, function(fid) {
#   cat("  Pulling form_id =", fid, "...\n")
#   df <- get_course_report(em, form_id = fid, date_from = date_from, date_to = date_to)
#   if (nrow(df) > 0)
#     write.csv(df, file.path(output_dir, paste0("report_course_form_", fid, ".csv")), row.names = FALSE)
#   return(df)
# }))
cat("   (Uncomment the form_id(s) you need above and re-run this section.)\n\n")


# ==========================================================================
# RESOLVE STUDENT IDENTITIES — attach names to all data frames
# ==========================================================================
cat("============================================================\n")
cat("  RESOLVING STUDENT IDENTITIES\n")
cat("============================================================\n")

all_proxy_ids <- unique(c(
  if ("target_record_id" %in% names(df_iter_responses))  df_iter_responses$target_record_id,
  if ("target_record_id" %in% names(df_epa_responses))   df_epa_responses$target_record_id,
  if ("target_record_id" %in% names(df_course_evals))    df_course_evals$target_record_id,
  if ("target_record_id" %in% names(df_end_of_course))   df_end_of_course$target_record_id,
  if ("proxy_id" %in% names(df_gradebook))               df_gradebook$proxy_id,
  if ("proxy_id" %in% names(df_exam_items))              df_exam_items$proxy_id
))
all_proxy_ids <- all_proxy_ids[!is.na(all_proxy_ids)]

if (length(all_proxy_ids) > 0) {
  cat("   Looking up", length(all_proxy_ids), "unique proxy IDs...\n")
  df_students <- resolve_proxy_ids(auth, all_proxy_ids)
  write.csv(df_students, file.path(output_dir, "lookup_students.csv"), row.names = FALSE)
  cat("   Resolved", nrow(df_students), "students -> lookup_students.csv\n\n")

  # Gradebook and exam items use proxy_id directly
  if (nrow(df_gradebook) > 0 && "proxy_id" %in% names(df_gradebook)) {
    df_gradebook <- left_join(df_gradebook, df_students, by = "proxy_id")
    write.csv(df_gradebook, file.path(output_dir, "report_gradebook_with_names.csv"), row.names = FALSE)
    cat("   Saved: report_gradebook_with_names.csv\n")
  }

  if (nrow(df_exam_items) > 0 && "proxy_id" %in% names(df_exam_items)) {
    df_exam_items <- left_join(df_exam_items, df_students, by = "proxy_id")
    write.csv(df_exam_items, file.path(output_dir, "report_exam_items_with_names.csv"), row.names = FALSE)
    cat("   Saved: report_exam_items_with_names.csv\n")
  }

  # Form-based reports use target_record_id for the student
  df_students_target <- rename(df_students, target_record_id = proxy_id)

  if (nrow(df_iter_responses) > 0 && "target_record_id" %in% names(df_iter_responses)) {
    df_iter_responses <- left_join(df_iter_responses, df_students_target, by = "target_record_id")
    write.csv(df_iter_responses, file.path(output_dir, "report_iter_responses_with_names.csv"), row.names = FALSE)
    cat("   Saved: report_iter_responses_with_names.csv\n")
  }

  if (nrow(df_epa_responses) > 0 && "target_record_id" %in% names(df_epa_responses)) {
    df_epa_responses <- left_join(df_epa_responses, df_students_target, by = "target_record_id")
    write.csv(df_epa_responses, file.path(output_dir, "report_epa_responses_with_names.csv"), row.names = FALSE)
    cat("   Saved: report_epa_responses_with_names.csv\n")
  }

  cat("\n")
} else {
  cat("   No proxy IDs found to resolve (data frames may be empty).\n\n")
}


# ==========================================================================
# SUMMARY OF ALL DATA FRAMES IN MEMORY
# ==========================================================================
cat("============================================================\n")
cat("  SUMMARY: DATA FRAMES NOW IN YOUR R ENVIRONMENT\n")
cat("============================================================\n")
cat("\n")
cat("  Discovery tables:\n")
cat("    df_iter_forms        -", nrow(df_iter_forms),      "rows  (ITER form definitions)\n")
cat("    df_epa_forms         -", nrow(df_epa_forms),       "rows  (EPA form definitions)\n")
cat("    df_eval_forms        -", nrow(df_eval_forms),      "rows  (Course Eval form defs)\n")
cat("    df_survey_forms      -", nrow(df_survey_forms),    "rows  (End of Course survey defs)\n")
cat("    df_plusdelta_forms   -", nrow(df_plusdelta_forms),  "rows  (Plus Delta form defs)\n")
cat("    df_courses           -", nrow(df_courses),         "rows  (VETM courses)\n")
cat("\n")
cat("  Report data frames:\n")
cat("    df_iter_responses    -", nrow(df_iter_responses),  "rows  (ITER form responses)\n")
cat("    df_epa_responses     -", nrow(df_epa_responses),   "rows  (EPA form responses)\n")
cat("    df_course_evals      -", nrow(df_course_evals),    "rows  (Course Evaluation responses)\n")
cat("    df_end_of_course     -", nrow(df_end_of_course),   "rows  (End of Course survey responses)\n")
cat("    df_plus_delta        -", nrow(df_plus_delta),      "rows  (Plus Delta survey responses)\n")
cat("    df_gradebook         -", nrow(df_gradebook),       "rows  (Gradebook grades)\n")
cat("    df_exam_items        -", nrow(df_exam_items),      "rows  (Exam item responses)\n")
cat("\n")
cat("  Identity lookup:\n")
if (exists("df_students")) {
  cat("    df_students          -", nrow(df_students), "rows  (proxy_id -> name mapping)\n")
} else {
  cat("    df_students          -  (not created — no proxy IDs found)\n")
}
cat("\n")
cat("  All CSVs saved to:", output_dir, "\n")
cat("============================================================\n")
cat("  DONE.\n")
cat("============================================================\n")


# ---- Close connections ----
cat("\n>> Closing database connections...\n")
vmh::stop_connection(em)
vmh::stop_connection(auth)
cat("   Connections closed.\n")



#######################
em   <- open_elentra_me()
auth <- open_elentra_auth()
df_survey_forms[grep("801", df_survey_forms$title), ]
# Pull 801 End of Course Survey_v3 (form_id = 573)
df_801_v3 <- get_form_responses(em, form_id = 573,
                                date_from = date_from,
                                date_to   = date_to)

df_801_v3$form_id    <- 573
df_801_v3$form_title <- "801 End of Course Survey_v3"

cat("Rows returned:", nrow(df_801_v3), "\n")

# -- CSV --
write.csv(df_801_v3,
          file.path(output_dir, "report_801_end_of_course_v3.csv"),
          row.names = FALSE)
cat("CSV saved.\n")

# -- PDF --
library(gridExtra)

pdf(file.path(output_dir, "report_801_end_of_course_v3.pdf"),
    width = 14, height = 8.5)

grid.table(
  df_801_v3 |>
    select(submitted_at, target_record_id,
           question_text, response_text, response_comments) |>
    head(200),           # pdf page limit — increase if needed
  rows = NULL
)

dev.off()
cat("PDF saved.\n")
