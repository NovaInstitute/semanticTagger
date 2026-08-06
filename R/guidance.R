# Reviewer-controlled reusable tagging guidance.

.tagging_guidance_statuses <- c(
  "candidate", "approved", "rejected", "retired", "superseded"
)
.tagging_guidance_kinds <- c("fact", "decision", "constraint")

.guidance_id <- function() {
  paste0(
    "guidance-",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    "-", sprintf("%06d", sample.int(999999L, 1L))
  )
}

#' Create reusable tagging guidance
#'
#' Guidance begins as a candidate and cannot affect model prompts until a
#' reviewer explicitly approves it.
#'
#' @param text Concise reusable instruction or domain fact.
#' @param kind One of `fact`, `decision`, or `constraint`.
#' @param tags Character vector used to categorize the guidance.
#' @param rationale Why the guidance should exist.
#' @param severity Optional constraint severity: `must`, `should`, or `prefer`.
#' @param guidance_id Optional stable identifier.
#' @return A `tagging_guidance` object with candidate status.
#' @export
new_tagging_guidance <- function(text, kind = c("constraint", "decision", "fact"),
                                 tags, rationale = "", severity = NULL,
                                 guidance_id = NULL) {
  kind <- match.arg(kind)
  text <- trimws(as.character(text))
  tags <- unique(trimws(as.character(tags)))
  tags <- tags[!is.na(tags) & nzchar(tags)]
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    stop("Guidance text must be non-empty.", call. = FALSE)
  }
  if (!length(tags)) stop("Guidance requires at least one tag.", call. = FALSE)
  if (!is.null(severity)) {
    severity <- match.arg(as.character(severity), c("must", "should", "prefer"))
  }
  if (!identical(kind, "constraint") && !is.null(severity)) {
    stop("Severity is only valid for constraint guidance.", call. = FALSE)
  }
  guidance_id <- guidance_id %||% .guidance_id()
  structure(list(
    guidance_id = as.character(guidance_id),
    text = text,
    kind = kind,
    tags = tags,
    rationale = as.character(rationale),
    severity = severity,
    status = "candidate",
    revision = 0L,
    created_at = Sys.time(),
    approved_at = NULL,
    approved_by = NULL,
    superseded_by = NULL,
    review_events = list()
  ), class = "tagging_guidance")
}

#' Review candidate or active tagging guidance
#'
#' @param guidance A `tagging_guidance` object.
#' @param decision One of `approve`, `reject`, or `retire`.
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Required explanation for rejection or retirement.
#' @param expected_revision Optional optimistic-lock revision.
#' @return Updated guidance with an immutable review event.
#' @export
review_tagging_guidance <- function(
    guidance, decision = c("approve", "reject", "retire"), reviewer_id,
    rationale = "", expected_revision = guidance$revision) {
  if (!inherits(guidance, "tagging_guidance")) {
    stop("`guidance` must be a tagging_guidance object.", call. = FALSE)
  }
  decision <- match.arg(decision)
  if (!identical(as.integer(expected_revision), as.integer(guidance$revision))) {
    stop("Guidance changed since it was loaded; refresh before reviewing.", call. = FALSE)
  }
  allowed <- switch(
    decision,
    approve = identical(guidance$status, "candidate"),
    reject = identical(guidance$status, "candidate"),
    retire = identical(guidance$status, "approved")
  )
  if (!allowed) {
    stop(
      "Decision '", decision, "' is invalid for ", guidance$status,
      " guidance.", call. = FALSE
    )
  }
  reviewer_id <- trimws(as.character(reviewer_id))
  if (length(reviewer_id) != 1L || is.na(reviewer_id) || !nzchar(reviewer_id)) {
    stop("A non-empty reviewer ID is required.", call. = FALSE)
  }
  rationale <- trimws(as.character(rationale))
  if (decision %in% c("reject", "retire") &&
      (length(rationale) != 1L || is.na(rationale) || !nzchar(rationale))) {
    stop("Rejecting or retiring guidance requires a rationale.", call. = FALSE)
  }
  now <- Sys.time()
  before <- guidance$status
  guidance$status <- switch(
    decision, approve = "approved", reject = "rejected", retire = "retired"
  )
  guidance$revision <- as.integer(guidance$revision) + 1L
  if (identical(decision, "approve")) {
    guidance$approved_by <- reviewer_id
    guidance$approved_at <- now
  }
  event <- list(
    event_id = paste0(guidance$guidance_id, ":revision:", guidance$revision),
    guidance_id = guidance$guidance_id,
    decision = decision,
    status_before = before,
    status_after = guidance$status,
    reviewer_id = reviewer_id,
    rationale = rationale,
    revision = guidance$revision,
    reviewed_at = now
  )
  guidance$review_events <- c(guidance$review_events %||% list(), list(event))
  guidance
}

#' Approve candidate tagging guidance
#'
#' @param guidance A `tagging_guidance` object.
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Optional approval rationale.
#' @param expected_revision Optional optimistic-lock revision.
#' @return Approved guidance ready to persist and recall.
#' @export
approve_tagging_guidance <- function(
    guidance, reviewer_id, rationale = "",
    expected_revision = guidance$revision) {
  review_tagging_guidance(
    guidance, "approve", reviewer_id, rationale, expected_revision
  )
}

#' Supersede approved guidance with a new candidate
#'
#' @param guidance Approved `tagging_guidance`.
#' @param replacement_text Text for the replacement candidate.
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Why replacement is necessary.
#' @param expected_revision Optional optimistic-lock revision.
#' @return A list containing the superseded guidance and replacement candidate.
#' @export
supersede_tagging_guidance <- function(
    guidance, replacement_text, reviewer_id, rationale,
    expected_revision = guidance$revision) {
  if (!inherits(guidance, "tagging_guidance") ||
      !identical(guidance$status, "approved")) {
    stop("Only approved guidance can be superseded.", call. = FALSE)
  }
  if (!identical(as.integer(expected_revision), as.integer(guidance$revision))) {
    stop("Guidance changed since it was loaded; refresh before reviewing.", call. = FALSE)
  }
  reviewer_id <- trimws(as.character(reviewer_id))
  rationale <- trimws(as.character(rationale))
  if (!nzchar(reviewer_id)) stop("A non-empty reviewer ID is required.", call. = FALSE)
  if (!nzchar(rationale)) stop("Superseding guidance requires a rationale.", call. = FALSE)
  replacement <- new_tagging_guidance(
    replacement_text, guidance$kind, guidance$tags, rationale,
    guidance$severity
  )
  now <- Sys.time()
  before <- guidance$status
  guidance$status <- "superseded"
  guidance$superseded_by <- replacement$guidance_id
  guidance$revision <- as.integer(guidance$revision) + 1L
  event <- list(
    event_id = paste0(guidance$guidance_id, ":revision:", guidance$revision),
    guidance_id = guidance$guidance_id,
    decision = "supersede",
    status_before = before,
    status_after = "superseded",
    reviewer_id = reviewer_id,
    rationale = rationale,
    revision = guidance$revision,
    reviewed_at = now,
    replacement_guidance_id = replacement$guidance_id
  )
  guidance$review_events <- c(guidance$review_events %||% list(), list(event))
  structure(
    list(guidance = guidance, replacement = replacement),
    class = "tagging_guidance_supersession"
  )
}

.empty_tagging_guidance <- function() {
  tibble::tibble(
    guidance_id = character(),
    text = character(),
    kind = character(),
    tags = list(),
    rationale = character(),
    severity = character(),
    status = character(),
    revision = integer(),
    approved_by = character(),
    superseded_by = character(),
    created_at = character(),
    approved_at = character()
  )
}
