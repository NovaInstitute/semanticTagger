.new_tag_run_id <- function() {
  paste0(
    "tagrun_",
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    "_",
    sprintf("%06d", sample.int(999999L, 1L))
  )
}

#' A list to keep track of tagging progress.
#'
#' The tag_state is saved as progress is made so the pipeline can be resumed in
#' case it is paused or interrupted.
#'
#' @param questions Data frame of question IDs and captions.
#' @param run_id Stable identifier for this tagging run.
#'
#' @return A new `tag_state` object.
#' @keywords internal
new_tag_state <- function(questions, run_id = NULL) {
  if (is.null(run_id)) run_id <- .new_tag_run_id()
  now <- Sys.time()
  structure(
    list(
      run_id = run_id,
      status = "in_progress",
      revision = 0L,
      created_at = now,
      updated_at = now,
      questions  = questions,        # tibble: id, caption
      embeddings = NULL,             # matrix
      hclust     = NULL,             # hclust object
      assignments = NULL,            # tibble: question + cluster_level_* columns
      clusters   = NULL,             # tibble: level, cluster_id, parent_cluster, tag
      proposals  = list(),           # named list of AI tag proposals
      review_events = list(),        # immutable reviewer decisions
      structure_events = list(),     # immutable applied hierarchy changes
      workflow = list(),             # resumable controller stage and provenance
      tags       = NULL,             # tibble: level, cluster_id, tag
      tag_matrix = NULL,             # tibble: id, tag_level_1...tag_level_n
      cleaned    = NULL,             # vocab cleanup info
      audit      = NULL              # audit results
    ),
    class = "tag_state"
  )
}

#' Validate and normalize tagging state
#'
#' @param state Object expected to represent a tagging run.
#' @return The validated object with class `tag_state`.
#' @export
validate_tag_state <- function(state) {
  if (!is.list(state)) {
    stop("Tagging state must be a list.", call. = FALSE)
  }
  if (!is.data.frame(state$questions) ||
      !all(c("id", "caption") %in% names(state$questions))) {
    stop("Tagging state must contain questions with `id` and `caption` columns.", call. = FALSE)
  }

  now <- Sys.time()
  state$run_id <- state$run_id %||% paste0(
    "tagrun_legacy_",
    sprintf("%06d", sample.int(999999L, 1L))
  )
  state$status <- state$status %||% "in_progress"
  state$revision <- as.integer(state$revision %||% 0L)
  state$created_at <- state$created_at %||% now
  state$updated_at <- state$updated_at %||% state$created_at
  state$proposals <- state$proposals %||% list()
  state$review_events <- state$review_events %||% list()
  state$structure_events <- state$structure_events %||% list()
  state$workflow <- state$workflow %||% list()

  if (!is.character(state$run_id) || length(state$run_id) != 1L ||
      is.na(state$run_id) || !nzchar(state$run_id)) {
    stop("Tagging state requires one non-empty `run_id`.", call. = FALSE)
  }
  if (!state$status %in% c("in_progress", "review", "complete", "failed")) {
    stop("Tagging state has an unsupported `status`.", call. = FALSE)
  }
  if (length(state$revision) != 1L || is.na(state$revision) || state$revision < 0L) {
    stop("Tagging state requires a non-negative `revision`.", call. = FALSE)
  }
  if (!is.list(state$proposals) || !is.list(state$review_events) ||
      !is.list(state$structure_events) || !is.list(state$workflow)) {
    stop("Tagging proposals, review events, structure events, and workflow metadata must be lists.", call. = FALSE)
  }

  class(state) <- unique(c("tag_state", class(state)))
  state
}
