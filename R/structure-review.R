.structure_change_id <- function(run_id) {
  paste0(
    run_id, ":reclassification:",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), ":",
    sprintf("%06d", sample.int(999999L, 1L))
  )
}

.cluster_metric_set <- function(state, cluster_ids, outlier_similarity) {
  purrr::map_dfr(
    as.character(cluster_ids),
    function(cluster_id) cluster_cohesion_metrics(
      state, 1L, cluster_id, outlier_similarity
    )
  )
}

.affected_cluster_paths <- function(before, after, rows) {
  level_columns <- cluster_level_cols(before)
  out <- lapply(seq_along(level_columns), function(i) {
    column <- level_columns[[i]]
    ids <- unique(c(before[[column]][rows], after[[column]][rows]))
    ids <- ids[!is.na(ids)]
    if (!length(ids)) return(NULL)
    tibble::tibble(level = as.integer(i), cluster_id = as.character(ids))
  })
  dplyr::distinct(dplyr::bind_rows(out), .data$level, .data$cluster_id)
}

#' Preview moving questions to an existing leaf cluster
#'
#' @param state A `tag_state`.
#' @param question_ids Question identifiers to move.
#' @param destination_cluster Existing level-1 cluster identifier.
#' @param outlier_similarity Threshold used for cohesion outlier counts.
#'
#' @return A `structure_change_preview`; the input state is not modified.
#' @export
preview_question_reclassification <- function(
    state, question_ids, destination_cluster, outlier_similarity = 0.5) {
  state <- validate_tag_state(state)
  validate_editable_state(state)
  question_ids <- unique(as.character(question_ids))
  if (!length(question_ids) || anyNA(question_ids) || any(!nzchar(question_ids))) {
    stop("Provide at least one non-empty question ID.", call. = FALSE)
  }
  rows <- match(question_ids, state$assignments$id)
  if (anyNA(rows)) {
    stop(
      "Unknown question ID(s): ",
      paste(question_ids[is.na(rows)], collapse = ", "),
      call. = FALSE
    )
  }
  destination_cluster <- as.character(destination_cluster)
  destination_rows <- which(
    as.character(state$assignments$cluster_level_1) == destination_cluster
  )
  if (!length(destination_rows)) {
    stop("Destination leaf cluster was not found.", call. = FALSE)
  }
  source_clusters <- unique(as.character(state$assignments$cluster_level_1[rows]))
  if (length(source_clusters) == 1L &&
      identical(source_clusters, destination_cluster)) {
    stop("All selected questions are already in the destination cluster.", call. = FALSE)
  }

  level_columns <- cluster_level_cols(state$assignments)
  destination_path <- state$assignments[destination_rows[[1]], level_columns, drop = FALSE]
  proposed_state <- state
  proposed_assignments <- proposed_state$assignments
  for (column in level_columns) {
    proposed_assignments[[column]][rows] <- destination_path[[column]][[1]]
  }
  proposed_state$assignments <- proposed_assignments
  proposed_state <- refresh_tag_state_artifacts(proposed_state)

  metric_clusters <- unique(c(source_clusters, destination_cluster))
  before_metrics <- .cluster_metric_set(state, metric_clusters, outlier_similarity)
  after_metrics <- .cluster_metric_set(
    proposed_state, metric_clusters, outlier_similarity
  )
  comparison <- dplyr::full_join(
    before_metrics,
    after_metrics,
    by = c("level", "cluster_id"),
    suffix = c("_before", "_after")
  )
  comparison$mean_similarity_change <-
    comparison$mean_similarity_after - comparison$mean_similarity_before

  structure(
    list(
      change_id = .structure_change_id(state$run_id),
      type = "question_reclassification",
      status = "preview",
      run_id = state$run_id,
      base_revision = state$revision,
      question_ids = question_ids,
      source_clusters = source_clusters,
      destination_cluster = destination_cluster,
      affected_clusters = .affected_cluster_paths(
        state$assignments, proposed_assignments, rows
      ),
      metrics = comparison,
      proposed_state = proposed_state,
      created_at = Sys.time()
    ),
    class = "structure_change_preview"
  )
}

#' Apply an approved structural preview
#'
#' @param state Current `tag_state`.
#' @param preview A `structure_change_preview`.
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Reviewer explanation for the change.
#'
#' @return Updated `tag_state`.
#' @export
apply_structure_change <- function(state, preview, reviewer_id,
                                   rationale = "") {
  state <- validate_tag_state(state)
  if (!inherits(preview, "structure_change_preview") ||
      !identical(preview$status, "preview")) {
    stop("`preview` must be an unapplied structure-change preview.", call. = FALSE)
  }
  if (!identical(state$run_id, preview$run_id)) {
    stop("Structural preview belongs to a different tagging run.", call. = FALSE)
  }
  if (!identical(as.integer(state$revision), as.integer(preview$base_revision))) {
    stop("Tagging state changed after this preview; create a new preview.", call. = FALSE)
  }
  reviewer_id <- trimws(as.character(reviewer_id))
  if (length(reviewer_id) != 1L || is.na(reviewer_id) || !nzchar(reviewer_id)) {
    stop("A non-empty reviewer ID is required.", call. = FALSE)
  }

  updated <- preview$proposed_state
  affected <- preview$affected_clusters
  if (nrow(affected)) {
    for (i in seq_len(nrow(affected))) {
      hit <- updated$clusters$level == affected$level[[i]] &
        as.character(updated$clusters$cluster_id) == affected$cluster_id[[i]]
      updated$clusters$tag[hit] <- NA_character_
    }
    for (proposal_id in names(updated$proposals)) {
      proposal <- updated$proposals[[proposal_id]]
      hit <- any(
        affected$level == proposal$level &
          affected$cluster_id == as.character(proposal$cluster_id)
      )
      if (hit && proposal$status %in% c("proposed", "deferred", "accepted", "edited")) {
        proposal$status <- "superseded"
        updated$proposals[[proposal_id]] <- proposal
      }
    }
  }
  updated$tag_matrix <- build_question_tag_matrix(updated$assignments, updated$clusters)
  updated$cleaned <- NULL
  updated$audit <- NULL
  event <- list(
    event_id = preview$change_id,
    type = preview$type,
    run_id = updated$run_id,
    base_revision = preview$base_revision,
    question_ids = preview$question_ids,
    source_clusters = preview$source_clusters,
    destination_cluster = preview$destination_cluster,
    affected_clusters = preview$affected_clusters,
    metrics = preview$metrics,
    reviewer_id = reviewer_id,
    rationale = as.character(rationale),
    created_at = Sys.time()
  )
  updated$structure_events[[length(updated$structure_events) + 1L]] <- event
  validate_tag_state(updated)
}

#' Discard a structural preview
#'
#' @param preview A `structure_change_preview`.
#'
#' @return The discarded preview, invisibly. No tagging state is modified.
#' @export
discard_structure_change <- function(preview) {
  if (!inherits(preview, "structure_change_preview")) {
    stop("`preview` must be a structure-change preview.", call. = FALSE)
  }
  preview$status <- "discarded"
  invisible(preview)
}
