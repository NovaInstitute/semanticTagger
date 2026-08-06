# Manual hierarchy maintenance. Interactive reclassification uses the
# preview/apply workflow in tagging-structure-review.R.

validate_editable_state <- function(state) {
  if (!inherits(state, "tag_state")) {
    stop("state must inherit from 'tag_state'.", call. = FALSE)
  }
  if (is.null(state$assignments) || is.null(state$clusters)) {
    stop("state must include non-NULL assignments and clusters.", call. = FALSE)
  }
  invisible(TRUE)
}

rebuild_cluster_index <- function(assignments) {
  level_columns <- cluster_level_cols(assignments)
  if (!length(level_columns)) {
    stop("No cluster_level_* columns found in assignments.", call. = FALSE)
  }
  max_level <- length(level_columns)
  purrr::map_dfr(seq_along(level_columns), function(level) {
    this_column <- level_columns[[level]]
    parent_column <- if (level < max_level) {
      level_columns[[level + 1L]]
    } else NA_character_
    assignments |>
      dplyr::filter(!is.na(.data[[this_column]])) |>
      dplyr::group_by(cluster_id = .data[[this_column]]) |>
      dplyr::summarise(
        level = level,
        parent_cluster = if (!is.na(parent_column)) {
          parent <- unique(.data[[parent_column]])
          parent <- parent[!is.na(parent)]
          if (!length(parent)) NA_integer_ else as.integer(parent[[1]])
        } else NA_integer_,
        question_ids = list(.data$id),
        .groups = "drop"
      )
  }) |>
    dplyr::arrange(.data$level, .data$cluster_id) |>
    dplyr::mutate(tag = NA_character_)
}

restore_cluster_tags <- function(clusters, previous_clusters) {
  lookup <- previous_clusters |>
    dplyr::select("level", "cluster_id", "tag")
  clusters |>
    dplyr::left_join(
      lookup, by = c("level", "cluster_id"), suffix = c("", "_old")
    ) |>
    dplyr::mutate(tag = dplyr::coalesce(.data$tag_old, .data$tag)) |>
    dplyr::select(-"tag_old")
}

#' Refresh derived artifacts after hierarchy edits
#'
#' @param state A `tag_state`.
#' @return Updated state with refreshed clusters and tag matrix.
#' @export
refresh_tag_state_artifacts <- function(state) {
  validate_editable_state(state)
  clusters <- rebuild_cluster_index(state$assignments)
  state$clusters <- restore_cluster_tags(clusters, state$clusters)
  state$tag_matrix <- build_question_tag_matrix(
    state$assignments, state$clusters
  )
  state$cleaned <- NULL
  state$audit <- NULL
  state
}

#' Prune an unwanted tag from the hierarchy
#'
#' @param state A `tag_state`.
#' @param tag Tag name to remove.
#' @param level Optional hierarchy level.
#' @param collapse_empty_levels Whether to remove levels made completely empty.
#' @return Updated `tag_state`.
#' @export
prune_tag_from_state <- function(state, tag, level = NULL,
                                 collapse_empty_levels = TRUE) {
  validate_editable_state(state)
  hits <- state$clusters |>
    dplyr::filter(.data$tag == !!tag)
  if (!is.null(level)) {
    hits <- hits |> dplyr::filter(.data$level == !!as.integer(level))
  }
  if (!nrow(hits)) stop("No matching cluster tag found to prune.", call. = FALSE)
  assignments <- state$assignments
  level_columns <- cluster_level_cols(assignments)
  for (i in seq_len(nrow(hits))) {
    column <- paste0("cluster_level_", as.integer(hits$level[[i]]))
    if (column %in% level_columns) {
      assignments[[column]][
        assignments[[column]] == as.integer(hits$cluster_id[[i]])
      ] <- NA_integer_
    }
  }
  if (isTRUE(collapse_empty_levels)) {
    empty <- purrr::keep(
      level_columns, function(column) all(is.na(assignments[[column]]))
    )
    if (length(empty)) {
      assignments <- assignments |> dplyr::select(-dplyr::all_of(empty))
      remaining <- cluster_level_cols(assignments)
      for (i in seq_along(remaining)) {
        names(assignments)[names(assignments) == remaining[[i]]] <-
          paste0("cluster_level_", i)
      }
    }
  }
  state$assignments <- assignments
  refresh_tag_state_artifacts(state)
}

#' Validate hierarchy consistency after manual edits
#'
#' @param state A `tag_state`.
#' @return Hierarchy diagnostics.
#' @export
validate_hierarchy_integrity <- function(state) {
  validate_editable_state(state)
  level_columns <- cluster_level_cols(state$assignments)
  missing_tags <- state$clusters |>
    dplyr::filter(is.na(.data$tag) | !nzchar(.data$tag))
  orphan_counts <- purrr::map_dfr(
    seq_along(level_columns)[-1],
    function(level) {
      child_column <- paste0("cluster_level_", level - 1L)
      parent_column <- paste0("cluster_level_", level)
      state$assignments |>
        dplyr::filter(
          !is.na(.data[[child_column]]) & is.na(.data[[parent_column]])
        ) |>
        dplyr::summarise(
          level = level,
          orphan_questions = dplyr::n()
        )
    }
  )
  list(
    n_questions = nrow(state$assignments),
    n_levels = length(level_columns),
    untagged_clusters = nrow(missing_tags),
    orphan_counts = orphan_counts
  )
}
