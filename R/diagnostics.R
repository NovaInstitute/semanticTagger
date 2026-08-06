.row_cosine_similarity <- function(x, reference) {
  denom <- sqrt(rowSums(x^2)) * sqrt(sum(reference^2))
  ifelse(denom == 0, NA_real_, as.numeric(x %*% reference) / denom)
}

.leaf_cluster_centroids <- function(state) {
  ids <- unique(state$assignments$cluster_level_1)
  ids <- ids[!is.na(ids)]
  embeddings <- state_embedding_matrix(state)
  centroids <- lapply(ids, function(id) {
    rows <- which(state$assignments$cluster_level_1 == id)
    colMeans(embeddings[rows, , drop = FALSE])
  })
  names(centroids) <- as.character(ids)
  centroids
}

.cluster_path_label <- function(state, level, cluster_id) {
  labels <- character()
  current_level <- as.integer(level)
  current_id <- cluster_id
  repeat {
    row <- state$clusters[
      state$clusters$level == current_level &
        as.character(state$clusters$cluster_id) == as.character(current_id),
      ,
      drop = FALSE
    ]
    if (nrow(row) != 1L) break
    label <- row$tag[[1]]
    labels <- c(labels, if (is.na(label) || !nzchar(label)) {
      paste0("cluster ", current_id)
    } else label)
    if (is.na(row$parent_cluster[[1]])) break
    current_id <- row$parent_cluster[[1]]
    current_level <- current_level + 1L
  }
  paste(rev(labels), collapse = " > ")
}

.accepted_tag_embedding <- function(state, cluster_id) {
  matches <- Filter(function(proposal) {
    identical(as.integer(proposal$level), 1L) &&
      identical(as.character(proposal$cluster_id), as.character(cluster_id)) &&
      proposal$status %in% c("accepted", "edited") &&
      !is.null(proposal$tag_embedding)
  }, state$proposals)
  if (!length(matches)) return(NULL)
  matches[[length(matches)]]$tag_embedding
}

#' Rank alternative leaf-cluster placements for questions
#'
#' Recommendations use the original embedding dimensions, cluster centroids,
#' and accepted tag embeddings when available.
#'
#' @param state A `tag_state`.
#' @param question_ids Question identifiers to inspect.
#' @param top_n Maximum candidate clusters returned per question.
#'
#' @return A tibble ordered by question and candidate rank.
#' @export
rank_question_placements <- function(state, question_ids, top_n = 5L) {
  state <- validate_tag_state(state)
  question_ids <- unique(as.character(question_ids))
  rows <- match(question_ids, state$questions$id)
  if (anyNA(rows)) {
    stop("Unknown question ID(s): ", paste(question_ids[is.na(rows)], collapse = ", "),
         call. = FALSE)
  }
  centroids <- .leaf_cluster_centroids(state)
  cluster_ids <- names(centroids)
  top_n <- min(max(1L, as.integer(top_n)), length(cluster_ids))
  embeddings <- state_embedding_matrix(state)

  purrr::map_dfr(seq_along(rows), function(i) {
    question_embedding <- embeddings[rows[[i]], ]
    centroid_similarity <- vapply(
      centroids,
      function(centroid) .row_cosine_similarity(
        matrix(question_embedding, nrow = 1L), centroid
      )[[1]],
      numeric(1)
    )
    tag_similarity <- vapply(cluster_ids, function(cluster_id) {
      tag_embedding <- .accepted_tag_embedding(state, cluster_id)
      if (is.null(tag_embedding)) return(NA_real_)
      .row_cosine_similarity(
        matrix(question_embedding, nrow = 1L), tag_embedding
      )[[1]]
    }, numeric(1))
    current <- as.character(
      state$assignments$cluster_level_1[
        match(question_ids[[i]], state$assignments$id)
      ]
    )
    order_idx <- order(centroid_similarity, decreasing = TRUE, na.last = TRUE)
    order_idx <- utils::head(order_idx, top_n)
    tibble::tibble(
      question_id = question_ids[[i]],
      question = state$questions$caption[rows[[i]]],
      candidate_cluster = cluster_ids[order_idx],
      rank = seq_along(order_idx),
      current_cluster = cluster_ids[order_idx] == current,
      centroid_similarity = centroid_similarity[order_idx],
      tag_similarity = tag_similarity[order_idx],
      hierarchy_path = vapply(
        cluster_ids[order_idx],
        function(id) .cluster_path_label(state, 1L, id),
        character(1)
      )
    )
  })
}

#' Diagnose cluster cohesion and likely misplaced questions
#'
#' @param state A `tag_state`.
#' @param outlier_similarity Centroid-similarity threshold.
#' @param placement_margin Minimum improvement over the current centroid needed
#'   to flag an alternative placement.
#' @param neighbour_k Number of nearest question neighbours used for local
#'   disagreement.
#'
#' @return A list with cluster and question diagnostic tables.
#' @export
diagnose_tagging_clusters <- function(state, outlier_similarity = 0.5,
                                      placement_margin = 0.1,
                                      neighbour_k = 5L) {
  state <- validate_tag_state(state)
  embeddings <- state_embedding_matrix(state)
  centroids <- .leaf_cluster_centroids(state)
  cluster_ids <- names(centroids)
  centroid_matrix <- do.call(rbind, centroids)
  similarity_to_centroids <- vapply(seq_len(nrow(centroid_matrix)), function(i) {
    .row_cosine_similarity(embeddings, centroid_matrix[i, ])
  }, numeric(nrow(embeddings)))
  if (is.null(dim(similarity_to_centroids))) {
    similarity_to_centroids <- matrix(similarity_to_centroids, ncol = 1L)
  }
  colnames(similarity_to_centroids) <- cluster_ids

  current <- as.character(state$assignments$cluster_level_1[
    match(state$questions$id, state$assignments$id)
  ])
  current_col <- match(current, cluster_ids)
  current_similarity <- similarity_to_centroids[
    cbind(seq_len(nrow(embeddings)), current_col)
  ]
  alternative_matrix <- similarity_to_centroids
  alternative_matrix[cbind(seq_len(nrow(embeddings)), current_col)] <- -Inf
  best_alternative_col <- max.col(alternative_matrix, ties.method = "first")
  best_alternative_similarity <- alternative_matrix[
    cbind(seq_len(nrow(embeddings)), best_alternative_col)
  ]
  if (length(cluster_ids) == 1L) {
    best_alternative_similarity[] <- NA_real_
    best_alternative_col[] <- NA_integer_
  }

  normalized <- embeddings / sqrt(rowSums(embeddings^2))
  normalized[!is.finite(normalized)] <- 0
  question_similarity <- normalized %*% t(normalized)
  diag(question_similarity) <- -Inf
  neighbour_k <- min(max(1L, as.integer(neighbour_k)), max(1L, nrow(embeddings) - 1L))
  disagreement <- vapply(seq_len(nrow(embeddings)), function(i) {
    if (nrow(embeddings) == 1L) return(0)
    nearest <- order(question_similarity[i, ], decreasing = TRUE)[seq_len(neighbour_k)]
    mean(current[nearest] != current[[i]])
  }, numeric(1))

  questions <- tibble::tibble(
    question_id = state$questions$id,
    question = state$questions$caption,
    current_cluster = current,
    current_centroid_similarity = current_similarity,
    best_alternative_cluster = ifelse(
      is.na(best_alternative_col), NA_character_, cluster_ids[best_alternative_col]
    ),
    best_alternative_similarity = best_alternative_similarity,
    placement_margin = best_alternative_similarity - current_similarity,
    neighbour_disagreement = disagreement,
    centroid_outlier = is.na(current_similarity) |
      current_similarity < outlier_similarity,
    alternative_better = !is.na(best_alternative_similarity) &
      best_alternative_similarity - current_similarity >= placement_margin
  )

  clusters <- purrr::map_dfr(cluster_ids, function(cluster_id) {
    rows <- which(current == cluster_id)
    metrics <- cluster_cohesion_metrics(
      state, 1L, cluster_id, outlier_similarity
    )
    metrics$mean_neighbour_disagreement <- mean(disagreement[rows], na.rm = TRUE)
    metrics$alternative_better_count <- sum(questions$alternative_better[rows])
    metrics$flagged <- metrics$outlier_count > 0L ||
      metrics$alternative_better_count > 0L ||
      metrics$mean_neighbour_disagreement >= 0.5
    metrics
  })
  list(clusters = clusters, questions = questions)
}

#' Build a deterministic two-dimensional question projection
#'
#' This PCA projection is for interactive navigation only. Placement and
#' outlier diagnostics use the original embedding dimensions.
#'
#' @param state A `tag_state`.
#'
#' @return A tibble with plot coordinates and question metadata.
#' @export
question_projection_2d <- function(state) {
  state <- validate_tag_state(state)
  embeddings <- state_embedding_matrix(state)
  if (nrow(embeddings) == 1L) {
    coordinates <- matrix(c(0, 0), nrow = 1L)
  } else {
    fit <- stats::prcomp(embeddings, center = TRUE, scale. = FALSE, rank. = 2L)
    coordinates <- fit$x[, seq_len(min(2L, ncol(fit$x))), drop = FALSE]
    if (ncol(coordinates) == 1L) coordinates <- cbind(coordinates, 0)
  }
  tibble::tibble(
    question_id = state$questions$id,
    question = state$questions$caption,
    cluster_id = as.character(state$assignments$cluster_level_1[
      match(state$questions$id, state$assignments$id)
    ]),
    x = coordinates[, 1],
    y = coordinates[, 2]
  )
}
