#' Extract the embedding matrix from a tag state
#'
#' @param state A `tag_state` object containing either `embeddings` or a
#'   question-level `embedding` list-column.
#'
#' @return A numeric matrix with one row per question.
#' @keywords internal
state_embedding_matrix <- function(state) {
  if (!is.null(state$embeddings)) {
    return(as.matrix(state$embeddings))
  }
  if ("embedding" %in% names(state$questions)) {
    return(do.call(rbind, state$questions$embedding))
  }
  stop("State must contain embeddings or questions$embedding.", call. = FALSE)
}

#' Build the assignment column name for a hierarchy level
#'
#' @param level Integer hierarchy level.
#'
#' @return A single assignment column name.
#' @keywords internal
cluster_assignment_col <- function(level) {
  paste0("cluster_level_", level)
}

#' Locate the questions assigned to a cluster
#'
#' @param state A `tag_state` object.
#' @param level Integer hierarchy level.
#' @param cluster_id Cluster identifier at `level`.
#'
#' @return Integer row positions in `state$questions`.
#' @keywords internal
cluster_question_rows <- function(state, level, cluster_id) {
  col <- cluster_assignment_col(level)
  if (!col %in% names(state$assignments)) {
    stop("Missing assignment column: ", col, call. = FALSE)
  }
  which(state$assignments[[col]] == cluster_id)
}

#' Calculate cosine distance from a centroid
#'
#' @param mat Numeric matrix with one observation per row.
#' @param centroid Numeric centroid vector.
#'
#' @return Numeric vector of cosine distances.
#' @keywords internal
cosine_distance_to_centroid <- function(mat, centroid) {
  denom <- sqrt(rowSums(mat^2)) * sqrt(sum(centroid^2))
  sim <- ifelse(denom == 0, 0, as.numeric(mat %*% centroid) / denom)
  1 - sim
}

#' Score a tag embedding against every question in a cluster
#'
#' @param state A `tag_state`.
#' @param level,cluster_id Cluster identity.
#' @param tag_embedding Numeric tag embedding.
#' @param low_similarity Similarities below this value are flagged.
#'
#' @return A tibble ordered from least to most similar.
#' @export
score_cluster_tag_similarity <- function(state, level, cluster_id, tag_embedding,
                                         low_similarity = 0.5) {
  rows <- cluster_question_rows(state, as.integer(level), cluster_id)
  if (!length(rows)) stop("Cluster has no assigned questions.", call. = FALSE)
  embeddings <- state_embedding_matrix(state)[rows, , drop = FALSE]
  tag_embedding <- as.numeric(tag_embedding)
  if (!length(tag_embedding) || any(!is.finite(tag_embedding))) {
    stop("Tag embedding must be non-empty, numeric, and finite.", call. = FALSE)
  }
  if (ncol(embeddings) != length(tag_embedding)) {
    stop("Tag and question embedding dimensions do not match.", call. = FALSE)
  }
  denom <- sqrt(rowSums(embeddings^2)) * sqrt(sum(tag_embedding^2))
  similarity <- ifelse(
    denom == 0,
    NA_real_,
    as.numeric(embeddings %*% tag_embedding) / denom
  )
  out <- tibble::tibble(
    question_id = state$questions$id[rows],
    question = state$questions$caption[rows],
    cosine_similarity = similarity,
    cosine_distance = 1 - similarity,
    low_similarity = is.na(similarity) | similarity < low_similarity
  )
  out[order(out$cosine_similarity, na.last = TRUE), , drop = FALSE]
}

#' Compare question similarity before and after a tag edit
#'
#' @param before,after Outputs from [score_cluster_tag_similarity()].
#'
#' @return Per-question changes and summary metrics.
#' @export
compare_tag_similarity <- function(before, after) {
  required <- c("question_id", "cosine_similarity", "low_similarity")
  if (!all(required %in% names(before)) || !all(required %in% names(after))) {
    stop("Similarity tables are missing required columns.", call. = FALSE)
  }
  comparison <- dplyr::full_join(
    dplyr::select(before, "question_id", before_similarity = "cosine_similarity",
                  before_low = "low_similarity"),
    dplyr::select(after, "question_id", after_similarity = "cosine_similarity",
                  after_low = "low_similarity"),
    by = "question_id"
  )
  comparison$change <- comparison$after_similarity - comparison$before_similarity
  summarize <- function(x, low) {
    list(
      mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
      minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      low_count = sum(low, na.rm = TRUE)
    )
  }
  list(
    questions = comparison,
    before = summarize(comparison$before_similarity, comparison$before_low),
    after = summarize(comparison$after_similarity, comparison$after_low),
    mean_change = mean(comparison$change, na.rm = TRUE)
  )
}

#' Summarize embedding cohesion for one cluster
#'
#' @param state A `tag_state`.
#' @param level,cluster_id Cluster identity.
#' @param outlier_similarity Similarities below this value are counted as
#'   centroid outliers.
#'
#' @return A one-row tibble of cohesion metrics.
#' @export
cluster_cohesion_metrics <- function(state, level, cluster_id,
                                     outlier_similarity = 0.5) {
  rows <- cluster_question_rows(state, as.integer(level), cluster_id)
  if (!length(rows)) {
    return(tibble::tibble(
      level = as.integer(level),
      cluster_id = as.character(cluster_id),
      question_count = 0L,
      mean_similarity = NA_real_,
      minimum_similarity = NA_real_,
      outlier_count = 0L
    ))
  }
  embeddings <- state_embedding_matrix(state)[rows, , drop = FALSE]
  centroid <- colMeans(embeddings)
  similarity <- 1 - cosine_distance_to_centroid(embeddings, centroid)
  tibble::tibble(
    level = as.integer(level),
    cluster_id = as.character(cluster_id),
    question_count = length(rows),
    mean_similarity = mean(similarity, na.rm = TRUE),
    minimum_similarity = min(similarity, na.rm = TRUE),
    outlier_count = sum(is.na(similarity) | similarity < outlier_similarity)
  )
}

#' Summarize questions for tagging evidence
#'
#' @param state A `tag_state` object.
#' @param row_idx Integer question row positions.
#' @param scores Optional numeric evidence scores.
#'
#' @return A tibble containing question identifiers, text, and optional scores.
#' @keywords internal
summarise_questions_for_evidence <- function(state, row_idx, scores = NULL) {
  out <- tibble::tibble(
    question_id = state$questions$id[row_idx],
    question_text = state$questions$caption[row_idx]
  )
  if (!is.null(scores)) {
    out$score <- as.numeric(scores)
  }
  out
}

#' Build a compact profile for one cluster
#'
#' @param state A `tag_state` object.
#' @param cluster_id Cluster identifier at `level`.
#' @param level Hierarchy level. If `NULL`, the cluster id must be unique.
#' @param sample_size Maximum number of examples per evidence category.
#'
#' @return A list with counts, tags, child metadata, and evidence tables.
#' @export
get_cluster_profile <- function(state, cluster_id, level = NULL, sample_size = 8L) {
  clusters <- state$clusters
  if (is.null(level)) {
    hit <- clusters[clusters$cluster_id == cluster_id, , drop = FALSE]
    if (nrow(hit) != 1L) {
      stop("Provide `level` when cluster_id is not unique across levels.", call. = FALSE)
    }
    level <- hit$level[[1]]
  }

  cluster_row <- clusters[
    clusters$level == level & clusters$cluster_id == cluster_id,
    ,
    drop = FALSE
  ]
  if (nrow(cluster_row) != 1L) {
    stop("Cluster not found for level ", level, " and cluster_id ", cluster_id, call. = FALSE)
  }

  rows <- cluster_question_rows(state, level, cluster_id)
  emb <- state_embedding_matrix(state)
  cluster_emb <- emb[rows, , drop = FALSE]
  centroid <- colMeans(cluster_emb)
  dist <- cosine_distance_to_centroid(cluster_emb, centroid)
  ord_close <- order(dist)
  ord_far <- order(dist, decreasing = TRUE)

  representative_rows <- rows[utils::head(ord_close, sample_size)]
  outlier_rows <- rows[utils::head(ord_far, sample_size)]

  diverse_rows <- integer(0)
  if (length(rows) > 0) {
    diverse_rows <- rows[[ord_close[[1]]]]
    remaining <- setdiff(seq_along(rows), ord_close[[1]])
    while (length(diverse_rows) < min(sample_size, length(rows)) && length(remaining) > 0) {
      chosen_local <- match(diverse_rows, rows)
      sims <- cluster_emb[remaining, , drop = FALSE] %*%
        t(cluster_emb[chosen_local, , drop = FALSE])
      next_local <- remaining[which.min(apply(sims, 1, max))]
      diverse_rows <- c(diverse_rows, rows[[next_local]])
      remaining <- setdiff(remaining, next_local)
    }
  }

  children <- clusters[
    clusters$parent_cluster == cluster_id & clusters$level == level - 1L,
    ,
    drop = FALSE
  ]
  children_summary <- if (nrow(children) == 0) {
    tibble::tibble()
  } else {
    tibble::tibble(
      cluster_id = children$cluster_id,
      tag = children$tag,
      support = purrr::map_int(children$question_ids, length)
    )
  }

  list(
    cluster_id = cluster_id,
    level = level,
    current_tag = cluster_row$tag[[1]],
    question_count = length(rows),
    parent_cluster = cluster_row$parent_cluster[[1]],
    child_clusters = children_summary,
    representative_questions = summarise_questions_for_evidence(
      state,
      representative_rows,
      scores = 1 - dist[match(representative_rows, rows)]
    ),
    diverse_questions = summarise_questions_for_evidence(state, diverse_rows),
    outlier_questions = summarise_questions_for_evidence(
      state,
      outlier_rows,
      scores = 1 - dist[match(outlier_rows, rows)]
    )
  )
}

#' Compact question text for prompts
#'
#' @param text Question text.
#' @param max_chars Maximum output length.
#'
#' @return A compact, plain-text question.
#' @keywords internal
compact_question_text_for_prompt <- function(text, max_chars = 180L) {
  if (is.null(text) || length(text) == 0L || is.na(text[[1]])) {
    return("")
  }

  text <- as.character(text[[1]])
  text <- stringr::str_replace_all(text, "<[^>]+>", " ")
  text <- stringr::str_replace_all(text, "&nbsp;|&#160;", " ")
  text <- stringr::str_replace_all(text, "&amp;", "&")
  text <- stringr::str_replace_all(text, "&lt;", "<")
  text <- stringr::str_replace_all(text, "&gt;", ">")
  text <- stringr::str_replace_all(text, "&quot;", "\"")
  text <- stringr::str_replace_all(text, "&#39;|&apos;", "'")
  text <- stringr::str_replace_all(text, "\\$\\{[^}]+\\}", "{value}")
  text <- stringr::str_squish(text)

  max_chars <- max(40L, as.integer(max_chars))
  if (nchar(text, type = "chars") > max_chars) {
    text <- paste0(substr(text, 1L, max_chars - 3L), "...")
  }

  text
}

#' Format a table of question evidence
#'
#' @param x Data frame containing `question_id` and `question_text`.
#' @param max_rows Maximum number of rows to format.
#' @param include_scores Whether to include a `score` column when present.
#' @param max_question_chars Maximum formatted length per question.
#'
#' @return A compact bulleted character string.
#' @keywords internal
format_evidence_table <- function(x, max_rows = 8L, include_scores = TRUE,
                                  max_question_chars = 180L) {
  if (is.null(x) || nrow(x) == 0) return("- none")
  x <- x[seq_len(min(max_rows, nrow(x))), , drop = FALSE]
  question_text <- purrr::map_chr(
    x$question_text,
    compact_question_text_for_prompt,
    max_chars = max_question_chars
  )
  paste0(
    "- ",
    x$question_id,
    ": ",
    question_text,
    if (isTRUE(include_scores) && "score" %in% names(x)) {
      paste0(" (score: ", round(x$score, 3), ")")
    } else {
      ""
    },
    collapse = "\n"
  )
}
