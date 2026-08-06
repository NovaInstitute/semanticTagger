# BERTopic clustering strategy and hierarchy construction.

#' Fit BERTopic and return per-document topic IDs
#'
#' @param texts Character vector of question captions.
#' @param embeddings Optional numeric matrix of precomputed embeddings.
#' @param bertopic_kwargs Named list forwarded to the BERTopic constructor.
#' @return List with topic IDs and Python objects used for tracing.
#' @export
fit_bertopic_topics <- function(texts, embeddings = NULL,
                                bertopic_kwargs = list()) {
  ensure_bertopic_python()
  texts <- as.character(texts)
  texts <- texts[!is.na(texts)]
  texts <- stringr::str_squish(texts)
  texts <- texts[nzchar(texts)]

  bertopic_mod <- reticulate::import("bertopic")
  kwargs <- utils::modifyList(
    list(
      calculate_probabilities = FALSE,
      verbose = TRUE,
      low_memory = TRUE
    ),
    bertopic_kwargs
  )
  if (is.null(kwargs$umap_model)) {
    umap_mod <- reticulate::import("umap")
    kwargs$umap_model <- umap_mod$UMAP(
      n_neighbors = 15L,
      n_components = 5L,
      min_dist = 0,
      metric = "cosine",
      random_state = 42L,
      low_memory = TRUE,
      n_jobs = 1L
    )
  }
  if (is.null(kwargs$hdbscan_model)) {
    hdbscan_mod <- reticulate::import("hdbscan")
    kwargs$hdbscan_model <- hdbscan_mod$HDBSCAN(
      min_cluster_size = 10L,
      metric = "euclidean",
      cluster_selection_method = "eom",
      prediction_data = FALSE,
      core_dist_n_jobs = 1L
    )
  }
  model <- do.call(bertopic_mod$BERTopic, kwargs)
  if (!is.null(embeddings)) {
    np <- reticulate::import("numpy", delay_load = TRUE)
    embeddings <- np$ascontiguousarray(
      reticulate::r_to_py(embeddings),
      dtype = np$float32
    )
  }
  gc(verbose = FALSE)
  fit <- if (is.null(embeddings)) {
    model$fit_transform(texts)
  } else {
    model$fit_transform(texts, embeddings = embeddings)
  }
  topic_ids <- tryCatch(
    reticulate::py_to_r(fit[[1]]),
    error = function(e) reticulate::py_to_r(fit)
  )
  list(topic_ids = as.integer(topic_ids), model = model, fit_raw = fit)
}

#' Infer hierarchy levels from BERTopic topics
#'
#' @param questions Data frame with a `caption` column.
#' @param question_embeddings List or matrix of question embeddings.
#' @param bertopic_kwargs Named list forwarded to [fit_bertopic_topics()].
#' @param include_outlier_topic Whether to count BERTopic's `-1` topic.
#' @return Integer hierarchy vector.
#' @export
infer_bertopic_clusters_by_level <- function(
    questions, question_embeddings, bertopic_kwargs = list(),
    include_outlier_topic = FALSE) {
  stopifnot("caption" %in% names(questions))
  embedding_matrix <- if (is.list(question_embeddings)) {
    do.call(rbind, question_embeddings)
  } else question_embeddings
  bertopic <- fit_bertopic_topics(
    questions$caption, embedding_matrix, bertopic_kwargs
  )
  topic_ids <- unique(as.integer(bertopic$topic_ids))
  topic_ids <- topic_ids[!is.na(topic_ids)]
  non_outlier <- setdiff(topic_ids, -1L)
  if (!isTRUE(include_outlier_topic) && length(non_outlier)) {
    topic_ids <- non_outlier
  }
  infer_topic_levels(max(1L, min(length(topic_ids), nrow(questions))))
}

#' Build hierarchical assignments from BERTopic leaf topics
#'
#' @param questions Data frame with `id`, `caption`, and `topic_id`.
#' @param question_embeddings List or matrix of question embeddings.
#' @param topic_levels Optional full hierarchy vector.
#' @return Questions, assignments, clusters, and cluster counts by level.
#' @export
build_bertopic_hierarchy <- function(questions, question_embeddings,
                                     topic_levels = NULL) {
  stopifnot(all(c("id", "caption", "topic_id") %in% names(questions)))
  topic_map <- sort(unique(questions$topic_id))
  topic_index <- seq_along(topic_map)
  names(topic_index) <- as.character(topic_map)
  leaf_cluster <- unname(topic_index[as.character(questions$topic_id)])
  embedding_matrix <- if (is.list(question_embeddings)) {
    do.call(rbind, question_embeddings)
  } else question_embeddings
  questions$embedding <- lapply(
    seq_len(nrow(embedding_matrix)),
    function(i) embedding_matrix[i, ]
  )
  assignments <- questions[, c("id", "caption"), drop = FALSE]
  assignments$cluster_level_1 <- as.integer(leaf_cluster)
  n_leaf <- length(topic_map)
  if (is.null(topic_levels)) topic_levels <- infer_topic_levels(n_leaf)
  topic_levels[[1]] <- n_leaf

  if (length(topic_levels) > 1L && n_leaf > 1L) {
    topic_centroids <- lapply(seq_len(n_leaf), function(cluster_id) {
      rows <- which(leaf_cluster == cluster_id)
      colMeans(embedding_matrix[rows, , drop = FALSE])
    })
    topic_tree <- stats::hclust(
      stats::dist(do.call(rbind, topic_centroids)),
      method = "ward.D2"
    )
    for (level in seq_along(topic_levels)[-1]) {
      topic_group <- stats::cutree(topic_tree, k = topic_levels[[level]])
      assignments[[paste0("cluster_level_", level)]] <-
        topic_group[leaf_cluster]
    }
  }
  clusters <- build_cluster_index(assignments, topic_levels)
  list(
    questions = questions,
    assignments = assignments,
    clusters = clusters,
    clusters_by_level = topic_levels
  )
}

#' Infer a descending hierarchy from a leaf-topic count
#'
#' @param n_leaf Number of leaf topics.
#' @return Integer vector from leaf count toward the root.
#' @export
infer_topic_levels <- function(n_leaf) {
  if (n_leaf <= 1L) return(1L)
  levels <- c(n_leaf)
  current <- n_leaf
  while (current > 2L) {
    current <- max(2L, as.integer(ceiling(current / 2)))
    levels <- c(levels, current)
  }
  unique(as.integer(levels))
}
