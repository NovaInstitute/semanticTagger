#' Build binary clustering levels up to k
#'
#' @param k_deep Desired number of clusters at the deepest level (this must be a power of 2).
#'
#' @return Integer vector like c(k, k/2, ..., 2) for use as clusters_by_level.
#'         Level 1 = most granular (k clusters), last level = 2 clusters.
#'
#' @examples
#' binary_levels(8)
#' @export
binary_levels <- function(k_deep) {
  if (k_deep < 2) stop("Number of clusters at the deepest level (k_deep) must be >= 2.")

  # Enforce power-of-two for clean binary splits
  if (bitwAnd(k_deep, k_deep - 1L) != 0L) {
    stop("Number of clusters at the deepest level (k_deep) must be a power of 2 (2, 4, 8, 16, ...).")
  }

  # 2, 4, 8, ..., k_deep  ->  k_deep, ..., 4, 2
  levels <- 2L^(1L:as.integer(log2(k_deep)))
  rev(levels)
}


#' Cluster embeddings and return hclust object
#'
#' @param embeddings List or matrix of embeddings.
#' @param clusters_by_level Integer vector of cluster counts by level.
#'
#' @return A list with hclust and distance matrix.
#'
#' @examples
#' embeddings <- matrix(rnorm(40), ncol = 4)
#' cluster_embeddings(embeddings, clusters_by_level = c(4, 2))
#' @export
cluster_embeddings <- function(embeddings, clusters_by_level) {
  if (is.list(embeddings)) {
    emb_matrix <- do.call(rbind, embeddings)
  } else {
    emb_matrix <- embeddings
  }
  tree <- stats::hclust(stats::dist(emb_matrix), method = "ward.D2")
  list(hclust = tree, dist = stats::dist(emb_matrix), clusters_by_level = clusters_by_level)
}

#' Add cluster assignments to question table
#'
#' @param questions Tibble with question data.
#' @param hclust Hierarchical clustering object.
#' @param clusters_by_level Integer vector of cluster counts by level.
#'
#' @return Tibble with cluster_level_* columns.
#'
#' @examples
#' questions <- tibble::tibble(id = c("q_001", "q_002"), caption = c("Age?", "Income?"))
#' embeddings <- matrix(rnorm(20), ncol = 10)
#' tree <- stats::hclust(stats::dist(embeddings))
#' add_cluster_assignments(questions, tree, clusters_by_level = c(2))
#' @export
add_cluster_assignments <- function(questions, hclust, clusters_by_level) {
  level_cols <- paste0("cluster_level_", seq_along(clusters_by_level))
  assignments <- questions
  for (i in seq_along(clusters_by_level)) {
    assignments[[level_cols[[i]]]] <- stats::cutree(hclust, k = clusters_by_level[[i]])
  }
  assignments
}


#' Summarise cluster memberships across levels
#'
#' @description
#' Creates a per-cluster index that includes parent links and question ids.
#'
#' @param assignments Question table with `cluster_level_*` columns.
#' @param clusters_by_level Integer cluster counts ordered leaf-to-root.
#'
#' @return A tibble with one row per cluster and columns:
#'   \itemize{
#'     \item \code{level}
#'     \item \code{cluster_id}
#'     \item \code{parent_cluster}
#'     \item \code{question_ids}
#'     \item \code{tag}
#'   }
#' @keywords internal
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom rlang .data
build_cluster_index <- function(assignments, clusters_by_level) {
  level_cols <- paste0("cluster_level_", seq_along(clusters_by_level))
  max_level <- length(level_cols)

  purrr::map_dfr(seq_along(level_cols), function(level_idx) {
    this_col <- level_cols[[level_idx]]
    parent_col <- if (level_idx < max_level) level_cols[[level_idx + 1]] else NA_character_

    assignments %>%
      group_by(cluster_id = .data[[this_col]]) %>%
      summarise(
        level = level_idx,
        parent_cluster = if (!is.na(parent_col)) unique(.data[[parent_col]])[1] else NA_integer_,
        question_ids = list(.data$id),
        .groups = "drop"
      ) %>%
      mutate(tag = NA_character_)
  }) %>%
    arrange(.data$level, .data$cluster_id)
}
