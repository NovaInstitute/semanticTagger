`%||%` <- function(x, y) if (is.null(x)) y else x

.validate_embedding_vectors <- function(values, expected_n = NULL) {
  if (is.matrix(values)) {
    values <- lapply(seq_len(nrow(values)), function(i) values[i, ])
  }
  if (!is.list(values) || !length(values)) {
    stop("Embeddings must be a non-empty list or matrix.", call. = FALSE)
  }
  if (!is.null(expected_n) && length(values) != expected_n) {
    stop("Embedding count does not match expected count.", call. = FALSE)
  }
  vectors <- lapply(values, function(value) {
    if (!is.numeric(value) || !length(value) || any(!is.finite(value))) {
      stop("Embeddings must be non-empty, numeric, and finite.", call. = FALSE)
    }
    as.numeric(value)
  })
  dimensions <- lengths(vectors)
  if (length(unique(dimensions)) != 1L) {
    stop("Embeddings must have consistent dimensions.", call. = FALSE)
  }
  vectors
}
