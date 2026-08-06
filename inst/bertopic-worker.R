args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) stop("Expected input, output, kwargs, and source-directory arguments.")

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  NUMBA_NUM_THREADS = "1",
  NUMBA_THREADING_LAYER = "workqueue",
  TOKENIZERS_PARALLELISM = "false"
)

input_path <- args[[1]]
output_path <- args[[2]]
kwargs_path <- args[[3]]
source_dir <- args[[4]]

if (nzchar(source_dir)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools is required for a source-tree BERTopic worker.")
  }
  devtools::load_all(source_dir, quiet = TRUE)
} else {
  library(novaTagger)
}

input <- readRDS(input_path)
kwargs <- readRDS(kwargs_path)
if (!is.list(input) || !is.data.frame(input$questions) ||
    is.null(input$embeddings)) {
  stop("BERTopic worker input requires `questions` and `embeddings`.")
}
embedding_matrix <- if (is.list(input$embeddings)) {
  do.call(rbind, input$embeddings)
} else input$embeddings
fit <- fit_bertopic_topics(
  input$questions$caption, embedding_matrix, bertopic_kwargs = kwargs
)
questions <- input$questions
questions$topic_id <- fit$topic_ids
result <- build_bertopic_hierarchy(questions, embedding_matrix)
saveRDS(result, output_path)
