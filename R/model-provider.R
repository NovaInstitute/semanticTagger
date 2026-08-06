#' Create a model-provider adapter
#'
#' @param name Provider identifier.
#' @param embedding_model,generation_model Model identifiers.
#' @param embed_one Function accepting `(text, trace_callback)`.
#' @param generate Function accepting `(prompt, max_output_tokens, temperature,
#'   format, trace_callback)`.
#' @param embed_batch Optional function accepting `(texts, trace_callback)`.
#' @param validate Optional zero-argument connection validation function.
#' @param metadata Optional non-secret provider metadata.
#'
#' @return A `model_provider`.
#' @export
new_model_provider <- function(name, embedding_model, generation_model,
                               embed_one, generate, embed_batch = NULL,
                               validate = NULL, metadata = list()) {
  values <- c(name, embedding_model, generation_model)
  if (anyNA(values) || any(!nzchar(as.character(values)))) {
    stop("Provider and model names must be non-empty.", call. = FALSE)
  }
  if (!is.function(embed_one) || !is.function(generate)) {
    stop("A model provider requires embedding and generation functions.", call. = FALSE)
  }
  if (!is.null(embed_batch) && !is.function(embed_batch)) {
    stop("`embed_batch` must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(validate) && !is.function(validate)) {
    stop("`validate` must be NULL or a function.", call. = FALSE)
  }
  if (!is.list(metadata)) stop("`metadata` must be a list.", call. = FALSE)
  structure(
    list(
      name = as.character(name),
      embedding_model = as.character(embedding_model),
      generation_model = as.character(generation_model),
      embed_one = embed_one,
      embed_batch = embed_batch,
      generate = generate,
      validate = validate,
      metadata = metadata
    ),
    class = "model_provider"
  )
}

validate_model_provider <- function(provider) {
  if (!inherits(provider, "model_provider")) {
    stop("`provider` must be a model_provider.", call. = FALSE)
  }
  invisible(provider)
}

.validate_provider_embeddings <- function(values, expected_n) {
  .validate_embedding_vectors(values, expected_n = expected_n)
}

#' Embed text with a model provider
#'
#' @param provider A `model_provider`.
#' @param text Character scalar.
#' @param trace_callback Optional structured trace callback.
#' @return Numeric embedding.
#' @export
model_embed <- function(provider, text, trace_callback = NULL) {
  validate_model_provider(provider)
  text <- as.character(text)
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    stop("`text` must be one non-empty string.", call. = FALSE)
  }
  .validate_provider_embeddings(
    list(provider$embed_one(text, trace_callback)),
    expected_n = 1L
  )[[1]]
}

#' Embed a batch with a model provider
#'
#' @param provider A `model_provider`.
#' @param texts Character vector.
#' @param trace_callback Optional structured trace callback.
#' @return List of numeric embeddings in input order.
#' @export
model_embed_batch <- function(provider, texts, trace_callback = NULL) {
  validate_model_provider(provider)
  texts <- as.character(texts)
  if (!length(texts) || anyNA(texts) || any(!nzchar(texts))) {
    stop("`texts` must contain non-empty strings.", call. = FALSE)
  }
  values <- if (is.function(provider$embed_batch)) {
    provider$embed_batch(texts, trace_callback)
  } else {
    lapply(texts, function(text) provider$embed_one(text, trace_callback))
  }
  .validate_provider_embeddings(values, expected_n = length(texts))
}

#' Generate text with a model provider
#'
#' @param provider A `model_provider`.
#' @param prompt Prompt text.
#' @param max_output_tokens Maximum output length.
#' @param temperature Optional sampling temperature.
#' @param format Optional requested format such as `"json"`.
#' @param trace_callback Optional structured trace callback.
#' @return Generated character scalar.
#' @export
model_generate <- function(provider, prompt, max_output_tokens = 256L,
                           temperature = NULL, format = NULL,
                           trace_callback = NULL) {
  validate_model_provider(provider)
  prompt <- as.character(prompt)
  if (length(prompt) != 1L || is.na(prompt) || !nzchar(prompt)) {
    stop("`prompt` must be one non-empty string.", call. = FALSE)
  }
  value <- provider$generate(
    prompt, as.integer(max_output_tokens), temperature, format, trace_callback
  )
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Model provider returned empty generated text.", call. = FALSE)
  }
  value
}

#' Validate a configured model provider
#'
#' @param provider A `model_provider`.
#' @return `TRUE` when no validation function is required or validation passes.
#' @export
model_provider_validate <- function(provider) {
  validate_model_provider(provider)
  if (is.null(provider$validate)) return(TRUE)
  isTRUE(provider$validate())
}
