#' Configure the optional Ollama model provider
#'
#' @param base_url Ollama server URL.
#' @param embed_model Embedding model identifier.
#' @param generation_model Generation model identifier.
#' @return An Ollama provider configuration list.
#' @export
ollama_config <- function(
    base_url = Sys.getenv("OLLAMA_BASE_URL", unset = "http://localhost:11434"),
    embed_model = Sys.getenv("OLLAMA_EMBED_MODEL", unset = "nomic-embed-text"),
    generation_model = Sys.getenv("OLLAMA_TAGGER_MODEL", unset = "llama3.1:8b")) {
  list(
    base_url = sub("/+$", "", as.character(base_url)),
    embed_model = as.character(embed_model),
    generation_model = as.character(generation_model)
  )
}

.ollama_request <- function(config, endpoint, body, timeout_sec) {
  response <- httr2::request(paste0(config$base_url, endpoint)) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_error(is_error = function(response) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(response)
  if (status >= 400L) {
    stop(
      "Ollama request failed: HTTP ", status,
      "\nURL: ", paste0(config$base_url, endpoint),
      "\nModel: ", body$model %||% "<none>",
      "\nResponse body: ", httr2::resp_body_string(response),
      call. = FALSE
    )
  }
  httr2::resp_body_json(response, simplifyVector = TRUE)
}

#' Create an optional Ollama model-provider adapter
#'
#' @param config Output from [ollama_config()].
#' @return A `model_provider`.
#' @export
ollama_model_provider <- function(config = ollama_config()) {
  emit <- function(callback, direction, operation, body) {
    if (is.function(callback)) callback(list(
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      system = "ollama", direction = direction, operation = operation,
      body = body
    ))
  }
  embed <- function(text, trace_callback = NULL) {
    body <- list(model = config$embed_model, prompt = text)
    emit(trace_callback, "request", "embed", body)
    parsed <- .ollama_request(config, "/api/embeddings", body, 120)
    value <- as.numeric(parsed$embedding)
    emit(trace_callback, "response", "embed", list(
      model = config$embed_model, embedding_dimension = length(value)
    ))
    value
  }
  generate <- function(prompt, max_output_tokens, temperature = NULL,
                       format = NULL, trace_callback = NULL) {
    body <- list(
      model = config$generation_model, prompt = prompt, stream = FALSE,
      options = list(
        num_predict = as.integer(max_output_tokens),
        temperature = temperature %||% 0.2
      )
    )
    if (!is.null(format)) body$format <- format
    emit(trace_callback, "request", "generate", body)
    parsed <- .ollama_request(config, "/api/generate", body, 250)
    value <- as.character(parsed$response %||% parsed$message$content %||% "")
    emit(trace_callback, "response", "generate", list(
      model = config$generation_model, response = value
    ))
    value
  }
  new_model_provider(
    name = "ollama",
    embedding_model = config$embed_model,
    generation_model = config$generation_model,
    embed_one = embed,
    generate = generate,
    validate = function() nzchar(generate("Return the word ok.", 3L, 0)),
    metadata = list(base_url = config$base_url)
  )
}
