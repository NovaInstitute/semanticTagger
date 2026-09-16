#' Configure the OpenAI model provider
#'
#' Secrets are read at runtime and are never included in provider metadata.
#'
#' @param api_key OpenAI API key. Defaults to `OPENAI_API_KEY`.
#' @param base_url OpenAI API base URL.
#' @param embed_model Embedding model identifier.
#' @param dimensions Optional output dimensions for embedding models that
#'   support shortened embeddings. Use `NULL` to request the model default.
#' @param generation_model Responses API model identifier.
#' @return An OpenAI provider configuration list.
#' @export
openai_config <- function(
    api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
    base_url = Sys.getenv("OPENAI_BASE_URL", unset = "https://api.openai.com/v1"),
    embed_model = Sys.getenv("OPENAI_EMBED_MODEL", unset = "text-embedding-3-small"),
    dimensions = NULL,
    generation_model = Sys.getenv("OPENAI_TAGGER_MODEL", unset = "gpt-5.4-mini")) {
  if (!is.null(dimensions)) {
    dimensions <- suppressWarnings(as.integer(dimensions))
    if (length(dimensions) != 1L || is.na(dimensions) || dimensions < 1L) {
      stop("`dimensions` must be NULL or one positive integer.", call. = FALSE)
    }
  }
  list(
    api_key = as.character(api_key),
    base_url = sub("/+$", "", as.character(base_url)),
    embed_model = as.character(embed_model),
    dimensions = dimensions,
    generation_model = as.character(generation_model)
  )
}

.openai_request <- function(config, endpoint, body, timeout_sec, trace_callback = NULL) {
  if (!nzchar(config$api_key)) {
    stop("OPENAI_API_KEY is not set.", call. = FALSE)
  }
  emit <- function(direction, value) {
    if (is.function(trace_callback)) trace_callback(list(
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      system = "openai", direction = direction, endpoint = endpoint,
      body = value
    ))
  }
  emit("request", body)
  request <- httr2::request(paste0(config$base_url, endpoint)) |>
    httr2::req_headers(
      Authorization = paste("Bearer", config$api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_error(is_error = function(response) FALSE)
  response <- tryCatch(httr2::req_perform(request), error = function(error) {
    stop("OpenAI request failed before a response was received: ",
         conditionMessage(error), call. = FALSE)
  })
  status <- httr2::resp_status(response)
  if (status >= 400L) {
    stop(
      "OpenAI request failed: HTTP ", status,
      "\nURL: ", paste0(config$base_url, endpoint),
      "\nModel: ", body$model %||% "<none>",
      "\nResponse body: ", httr2::resp_body_string(response),
      call. = FALSE
    )
  }
  parsed <- httr2::resp_body_json(response, simplifyVector = FALSE)
  traced <- parsed
  if (identical(endpoint, "/embeddings")) {
    traced$data <- lapply(traced$data %||% list(), function(item) {
      item$embedding <- list(`__vector__` = TRUE, dimension = length(item$embedding))
      item
    })
  }
  emit("response", traced)
  parsed
}

.openai_response_text <- function(response) {
  if (!is.null(response$output_text)) {
    return(paste(as.character(response$output_text), collapse = "\n"))
  }
  chunks <- unlist(lapply(response$output %||% list(), function(item) {
    vapply(item$content %||% list(), function(part) {
      as.character(part$text %||% part$output_text %||% "")
    }, character(1))
  }), use.names = FALSE)
  paste(chunks[nzchar(chunks)], collapse = "\n")
}

#' Create an OpenAI model-provider adapter
#'
#' Generation uses the Responses API and embeddings use the Embeddings API.
#'
#' @param config Output from [openai_config()].
#' @return A `model_provider`.
#' @export
openai_model_provider <- function(config = openai_config()) {
  new_model_provider(
    name = "openai",
    embedding_model = config$embed_model,
    generation_model = config$generation_model,
    embed_one = function(text, trace_callback = NULL) {
      .openai_embed_batch(config, text, trace_callback)[[1]]
    },
    embed_batch = function(texts, trace_callback = NULL) {
      .openai_embed_batch(config, texts, trace_callback)
    },
    generate = function(prompt, max_output_tokens, temperature = NULL,
                        format = NULL, trace_callback = NULL) {
      body <- list(
        model = config$generation_model,
        input = prompt,
        max_output_tokens = max(64L, as.integer(max_output_tokens))
      )
      if (!is.null(temperature)) body$temperature <- temperature
      if (identical(format, "json")) {
        body$text <- list(format = list(type = "json_object"))
      }
      response <- .openai_request(
        config, "/responses", body, 250, trace_callback
      )
      .openai_response_text(response)
    },
    validate = function() {
      if (!nzchar(config$api_key)) {
        stop("OPENAI_API_KEY is not set.", call. = FALSE)
      }
      TRUE
    },
    metadata = list(
      base_url = config$base_url,
      embedding_dimensions = config$dimensions
    )
  )
}

.openai_embed_batch <- function(config, texts, trace_callback = NULL) {
  body <- list(model = config$embed_model, input = unname(as.character(texts)))
  if (!is.null(config$dimensions)) body$dimensions <- config$dimensions
  response <- .openai_request(
    config, "/embeddings", body,
    120, trace_callback
  )
  items <- response$data %||% list()
  if (length(items) != length(texts)) {
    stop("OpenAI returned an unexpected embedding count.", call. = FALSE)
  }
  indices <- vapply(items, function(item) as.integer(item$index %||% 0L), integer(1)) + 1L
  if (!setequal(indices, seq_along(texts))) {
    stop("OpenAI returned invalid embedding indices.", call. = FALSE)
  }
  output <- vector("list", length(texts))
  for (i in seq_along(items)) output[[indices[[i]]]] <- as.numeric(items[[i]]$embedding)
  output
}
