test_that("OpenAI adapter conforms to the shared provider contract", {
  config <- openai_config(
    api_key = "test-secret", base_url = "https://example.invalid/v1/",
    embed_model = "embed-test", generation_model = "generation-test"
  )
  calls <- list()
  testthat::with_mocked_bindings(
    {
      provider <- openai_model_provider(config)
      expect_equal(model_embed(provider, "age"), c(1, 0))
      expect_equal(
        model_embed_batch(provider, c("age", "income")),
        list(c(1, 0), c(0, 1))
      )
      expect_equal(
        model_generate(provider, "tag this", format = "json"),
        '{"tag":"age"}'
      )
      expect_true(model_provider_validate(provider))
      expect_equal(provider$metadata$base_url, "https://example.invalid/v1")
      expect_false("api_key" %in% names(provider$metadata))
    },
    .openai_request = function(config, endpoint, body, timeout_sec,
                               trace_callback = NULL) {
      calls[[length(calls) + 1L]] <<- list(endpoint = endpoint, body = body)
      if (identical(endpoint, "/embeddings")) {
        data <- lapply(rev(seq_along(body$input)), function(i) {
          list(index = i - 1L, embedding = if (i == 1L) c(1, 0) else c(0, 1))
        })
        return(list(data = data))
      }
      list(output = list(list(content = list(list(text = '{"tag":"age"}')))))
    },
    .package = "novaTagger"
  )
  expect_equal(vapply(calls, `[[`, character(1), "endpoint"),
               c("/embeddings", "/embeddings", "/responses"))
  expect_equal(calls[[3]]$body$text$format$type, "json_object")
})

test_that("OpenAI configuration requires a runtime secret", {
  provider <- openai_model_provider(openai_config(api_key = ""))
  expect_error(model_provider_validate(provider), "OPENAI_API_KEY")
  expect_error(model_embed(provider, "age"), "OPENAI_API_KEY")
})

test_that("OpenAI response extraction supports both response layouts", {
  expect_equal(novaTagger:::.openai_response_text(list(output_text = "done")), "done")
  nested <- list(output = list(list(content = list(
    list(text = "hello"), list(text = "world")
  ))))
  expect_equal(novaTagger:::.openai_response_text(nested), "hello\nworld")
})

test_that("Ollama adapter conforms to the shared provider contract", {
  config <- ollama_config(
    base_url = "http://example.invalid/", embed_model = "embed-test",
    generation_model = "generation-test"
  )
  events <- list()
  callback <- function(event) events[[length(events) + 1L]] <<- event
  testthat::with_mocked_bindings(
    {
      provider <- ollama_model_provider(config)
      expect_equal(model_embed_batch(provider, c("a", "b"), callback),
                   list(c(1, 0), c(1, 0)))
      expect_equal(model_generate(provider, "tag", trace_callback = callback), "ok")
      expect_true(model_provider_validate(provider))
      expect_equal(provider$metadata$base_url, "http://example.invalid")
    },
    .ollama_request = function(config, endpoint, body, timeout_sec) {
      if (identical(endpoint, "/api/embeddings")) {
        return(list(embedding = c(1, 0)))
      }
      list(response = "ok")
    },
    .package = "novaTagger"
  )
  expect_true(length(events) >= 6L)
  expect_true(all(vapply(events, function(event) event$system == "ollama", logical(1))))
})

test_that("provider configuration reads namespaced environment variables", {
  withr::local_envvar(c(
    OPENAI_EMBED_MODEL = "openai-embed",
    OPENAI_TAGGER_MODEL = "openai-generate",
    OLLAMA_EMBED_MODEL = "ollama-embed",
    OLLAMA_TAGGER_MODEL = "ollama-generate"
  ))
  expect_equal(openai_config(api_key = "test")$embed_model, "openai-embed")
  expect_equal(openai_config(api_key = "test")$generation_model, "openai-generate")
  expect_equal(ollama_config()$embed_model, "ollama-embed")
  expect_equal(ollama_config()$generation_model, "ollama-generate")
})
