test_that("model provider validates and delegates capabilities", {
  provider <- new_model_provider(
    name = "deterministic",
    embedding_model = "embedding-v1",
    generation_model = "generation-v1",
    embed_one = function(text, trace_callback = NULL) c(nchar(text), 1),
    embed_batch = function(texts, trace_callback = NULL) {
      lapply(texts, function(text) c(nchar(text), 1))
    },
    generate = function(prompt, max_output_tokens, temperature, format,
                        trace_callback = NULL) paste("generated", prompt)
  )
  expect_equal(model_embed(provider, "abc"), c(3, 1))
  expect_equal(model_embed_batch(provider, c("a", "abcd")),
               list(c(1, 1), c(4, 1)))
  expect_equal(model_generate(provider, "tag"), "generated tag")
  expect_true(model_provider_validate(provider))
})

test_that("embedding validation is independent of persistence", {
  provider <- new_model_provider(
    "bad", "embedding-v1", "generation-v1",
    embed_one = function(text, trace_callback = NULL) c(1, NA),
    generate = function(...) "unused"
  )
  expect_error(model_embed(provider, "question"), "finite")
  expect_false("novaRush" %in% loadedNamespaces())
})
