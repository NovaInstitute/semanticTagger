test_that("provider-neutral core supports a resumable reviewed tagging flow", {
  questions <- tibble::tibble(
    id = paste0("q", 1:6),
    caption = c(
      "How old are you?", "What is your age?", "Which age group are you in?",
      "What is your income?", "What is your salary?", "Which income band applies?"
    )
  )
  embeddings <- rbind(
    c(1.00, 0.05), c(0.98, 0.10), c(0.95, 0.15),
    c(0.05, 1.00), c(0.10, 0.98), c(0.15, 0.95)
  )
  provider <- new_model_provider(
    name = "deterministic",
    embedding_model = "fixture-embedding",
    generation_model = "fixture-generation",
    embed_one = function(text, trace_callback) c(1, 0),
    generate = function(prompt, max_output_tokens, temperature, format,
                        trace_callback) {
      '{"tag":"age","confidence":0.95,"rationale":"age questions","needs_review":false}'
    }
  )

  fit <- cluster_embeddings(embeddings, clusters_by_level = 2L)
  assignments <- add_cluster_assignments(questions, fit$hclust, 2L)
  state <- new_tag_state(questions, run_id = "core-flow")
  state$embeddings <- embeddings
  state$hclust <- fit$hclust
  state$assignments <- assignments
  state$clusters <- build_cluster_index(assignments, 2L)

  diagnostics <- diagnose_tagging_clusters(state, neighbour_k = 2L)
  expect_equal(nrow(diagnostics$questions), nrow(questions))
  expect_equal(nrow(diagnostics$clusters), 2L)

  raw <- model_generate(provider, "Propose a tag", format = "json")
  proposal <- parse_tag_proposal_response(raw)
  cluster_id <- state$clusters$cluster_id[[1]]
  state <- register_tag_proposal(
    state,
    level = 1L,
    cluster_id = cluster_id,
    tag = proposal$tag,
    confidence = proposal$confidence,
    rationale = proposal$rationale,
    needs_review = proposal$needs_review,
    provider = provider$name,
    model = provider$generation_model,
    embedding_model = provider$embedding_model,
    tag_embedding = model_embed(provider, proposal$tag)
  )
  proposal_id <- names(state$proposals)[[1]]
  state <- review_tag_proposal(
    state, proposal_id, decision = "accepted", reviewer_id = "reviewer-1"
  )

  expect_equal(state$proposals[[proposal_id]]$status, "accepted")
  expect_equal(state$clusters$tag[state$clusters$cluster_id == cluster_id], "age")
  expect_length(state$review_events, 1L)
  expect_true(is.list(state$review_events[[1]]$similarity))

  store <- memory_tag_store()
  saved <- tag_store_save(store, state)
  resumed <- tag_store_load(store)
  expect_equal(resumed$run_id, "core-flow")
  expect_equal(resumed$revision, 1L)
  expect_equal(resumed$proposals[[proposal_id]]$status, "accepted")
  expect_equal(saved$revision, resumed$revision)
})
