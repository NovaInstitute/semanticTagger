semantic_state_fixture <- function() {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("Age?", "Age group?", "Income?", "Income group?")
  )
  embeddings <- rbind(c(1, 0), c(.9, .1), c(0, 1), c(.1, .9))
  fit <- cluster_embeddings(embeddings, 2L)
  state <- new_tag_state(questions, "semantic:run/one")
  state$revision <- 7L
  state$embeddings <- embeddings
  state$assignments <- add_cluster_assignments(questions, fit$hclust, 2L)
  state$clusters <- build_cluster_index(state$assignments, 2L)
  state$clusters_by_level <- 2L
  state$workflow <- list(
    stage = "review", embedding_model = "embed-v1",
    generation_model = "generate-v1", clustering_method = "hierarchical_ward_d2"
  )
  state <- register_tag_proposal(
    state, 1L, state$clusters$cluster_id[[1]], "age", confidence = .9,
    rationale = "age questions", provider = "fixture", model = "generate-v1",
    embedding_model = "embed-v1", tag_embedding = c(1, 0),
    evidence = list(source = "fixture")
  )
  proposal_id <- names(state$proposals)[[1]]
  review_tag_proposal(
    state, proposal_id, "accepted", "reviewer-1", "looks coherent"
  )
}
