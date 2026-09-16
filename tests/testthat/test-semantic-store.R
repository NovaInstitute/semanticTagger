test_that("semantic tag store saves and resumes pre-hierarchy progress", {
  questions <- tibble::tibble(id = c("q1", "q2"), caption = c("Age?", "Income?"))
  repository <- memory_semantic_repository()
  store <- semantic_tag_store(
    repository, questions, "semantic-store-early",
    question_base_iri = "https://example.org/question/"
  )
  workflow <- new_tagging_workflow(questions, store, run_id = "semantic-store-early")
  resumed <- resume_tagging_workflow(store)
  expect_equal(resumed$state$workflow$stage, "questions_ready")
  expect_null(resumed$state$clusters)
  expect_equal(resumed$state$revision, workflow$state$revision)
})

test_that("semantic tag store filters immutable history to current hierarchy", {
  questions <- tibble::tibble(
    id = paste0("q", 1:4), caption = c("Age?", "Age band?", "Income?", "Income band?")
  )
  repository <- memory_semantic_repository()
  store <- semantic_tag_store(
    repository, questions, "semantic-store-history",
    question_base_iri = "https://example.org/question/"
  )
  state <- new_tag_state(questions, "semantic-store-history")
  state$embeddings <- rbind(c(1, 0), c(.9, .1), c(0, 1), c(.1, .9))
  fit <- cluster_embeddings(state$embeddings, 2L)
  state$assignments <- add_cluster_assignments(questions, fit$hclust, 2L)
  state$clusters <- build_cluster_index(state$assignments, 2L)
  state$clusters_by_level <- 2L
  state$workflow <- list(
    stage = "clustered", embedding_model = "embed-v1",
    hierarchy_version = 1L
  )
  state <- tag_store_save(store, state)

  state$workflow$hierarchy_version <- 2L
  state <- tag_store_save(store, state)
  expect_equal(length(novaTagger:::.tag_nodes_of_type(
    repository$storage$partitions$hierarchy, "HierarchyVersion"
  )), 2L)

  restored <- tag_store_load(store)
  expect_equal(restored$revision, 2L)
  expect_equal(restored$workflow$hierarchy_version, 2L)
  expect_equal(restored$assignments, state$assignments)
  expect_equal(length(novaTagger:::.tag_nodes_of_type(
    novaTagger:::.current_semantic_records(repository$load("unused"))$hierarchy,
    "HierarchyVersion"
  )), 1L)
})

test_that("semantic tag store selects the newest duplicate run pointer", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  historical_run <- records$run[[1L]]
  historical_run[["https://schema.org/version"]] <- state$revision - 1L
  records$run <- c(list(historical_run), records$run)

  current <- novaTagger:::.current_semantic_records(records)
  expect_length(current$run, 1L)
  expect_equal(
    current$run[[1L]][["https://schema.org/version"]], state$revision
  )
})
