test_that("semantic projection uses absolute and safely encoded entity IRIs", {
  records <- tag_state_to_semantic_records(
    semantic_state_fixture(), base_iri = "https://example.org/tagger",
    question_base_iri = "https://example.org/survey/question"
  )
  nodes <- unlist(records, recursive = FALSE)
  ids <- vapply(nodes, `[[`, character(1), "@id")
  expect_true(all(grepl("^https://", ids)))
  expect_false(any(grepl("semantic:run", ids, fixed = TRUE)))
  expect_true(any(grepl("semantic%3Arun%2Fone", ids, fixed = TRUE)))
  expect_silent(jsonlite::toJSON(records, auto_unbox = TRUE, null = "null"))
})

test_that("hierarchy projection stores membership only at the leaf level", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/survey/question/"
  )
  memberships <- novaTagger:::.tag_nodes_of_type(
    records, "LeafClusterMembership"
  )
  clusters <- novaTagger:::.tag_nodes_of_type(records, "QuestionCluster")
  expect_length(memberships, nrow(state$questions))
  expect_length(clusters, nrow(state$clusters))
  expect_false(any(vapply(clusters, function(node) {
    "https://data.nova.org/vocabulary/tagging/question" %in% names(node)
  }, logical(1))))
  expect_true(all(vapply(memberships, function(node) {
    grepl("/cluster/1/", node[[
      "https://data.nova.org/vocabulary/tagging/cluster"
    ]][["@id"]], fixed = TRUE)
  }, logical(1))))
})

test_that("non-structural revisions preserve hierarchy entity identities", {
  state <- semantic_state_fixture()
  state$workflow$hierarchy_version <- 3L
  state$revision <- 8L
  before <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/survey/question/"
  )

  state$revision <- 9L
  state$proposals[[1]]$status <- "edited"
  state$clusters$tag[[1]] <- "reviewed age"
  after <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/survey/question/"
  )

  hierarchy_ids <- function(records, type) {
    sort(vapply(
      novaTagger:::.tag_nodes_of_type(records$hierarchy, type),
      `[[`, character(1), "@id"
    ))
  }
  expect_equal(
    hierarchy_ids(after, "QuestionCluster"),
    hierarchy_ids(before, "QuestionCluster")
  )
  expect_equal(
    hierarchy_ids(after, "LeafClusterMembership"),
    hierarchy_ids(before, "LeafClusterMembership")
  )
  expect_true(all(grepl(
    "/hierarchy/semantic%3Arun%2Fone/3/",
    hierarchy_ids(after, "QuestionCluster"), fixed = TRUE
  )))
  expect_false(any(grepl(
    "/hierarchy/semantic%3Arun%2Fone/9/",
    hierarchy_ids(after, "QuestionCluster"), fixed = TRUE
  )))
})

test_that("parent links reconstruct every level from leaf-only membership", {
  questions <- tibble::tibble(
    id = paste0("q", 1:8), caption = paste("Question", 1:8)
  )
  embeddings <- rbind(
    c(1, 0, 0), c(.95, .05, 0), c(.7, .3, 0), c(.65, .35, 0),
    c(0, .35, .65), c(0, .3, .7), c(0, .05, .95), c(0, 0, 1)
  )
  fit <- cluster_embeddings(embeddings, c(4L, 2L))
  state <- new_tag_state(questions, "multi-level")
  state$revision <- 2L
  state$embeddings <- embeddings
  state$assignments <- add_cluster_assignments(
    questions, fit$hclust, c(4L, 2L)
  )
  state$clusters <- build_cluster_index(state$assignments, c(4L, 2L))
  state$clusters_by_level <- c(4L, 2L)
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )

  memberships <- novaTagger:::.tag_nodes_of_type(records, "LeafClusterMembership")
  cluster_nodes <- novaTagger:::.tag_nodes_of_type(records, "QuestionCluster")
  parent_property <- "https://data.nova.org/vocabulary/tagging/parentCluster"
  expect_length(memberships, 8L)
  expect_equal(sum(vapply(cluster_nodes, function(node) {
    parent_property %in% names(node)
  }, logical(1))), 4L)

  restored <- tag_state_from_semantic_records(records, questions)
  expect_equal(restored$assignments, state$assignments)
  expect_equal(restored$clusters$question_ids, state$clusters$question_ids)
})

test_that("embedding records are separate and transport neutral", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  expect_length(records$embedding, nrow(state$questions))
  expect_true(all(vapply(records$embedding, function(node) {
    identical(node[["https://data.nova.org/vocabulary/tagging/dimension"]], 2L)
  }, logical(1))))
  encoded <- jsonlite::toJSON(records$embedding, auto_unbox = TRUE)
  expect_false(grepl("@vector", encoded, fixed = TRUE))
  expect_true(grepl('"@type":"@json"', encoded, fixed = TRUE))
})

test_that("semantic records reconstruct authoritative tagging state", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  restored <- tag_state_from_semantic_records(records, state$questions)

  expect_equal(restored$run_id, state$run_id)
  expect_equal(restored$revision, state$revision)
  expect_equal(restored$status, state$status)
  expect_equal(restored$workflow, state$workflow)
  expect_equal(unname(restored$embeddings), unname(state$embeddings))
  expect_equal(restored$assignments, state$assignments)
  expect_equal(restored$clusters$level, state$clusters$level)
  expect_equal(restored$clusters$cluster_id, state$clusters$cluster_id)
  expect_equal(restored$clusters$parent_cluster, state$clusters$parent_cluster)
  expect_equal(restored$clusters$question_ids, state$clusters$question_ids)
  expect_equal(restored$clusters$tag, state$clusters$tag)
  expect_equal(names(restored$proposals), names(state$proposals))
  expect_equal(restored$proposals[[1]]$tag, state$proposals[[1]]$tag)
  expect_equal(restored$review_events[[1]]$decision,
               state$review_events[[1]]$decision)
  expect_true(is.data.frame(restored$tag_matrix))
})

test_that("semantic reconstruction rejects ambiguous projections", {
  expect_error(
    tag_state_from_semantic_records(list(), tibble::tibble(id = "q1", caption = "Age?")),
    "exactly one run"
  )
  expect_error(
    tag_state_to_semantic_records(
      semantic_state_fixture(), base_iri = "not-an-iri"
    ),
    "absolute HTTP"
  )
})
