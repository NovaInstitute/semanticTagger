make_diagnostic_state <- function() {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("Age?", "School attendance?", "Enrolled?", "Highest grade?")
  )
  state <- new_tag_state(questions, run_id = "diagnostic-run")
  state$embeddings <- rbind(c(1, 0), c(0, 1), c(0.1, 0.9), c(0.2, 0.8))
  state$assignments <- tibble::tibble(
    id = questions$id,
    caption = questions$caption,
    cluster_level_1 = c(1L, 1L, 2L, 2L),
    cluster_level_2 = 1L
  )
  state$clusters <- build_cluster_index(state$assignments, c(2L, 1L))
  state$clusters$tag <- c("demographics", "education", "person")
  state
}

test_that("diagnostics flag questions that fit another centroid better", {
  diagnostics <- diagnose_tagging_clusters(
    make_diagnostic_state(),
    placement_margin = 0.1,
    neighbour_k = 2L
  )
  q2 <- diagnostics$questions[diagnostics$questions$question_id == "q2", ]

  expect_equal(q2$current_cluster, "1")
  expect_equal(q2$best_alternative_cluster, "2")
  expect_true(q2$alternative_better)
  expect_true(diagnostics$clusters$flagged[
    diagnostics$clusters$cluster_id == "1"
  ])
})

test_that("placement ranking includes current state and hierarchy paths", {
  state <- make_diagnostic_state()
  state <- register_tag_proposal(
    state, 1L, 2L, "education",
    tag_embedding = c(0, 1)
  )
  proposal_id <- names(state$proposals)[[1]]
  state <- review_tag_proposal(
    state, proposal_id, "accepted", reviewer_id = "reviewer"
  )

  ranking <- rank_question_placements(state, "q2", top_n = 2L)
  expect_equal(ranking$candidate_cluster[[1]], "2")
  expect_false(ranking$current_cluster[[1]])
  expect_gt(ranking$tag_similarity[[1]], 0.9)
  expect_match(ranking$hierarchy_path[[1]], "person > education", fixed = TRUE)
})

test_that("2D projection preserves question identities and is deterministic", {
  state <- make_diagnostic_state()
  first <- question_projection_2d(state)
  second <- question_projection_2d(state)

  expect_equal(first, second)
  expect_equal(first$question_id, state$questions$id)
  expect_true(all(is.finite(first$x)))
  expect_true(all(is.finite(first$y)))
})

test_that("diagnostics handle a single-cluster state", {
  state <- make_diagnostic_state()
  state$assignments$cluster_level_1 <- 1L
  state$clusters <- build_cluster_index(state$assignments, c(1L, 1L))

  diagnostics <- diagnose_tagging_clusters(state)
  expect_true(all(is.na(diagnostics$questions$best_alternative_cluster)))
  expect_true(all(is.na(diagnostics$questions$placement_margin)))
})
