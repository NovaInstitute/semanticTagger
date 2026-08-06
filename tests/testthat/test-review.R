make_review_state <- function() {
  state <- new_tag_state(
    tibble::tibble(
      id = c("q1", "q2"),
      caption = c("Age?", "How old are you?")
    ),
    run_id = "review-run"
  )
  state$embeddings <- rbind(c(1, 0), c(0.8, 0.2))
  state$assignments <- tibble::tibble(
    id = c("q1", "q2"),
    caption = state$questions$caption,
    cluster_level_1 = 1L
  )
  state$clusters <- tibble::tibble(
    level = 1L,
    cluster_id = 1L,
    parent_cluster = NA_integer_,
    question_ids = list(c("q1", "q2")),
    tag = NA_character_
  )
  state
}

test_that("AI proposals remain unaccepted until a reviewer decision", {
  state <- register_tag_proposal(
    make_review_state(), 1L, 1L, "demographics",
    confidence = 0.8, provider = "test", model = "test-model",
    tag_embedding = c(0, 1)
  )

  expect_length(state$proposals, 1L)
  expect_equal(state$proposals[[1]]$status, "proposed")
  expect_true(is.na(state$clusters$tag[[1]]))
})

test_that("edited tags are re-embedded and similarity improvement is recorded", {
  state <- register_tag_proposal(
    make_review_state(), 1L, 1L, "demographics",
    tag_embedding = c(0, 1)
  )
  proposal_id <- names(state$proposals)[[1]]
  state <- review_tag_proposal(
    state,
    proposal_id,
    decision = "edited",
    reviewer_id = "reviewer-1",
    rationale = "Age is more precise.",
    tag = "age",
    embed_tag = function(tag) c(1, 0)
  )

  event <- state$review_events[[1]]
  expect_equal(state$clusters$tag[[1]], "age")
  expect_equal(state$proposals[[proposal_id]]$tag_embedding, c(1, 0))
  expect_gt(event$similarity$mean_change, 0)
  expect_true(all(event$similarity$questions$change > 0))
  expect_equal(event$reviewer_id, "reviewer-1")
})

test_that("defer is append-only and permits a later final decision", {
  state <- register_tag_proposal(
    make_review_state(), 1L, 1L, "age", tag_embedding = c(1, 0)
  )
  proposal_id <- names(state$proposals)[[1]]
  state <- review_tag_proposal(
    state, proposal_id, "deferred", reviewer_id = "reviewer-1"
  )
  expect_true(is.na(state$clusters$tag[[1]]))
  expect_equal(state$proposals[[proposal_id]]$status, "deferred")

  state <- review_tag_proposal(
    state, proposal_id, "accepted", reviewer_id = "reviewer-2"
  )
  expect_equal(state$clusters$tag[[1]], "age")
  expect_equal(vapply(state$review_events, `[[`, character(1), "decision"),
               c("deferred", "accepted"))
  expect_error(
    review_tag_proposal(state, proposal_id, "rejected", reviewer_id = "reviewer-3"),
    "Only proposed or deferred"
  )
})

test_that("rejection keeps proposal evidence but clears the cluster tag", {
  state <- register_tag_proposal(
    make_review_state(), 1L, 1L, "demographics",
    evidence = list(question_ids = c("q1", "q2")),
    tag_embedding = c(0, 1)
  )
  proposal_id <- names(state$proposals)[[1]]
  state <- review_tag_proposal(
    state, proposal_id, "rejected",
    reviewer_id = "reviewer-1", rationale = "Too broad."
  )

  expect_true(is.na(state$clusters$tag[[1]]))
  expect_equal(state$proposals[[proposal_id]]$evidence$question_ids, c("q1", "q2"))
  expect_equal(state$review_events[[1]]$decision, "rejected")
})

test_that("similarity scoring exposes likely outlier questions", {
  state <- make_review_state()
  scores <- score_cluster_tag_similarity(
    state, 1L, 1L, c(1, 0), low_similarity = 0.99
  )

  expect_equal(scores$question_id, c("q2", "q1"))
  expect_true(scores$low_similarity[[1]])
  expect_false(scores$low_similarity[[2]])
})
