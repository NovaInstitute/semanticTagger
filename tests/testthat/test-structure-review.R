make_reclassification_state <- function() {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("Age?", "School attendance?", "Enrolled?", "Highest grade?")
  )
  state <- new_tag_state(questions, run_id = "structure-run")
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

test_that("reclassification preview does not mutate authoritative state", {
  state <- make_reclassification_state()
  original <- state$assignments

  preview <- preview_question_reclassification(state, "q2", 2L)

  expect_s3_class(preview, "structure_change_preview")
  expect_equal(state$assignments, original)
  expect_equal(
    preview$proposed_state$assignments$cluster_level_1[
      preview$proposed_state$assignments$id == "q2"
    ],
    2L
  )
  source <- preview$metrics[preview$metrics$cluster_id == "1", ]
  expect_gt(source$mean_similarity_change, 0)
})

test_that("applying a preview records provenance and invalidates affected tags", {
  state <- register_tag_proposal(
    make_reclassification_state(), 1L, 1L, "demographics",
    tag_embedding = c(1, 0)
  )
  preview <- preview_question_reclassification(state, "q2", 2L)
  updated <- apply_structure_change(
    state, preview, reviewer_id = "reviewer-1",
    rationale = "Question aligns with education."
  )

  expect_equal(updated$assignments$cluster_level_1[updated$assignments$id == "q2"], 2L)
  expect_length(updated$structure_events, 1L)
  expect_equal(updated$structure_events[[1]]$reviewer_id, "reviewer-1")
  expect_true(all(is.na(updated$clusters$tag[updated$clusters$level == 1L])))
  expect_equal(updated$proposals[[1]]$status, "superseded")
  expect_null(updated$cleaned)
  expect_null(updated$audit)
})

test_that("stale previews cannot overwrite newer state", {
  state <- make_reclassification_state()
  preview <- preview_question_reclassification(state, "q2", 2L)
  state$revision <- state$revision + 1L

  expect_error(
    apply_structure_change(state, preview, reviewer_id = "reviewer-1"),
    "changed after this preview"
  )
})

test_that("discarding a preview causes no project-state mutation", {
  state <- make_reclassification_state()
  preview <- preview_question_reclassification(state, "q2", 2L)
  discarded <- discard_structure_change(preview)

  expect_equal(discarded$status, "discarded")
  expect_length(state$structure_events, 0L)
  expect_equal(state$assignments$cluster_level_1, c(1L, 1L, 2L, 2L))
})

test_that("reclassification validates questions and destinations", {
  state <- make_reclassification_state()
  expect_error(
    preview_question_reclassification(state, "missing", 2L),
    "Unknown question"
  )
  expect_error(
    preview_question_reclassification(state, "q2", 99L),
    "Destination leaf cluster"
  )
  expect_error(
    preview_question_reclassification(state, "q1", 1L),
    "already in"
  )
})
