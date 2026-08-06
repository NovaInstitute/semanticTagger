make_evidence_state <- function(use_matrix = TRUE) {
  questions <- tibble::tibble(
    id = c("q1", "q2", "q3", "q4"),
    caption = c("Cooking fuel?", "Which stove?", "Age?", "School attendance?"),
    embedding = list(c(1, 0), c(0.8, 0.2), c(0, 1), c(0.2, 0.8))
  )
  state <- structure(
    list(
      questions = questions,
      assignments = tibble::tibble(
        id = questions$id,
        cluster_level_1 = c(1L, 1L, 2L, 2L),
        cluster_level_2 = c(1L, 1L, 1L, 1L)
      ),
      clusters = tibble::tibble(
        level = c(1L, 1L, 2L),
        cluster_id = c(1L, 2L, 1L),
        parent_cluster = c(1L, 1L, NA_integer_),
        question_ids = list(c("q1", "q2"), c("q3", "q4"), questions$id),
        tag = c("cooking", "demographics", "survey topics")
      )
    ),
    class = "tag_state"
  )
  if (isTRUE(use_matrix)) {
    state$embeddings <- do.call(rbind, questions$embedding)
  }
  state
}

test_that("state embeddings can come from a matrix or question list-column", {
  expected <- rbind(c(1, 0), c(0.8, 0.2), c(0, 1), c(0.2, 0.8))
  expect_equal(state_embedding_matrix(make_evidence_state()), expected)
  expect_equal(state_embedding_matrix(make_evidence_state(FALSE)), expected)

  invalid <- make_evidence_state(FALSE)
  invalid$questions$embedding <- NULL
  expect_error(state_embedding_matrix(invalid), "must contain embeddings")
})

test_that("cluster rows validate and resolve hierarchy assignments", {
  state <- make_evidence_state()
  expect_equal(cluster_question_rows(state, 1L, 1L), c(1L, 2L))
  expect_error(cluster_question_rows(state, 3L, 1L), "Missing assignment column")
})

test_that("cluster profiles contain representative, diverse, and outlier evidence", {
  profile <- get_cluster_profile(make_evidence_state(), cluster_id = 1L, level = 1L, sample_size = 2L)

  expect_equal(profile$question_count, 2L)
  expect_setequal(profile$representative_questions$question_id, c("q1", "q2"))
  expect_setequal(profile$diverse_questions$question_id, c("q1", "q2"))
  expect_setequal(profile$outlier_questions$question_id, c("q1", "q2"))
  expect_equal(nrow(profile$child_clusters), 0L)

  parent <- get_cluster_profile(make_evidence_state(), cluster_id = 1L, level = 2L)
  expect_equal(parent$child_clusters$tag, c("cooking", "demographics"))
})

test_that("cluster profiles reject ambiguous or missing cluster identities", {
  state <- make_evidence_state()
  expect_error(get_cluster_profile(state, cluster_id = 1L), "Provide `level`")
  expect_error(get_cluster_profile(state, cluster_id = 99L, level = 1L), "Cluster not found")
})

test_that("evidence formatting removes markup and limits output", {
  evidence <- tibble::tibble(
    question_id = "q1",
    question_text = "<p>What &amp; why ${value}?</p>",
    score = 0.8764
  )
  formatted <- format_evidence_table(evidence)
  expect_match(formatted, "What & why \\{value\\}\\?")
  expect_match(formatted, "score: 0.876")
  expect_equal(format_evidence_table(evidence[0, ]), "- none")
})

