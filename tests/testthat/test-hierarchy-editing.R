test_that("pruning removes matching assignments and rebuilds tag matrix", {
  state <- structure(list(
    assignments = tibble::tibble(
      id = c("q1", "q2", "q3"),
      caption = c("Age", "Income", "Education"),
      cluster_level_1 = c(1L, 2L, 2L),
      cluster_level_2 = c(1L, 1L, 1L)
    ),
    clusters = tibble::tibble(
      level = c(1L, 1L, 2L),
      cluster_id = c(1L, 2L, 1L),
      parent_cluster = c(1L, 1L, NA_integer_),
      question_ids = list("q1", c("q2", "q3"), c("q1", "q2", "q3")),
      tag = c("age", "income", "demographics")
    )
  ), class = "tag_state")

  out <- prune_tag_from_state(state, tag = "income")

  expect_true(all(is.na(
    out$assignments$cluster_level_1[out$assignments$id %in% c("q2", "q3")]
  )))
  expect_true(all(c("tag_level_1", "tag_level_2") %in% names(out$tag_matrix)))
})

test_that("hierarchy integrity identifies orphaned assignments", {
  state <- structure(list(
    assignments = tibble::tibble(
      id = c("q1", "q2"),
      caption = c("Age", "Income"),
      cluster_level_1 = c(1L, 2L),
      cluster_level_2 = c(1L, NA_integer_)
    ),
    clusters = tibble::tibble(
      level = c(1L, 1L, 2L),
      cluster_id = c(1L, 2L, 1L),
      parent_cluster = c(1L, NA_integer_, NA_integer_),
      question_ids = list("q1", "q2", "q1"),
      tag = c("age", "income", "demo")
    )
  ), class = "tag_state")

  info <- validate_hierarchy_integrity(state)

  expect_equal(info$n_questions, 2L)
  expect_equal(info$n_levels, 2L)
  expect_equal(info$orphan_counts$orphan_questions[[1]], 1L)
})
