test_that("infer_topic_levels returns a descending hierarchy", {
  expect_equal(infer_topic_levels(1), 1L)
  expect_equal(infer_topic_levels(2), 2L)
  expect_equal(infer_topic_levels(5), c(5L, 3L, 2L))
  expect_equal(infer_topic_levels(8), c(8L, 4L, 2L))
})

test_that("BERTopic level inference ignores outliers by default", {
  questions <- tibble::tibble(
    id = paste0("q", 1:5),
    caption = paste("Question", 1:5)
  )
  embeddings <- matrix(rnorm(10), ncol = 2)
  testthat::with_mocked_bindings(
    {
      expect_equal(
        infer_bertopic_clusters_by_level(questions, embeddings),
        c(3L, 2L)
      )
      expect_equal(
        infer_bertopic_clusters_by_level(
          questions, embeddings, include_outlier_topic = TRUE
        ),
        c(4L, 2L)
      )
    },
    fit_bertopic_topics = function(...) {
      list(topic_ids = c(-1L, 1L, 1L, 2L, 3L))
    },
    .package = "novaTagger"
  )
})

test_that("BERTopic hierarchy builds contiguous leaf assignments", {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("A", "B", "C", "D"),
    topic_id = c(20L, 20L, 7L, 7L)
  )
  embeddings <- matrix(
    c(1, 0, 1, .1, 0, 1, .1, 1),
    byrow = TRUE,
    ncol = 2
  )

  out <- build_bertopic_hierarchy(
    questions, embeddings, topic_levels = c(2L, 2L)
  )

  expect_equal(out$clusters_by_level, c(2L, 2L))
  expect_setequal(unique(out$assignments$cluster_level_1), c(1L, 2L))
  expect_true(all(
    c("cluster_level_1", "cluster_level_2") %in% names(out$assignments)
  ))
  expect_true(all(
    c("level", "cluster_id", "parent_cluster", "question_ids", "tag") %in%
      names(out$clusters)
  ))
  expect_length(out$questions$embedding, 4L)
})

test_that("BERTopic hierarchy derives parent levels when omitted", {
  questions <- tibble::tibble(
    id = paste0("q", 1:6),
    caption = paste("Question", 1:6),
    topic_id = rep(c(10L, 20L, 30L), each = 2L)
  )
  embeddings <- matrix(
    c(
      1, 0, .9, .1,
      0, 1, .1, .9,
      -.8, -.8, -.9, -.7
    ),
    byrow = TRUE,
    ncol = 2
  )

  out <- build_bertopic_hierarchy(questions, embeddings)

  expect_equal(out$clusters_by_level, c(3L, 2L))
  expect_true("cluster_level_2" %in% names(out$assignments))
})
