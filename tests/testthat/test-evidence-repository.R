test_that("evidence repository delegates without transport assumptions", {
  repository <- new_tagging_evidence_repository(
    similar_questions = function(request) request$exclude_question_ids,
    reviewed_precedents = function(request) request$embedding_model,
    approved_guidance = function(request) request$limit
  )
  request <- new_tagging_evidence_request(
    c(1, 0), limit = 4L, exclude_question_ids = c("q1", "q1"),
    embedding_model = "model-a"
  )
  evidence <- retrieve_tagging_evidence(repository, request)
  expect_equal(evidence$similar_questions, "q1")
  expect_equal(evidence$reviewed_precedents, "model-a")
  expect_equal(evidence$approved_guidance, 4L)
})

test_that("evidence requests reject invalid vectors and limits", {
  expect_error(new_tagging_evidence_request(numeric()), "non-empty")
  expect_error(new_tagging_evidence_request(c(1, NA_real_)), "finite")
  expect_error(new_tagging_evidence_request(1, limit = -1L), "non-negative")
})

test_that("evidence repository requires every callback", {
  expect_error(
    new_tagging_evidence_repository(identity, identity, NULL),
    "approved_guidance"
  )
})
