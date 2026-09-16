integration_question_fixture <- function() {
  tibble::tibble(
    id = c("https://example.org/question/age", "https://example.org/question/income"),
    iri = c("https://example.org/question/age", "https://example.org/question/income"),
    caption = c("What is your age?", "What is your income?")
  )
}

test_that("Fluree workflow starts a new run and checkpoints it", {
  repository <- memory_semantic_repository()
  testthat::local_mocked_bindings(
    .fluree_workflow_questions = function(...) integration_question_fixture(),
    .fluree_workflow_repository = function(...) repository,
    .package = "novaTagger"
  )
  workflow <- fluree_tagging_workflow(
    config = list(branch = "candidate"),
    survey_graph = "https://example.org/graphs/survey",
    tagging_graphs = tagging_graph_fixture(),
    run_id = "integration-001"
  )
  expect_s3_class(workflow, "tagging_workflow")
  expect_equal(workflow$state$run_id, "integration-001")
  expect_equal(workflow$state$workflow$stage, "questions_ready")
  expect_equal(workflow$state$revision, 1L)
  expect_true(length(repository$storage$partitions$run) == 1L)
})

test_that("Fluree workflow resumes the same authoritative run", {
  repository <- memory_semantic_repository()
  testthat::local_mocked_bindings(
    .fluree_workflow_questions = function(...) integration_question_fixture(),
    .fluree_workflow_repository = function(...) repository,
    .package = "novaTagger"
  )
  first <- fluree_tagging_workflow(
    list(branch = "main"), "https://example.org/graphs/survey",
    tagging_graph_fixture(), "integration-resume"
  )
  resumed <- fluree_tagging_workflow(
    list(branch = "main"), "https://example.org/graphs/survey",
    tagging_graph_fixture(), "integration-resume"
  )
  expect_equal(resumed$state$revision, first$state$revision)
  expect_equal(resumed$state$run_id, first$state$run_id)
  expect_equal(resumed$state$questions, first$state$questions)
})

test_that("Fluree workflow rejects empty selections and unstable run IDs", {
  testthat::local_mocked_bindings(
    .fluree_workflow_questions = function(...) integration_question_fixture()[0, ],
    .fluree_workflow_repository = function(...) memory_semantic_repository(),
    .package = "novaTagger"
  )
  expect_error(
    fluree_tagging_workflow(
      list(branch = "main"), "https://example.org/graphs/survey",
      tagging_graph_fixture(), "empty"
    ),
    "no taggable questions"
  )
  expect_error(
    fluree_tagging_workflow(
      list(branch = "main"), "https://example.org/graphs/survey",
      tagging_graph_fixture(), ""
    ),
    "stable identifier"
  )
})
