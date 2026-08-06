workflow_fixture_questions <- function() {
  tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("What is your age?", "Which age band?",
                "What is your income?", "Which income band?")
  )
}

workflow_fixture_provider <- function() {
  new_model_provider(
    "fixture", "fixture-embed", "fixture-generate",
    embed_one = function(text, trace_callback = NULL) {
      if (grepl("age", text, ignore.case = TRUE)) c(1, 0) else c(0, 1)
    },
    embed_batch = function(texts, trace_callback = NULL) {
      lapply(texts, function(text) {
        if (grepl("age", text, ignore.case = TRUE)) c(1, 0) else c(0, 1)
      })
    },
    generate = function(prompt, max_output_tokens, temperature, format,
                        trace_callback = NULL) {
      if (grepl("age", prompt, ignore.case = TRUE)) {
        '{"tag":"age","confidence":0.95,"rationale":"age items","needs_review":false}'
      } else {
        '{"tag":"income","confidence":0.94,"rationale":"income items","needs_review":false}'
      }
    }
  )
}

test_that("workflow checkpoints every stage and resumes from its store", {
  store <- memory_tag_store()
  provider <- workflow_fixture_provider()
  workflow <- new_tagging_workflow(
    workflow_fixture_questions(), store, run_id = "workflow-resume"
  )
  expect_equal(workflow$state$workflow$stage, "questions_ready")
  expect_equal(workflow$state$revision, 1L)

  workflow <- workflow_embed_questions(workflow, provider, batch_size = 2L)
  expect_equal(workflow$state$workflow$stage, "embedded")
  expect_true(is.matrix(workflow$state$embeddings))
  expect_equal(workflow$state$revision, 4L)

  workflow <- workflow_cluster_hierarchical(workflow, 2L)
  resumed <- resume_tagging_workflow(store)
  expect_equal(resumed$state$workflow$stage, "clustered")
  expect_equal(resumed$state$workflow$clustering_method, "hierarchical_ward_d2")
  expect_equal(resumed$state$clusters, workflow$state$clusters)
})

test_that("embedding resumes after a provider fails between batches", {
  store <- memory_tag_store()
  calls <- 0L
  unstable <- new_model_provider(
    "unstable", "embed", "generate",
    embed_one = function(text, trace_callback = NULL) c(1, 0),
    embed_batch = function(texts, trace_callback = NULL) {
      calls <<- calls + 1L
      if (calls == 2L) stop("temporary provider failure")
      lapply(texts, function(text) c(1, 0))
    },
    generate = function(...) "unused"
  )
  workflow <- new_tagging_workflow(
    workflow_fixture_questions(), store, run_id = "workflow-interruption"
  )
  expect_error(
    workflow_embed_questions(workflow, unstable, batch_size = 2L),
    "temporary provider failure"
  )

  resumed <- resume_tagging_workflow(store)
  expect_equal(resumed$state$workflow$stage, "embedding")
  expect_equal(sum(!vapply(resumed$state$embeddings, is.null, logical(1))), 2L)

  stable <- workflow_fixture_provider()
  completed <- workflow_embed_questions(resumed, stable, batch_size = 2L)
  expect_equal(completed$state$workflow$stage, "embedded")
  expect_true(is.matrix(completed$state$embeddings))
})

test_that("proposal and review flow remains provider and persistence neutral", {
  store <- memory_tag_store()
  provider <- workflow_fixture_provider()
  workflow <- new_tagging_workflow(
    workflow_fixture_questions(), store, run_id = "workflow-review"
  )
  workflow <- workflow_embed_questions(workflow, provider, batch_size = 4L)
  workflow <- workflow_cluster_hierarchical(workflow, 2L)

  evidence <- tibble::tibble(
    question_id = "known-age", question_text = "Respondent age", score = 0.91
  )
  workflow <- workflow_propose_next(workflow, provider, evidence = evidence)
  proposal_id <- names(workflow$state$proposals)[[1]]
  proposal <- workflow$state$proposals[[proposal_id]]
  expect_equal(proposal$status, "proposed")
  expect_equal(proposal$evidence$questions, evidence)
  expect_equal(workflow$state$workflow$stage, "review")

  workflow <- workflow_review_proposal(
    workflow, proposal_id, "accepted", "reviewer-1"
  )
  expect_equal(workflow$state$proposals[[proposal_id]]$status, "accepted")
  expect_equal(length(workflow$state$review_events), 1L)
  expect_equal(workflow$state$workflow$stage, "tagging")

  workflow <- workflow_propose_next(workflow, provider)
  second_id <- setdiff(names(workflow$state$proposals), proposal_id)
  workflow <- workflow_review_proposal(
    workflow, second_id, "accepted", "reviewer-1"
  )
  expect_equal(workflow$state$workflow$stage, "complete")
  expect_equal(resume_tagging_workflow(store)$state$workflow$stage, "complete")
})

test_that("edited workflow decisions recompute and persist similarities", {
  provider <- workflow_fixture_provider()
  workflow <- new_tagging_workflow(
    workflow_fixture_questions(), memory_tag_store(), run_id = "workflow-edit"
  )
  workflow <- workflow_embed_questions(workflow, provider)
  workflow <- workflow_cluster_hierarchical(workflow, 2L)
  workflow <- workflow_propose_next(workflow, provider)
  proposal_id <- names(workflow$state$proposals)[[1]]

  workflow <- workflow_review_proposal(
    workflow, proposal_id, "edited", "reviewer-1",
    rationale = "More precise", tag = "respondent age", provider = provider
  )
  event <- workflow$state$review_events[[1]]
  expect_equal(event$resulting_tag, "respondent age")
  expect_true(is.list(event$similarity))
  expect_true(is.data.frame(event$similarity$questions))
  expect_equal(
    resume_tagging_workflow(workflow$store)$state$review_events[[1]]$resulting_tag,
    "respondent age"
  )
})

test_that("workflow rejects malformed normalized question inputs", {
  expect_error(
    new_tagging_workflow(tibble::tibble(id = "q1")),
    "require `id` and `caption`"
  )
  expect_error(
    new_tagging_workflow(tibble::tibble(id = c("q1", "q1"), caption = c("a", "b"))),
    "must be unique"
  )
})

test_that("BERTopic workflow strategy checkpoints the inferred hierarchy", {
  provider <- workflow_fixture_provider()
  workflow <- new_tagging_workflow(
    workflow_fixture_questions(), memory_tag_store(), run_id = "workflow-bertopic"
  )
  workflow <- workflow_embed_questions(workflow, provider)
  testthat::with_mocked_bindings(
    {
      clustered <- workflow_cluster_bertopic(workflow)
      expect_equal(clustered$state$workflow$stage, "clustered")
      expect_equal(clustered$state$workflow$clustering_method, "bertopic")
      expect_equal(clustered$state$clusters_by_level, 2L)
      expect_equal(resume_tagging_workflow(clustered$store)$state$clusters,
                   clustered$state$clusters)
    },
    fit_bertopic_topics = function(documents, embeddings, bertopic_kwargs) {
      list(topic_ids = c(0L, 0L, 1L, 1L))
    },
    .package = "novaTagger"
  )
})
