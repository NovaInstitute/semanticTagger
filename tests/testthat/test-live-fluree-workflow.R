test_that("live Fluree workflow persists, clusters, and resumes end to end", {
  testthat::skip_if_not(
    live_tagger_enabled(),
    "Set FLUREE_LIVE_TEST=true to run against a live Fluree server."
  )
  stamp <- gsub("[^A-Za-z0-9]", "", format(
    Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"
  ))
  config <- novaRush::setConfig(
    baseUrl = Sys.getenv("FLUREE_BASE_URL", "http://localhost:8090"),
    ledger = Sys.getenv("FLUREE_TEST_LEDGER", "novatagger-integration"),
    branch = Sys.getenv("FLUREE_TEST_BRANCH", "main"),
    timeout = as.numeric(Sys.getenv("FLUREE_REQUEST_TIMEOUT", "60"))
  )
  novaRush::createLedger(config)
  base <- paste0("https://data.nova.org/test/workflow/", stamp, "/")
  survey_graph <- paste0(base, "graph/survey")
  procedure <- paste0(base, "procedure")
  form <- paste0(base, "form")
  question_text <- c("Age?", "Age band?", "Income?", "Income band?")
  question_ids <- paste0(base, "question/", seq_along(question_text))
  survey_nodes <- lapply(seq_along(question_text), function(i) list(
    "@id" = question_ids[[i]],
    "@type" = "https://w3id.org/survey-ontology#SingleInputQuestion",
    "https://w3id.org/survey-ontology#hasText" = question_text[[i]],
    "https://w3id.org/survey-ontology#inSurveyProcedure" =
      list("@id" = procedure),
    "http://www.w3.org/ns/prov#wasDerivedFrom" = list("@id" = form),
    "https://data.nova.org/vocabulary/survey/questionClass" = "open",
    "https://data.nova.org/vocabulary/survey/responseCardinality" = "single",
    "https://data.nova.org/vocabulary/survey/responseDatatype" = "text",
    "https://data.nova.org/vocabulary/survey/sourceElementName" =
      paste0("q", i),
    "https://data.nova.org/vocabulary/survey/elementOrder" = i
  ))
  novaRush::upsertNamedGraph(survey_nodes, survey_graph, config)
  tagging_graphs <- stats::setNames(
    paste0(base, "graph/tagging/", c("run", "embedding", "hierarchy", "review")),
    c("run", "embedding", "hierarchy", "review")
  ) |> as.list()
  run_id <- paste0("integration-", stamp)

  workflow <- fluree_tagging_workflow(
    config, survey_graph, tagging_graphs, run_id,
    page_size = 2L, batch_size = 2L
  )
  provider <- new_model_provider(
    name = "live-deterministic",
    embedding_model = "live-deterministic-v1",
    generation_model = "unused",
    embed_one = function(text, trace_callback = NULL) {
      if (grepl("Age", text)) c(1, 0) else c(0, 1)
    },
    embed_batch = function(texts, trace_callback = NULL) {
      lapply(texts, function(text) {
        if (grepl("Age", text)) c(1, 0) else c(0, 1)
      })
    },
    generate = function(...) "unused"
  )
  workflow <- workflow_embed_questions(workflow, provider, batch_size = 2L)
  workflow <- workflow_cluster_hierarchical(workflow, clusters_by_level = 2L)

  resumed <- fluree_tagging_workflow(
    config, survey_graph, tagging_graphs, run_id,
    page_size = 2L, batch_size = 2L
  )
  expect_equal(resumed$state$revision, workflow$state$revision)
  expect_equal(resumed$state$workflow$stage, "clustered")
  expect_equal(unname(resumed$state$embeddings),
               unname(workflow$state$embeddings))
  expect_equal(resumed$state$assignments, workflow$state$assignments)
})
