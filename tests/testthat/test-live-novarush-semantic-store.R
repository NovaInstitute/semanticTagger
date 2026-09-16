test_that("live novaRush semantic store saves and resumes tagging state", {
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
    apiKey = if (nzchar(Sys.getenv("FLUREE_API_TOKEN"))) {
      Sys.getenv("FLUREE_API_TOKEN")
    } else NULL,
    timeout = as.numeric(Sys.getenv("FLUREE_REQUEST_TIMEOUT", "60"))
  )
  novaRush::createLedger(config)
  graph_base <- paste0("https://data.nova.org/test/tagger/", stamp, "/")
  graphs <- stats::setNames(
    paste0(graph_base, c("run", "embedding", "hierarchy", "review")),
    c("run", "embedding", "hierarchy", "review")
  ) |> as.list()
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    iri = paste0("https://data.nova.org/test/question/", stamp, "/", 1:4),
    caption = c("Age?", "Age band?", "Income?", "Income band?")
  )
  run_id <- paste0("live-", stamp)
  repository <- novarush_semantic_repository(config, graphs, batch_size = 2L)
  store <- semantic_tag_store(repository, questions, run_id)
  workflow <- new_tagging_workflow(questions, store, run_id = run_id)

  workflow$state$embeddings <- rbind(c(1, 0), c(.9, .1), c(0, 1), c(.1, .9))
  fit <- cluster_embeddings(workflow$state$embeddings, 2L)
  workflow$state$assignments <- add_cluster_assignments(
    questions, fit$hclust, 2L
  )
  workflow$state$clusters <- build_cluster_index(
    workflow$state$assignments, 2L
  )
  workflow$state$clusters_by_level <- 2L
  workflow$state$workflow$stage <- "clustered"
  workflow$state$workflow$embedding_model <- "live-fixture-v1"
  workflow$state$workflow$hierarchy_version <- 1L
  workflow$state <- tag_store_save(store, workflow$state)

  fresh_repository <- novarush_semantic_repository(config, graphs, batch_size = 2L)
  resumed <- resume_tagging_workflow(
    semantic_tag_store(fresh_repository, questions, run_id)
  )
  expect_equal(resumed$state$revision, workflow$state$revision)
  expect_equal(unname(resumed$state$embeddings),
               unname(workflow$state$embeddings), tolerance = 1e-6)
  expect_equal(resumed$state$assignments, workflow$state$assignments)
  expect_equal(resumed$state$workflow$stage, "clustered")
})
