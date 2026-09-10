tagging_graph_fixture <- function() {
  base <- "https://example.org/graphs/tagging/"
  stats::setNames(paste0(base, c("run", "embedding", "hierarchy", "review")),
                  c("run", "embedding", "hierarchy", "review")) |>
    as.list()
}

test_that("novaRush repository validates its persistence configuration", {
  config <- list(branch = "main")
  expect_error(
    novarush_semantic_repository(config, list(run = "not-an-iri")),
    "name exactly"
  )
  graphs <- tagging_graph_fixture()
  graphs$review <- "relative"
  expect_error(
    novarush_semantic_repository(config, graphs), "absolute HTTP"
  )
  expect_error(
    novarush_semantic_repository(config, tagging_graph_fixture(),
                                 batch_size = 0L),
    "positive integer"
  )
  expect_error(
    novarush_semantic_repository(config, tagging_graph_fixture(),
                                 query_page_size = 0L),
    "query_page_size"
  )
  expect_error(
    novarush_semantic_repository(
      config, tagging_graph_fixture(), transaction_delay_seconds = -1
    ),
    "transaction_delay_seconds"
  )
})

test_that("semantic repository hydration reads bounded pages", {
  offsets <- integer()
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(query, graph, config, branch) {
      offsets <<- c(offsets, query$offset)
      values <- list(list("a"), list("b"), list("c"), list("d"), list("e"))
      from <- query$offset + 1L
      to <- min(query$offset + query$limit, length(values))
      if (from > length(values)) return(list())
      values[seq.int(from, to)]
    },
    .package = "novaTagger"
  )
  rows <- novaTagger:::.nr_query_pages(
    list(select = list("?value")), "https://example.org/graph",
    list(branch = "main"), "main", page_size = 2L
  )
  expect_length(rows, 5L)
  expect_equal(offsets, c(0L, 2L, 4L))
})

test_that("supporting partitions are batched and the run pointer is last", {
  calls <- list()
  pauses <- numeric()
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(...) list(),
    .nr_upsert_vectors = function(records, graph, vector_property, config,
                                  branch) {
      calls[[length(calls) + 1L]] <<- list("embedding", length(records), graph)
      list(status = "ok")
    },
    .nr_upsert_named_graph = function(document, graph, config, branch) {
      calls[[length(calls) + 1L]] <<- list("graph", length(document), graph)
      list(status = "ok")
    },
    .nr_pause = function(seconds) pauses <<- c(pauses, seconds),
    .package = "novaTagger"
  )
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  repository <- novarush_semantic_repository(
    list(branch = "candidate"), tagging_graph_fixture(), batch_size = 2L,
    transaction_delay_seconds = 1.5
  )
  scope <- records$run[[1L]][["@id"]]
  repository$save(records, scope, state$revision)

  kinds <- vapply(calls, `[[`, character(1), 1L)
  graphs <- vapply(calls, `[[`, character(1), 3L)
  expect_equal(sum(kinds == "embedding"), 2L)
  expect_equal(tail(graphs, 1L), tagging_graph_fixture()$run)
  expect_true(all(vapply(calls, function(call) call[[2L]] <= 2L, logical(1))))
  expect_equal(pauses, rep(1.5, length(calls) - 1L))
})

test_that("a failed supporting write never publishes the run pointer", {
  written_graphs <- character()
  graphs <- tagging_graph_fixture()
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(...) list(),
    .nr_upsert_vectors = function(...) list(status = "ok"),
    .nr_upsert_named_graph = function(document, graph, config, branch) {
      written_graphs <<- c(written_graphs, graph)
      if (identical(graph, graphs$hierarchy)) stop("simulated timeout")
      list(status = "ok")
    },
    .package = "novaTagger"
  )
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  repository <- novarush_semantic_repository(
    list(branch = "candidate"), graphs, batch_size = 2L
  )
  expect_error(
    repository$save(records, records$run[[1L]][["@id"]], state$revision),
    "simulated timeout"
  )
  expect_false(graphs$run %in% written_graphs)
})

test_that("embedding payloads exclude duplicated vectors and hydrate on load", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  node <- records$embedding[[1L]]
  envelope <- novaTagger:::.nr_envelope(
    node, records$run[[1L]][["@id"]], embedding = TRUE
  )
  payload <- envelope[[novaTagger:::.nr_payload_property]][["@value"]]
  expect_null(payload[[novaTagger:::.nr_vector_property]])
  expect_equal(envelope[[novaTagger:::.nr_vector_property]], c(1, 0))
  expect_equal(
    envelope[[novaTagger:::.nr_scope_property]][["@id"]],
    records$run[[1L]][["@id"]]
  )
})

test_that("repository rejects a stale revision before writing", {
  writes <- 0L
  run_node <- tag_state_to_semantic_records(
    semantic_state_fixture(),
    question_base_iri = "https://example.org/question/"
  )$run[[1L]]
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(...) list(list(10L)),
    .nr_upsert_vectors = function(...) { writes <<- writes + 1L },
    .nr_upsert_named_graph = function(...) { writes <<- writes + 1L },
    .package = "novaTagger"
  )
  repository <- novarush_semantic_repository(
    list(branch = "main"), tagging_graph_fixture()
  )
  records <- list(run = list(run_node), embedding = list(),
                  hierarchy = list(), review = list())
  expect_error(
    repository$save(records, run_node[["@id"]], revision = 12L),
    "stale tagging state"
  )
  expect_equal(writes, 0L)
})

test_that("hydrated embeddings are not written again after interruption", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  vector_writes <- 0L
  loaded <- records
  testthat::local_mocked_bindings(
    .nr_load_records = function(...) loaded,
    .nr_query_run_revision = function(...) state$revision,
    .nr_upsert_vectors = function(records, ...) {
      vector_writes <<- vector_writes + length(records)
    },
    .nr_upsert_named_graph = function(...) list(status = "ok"),
    .package = "novaTagger"
  )
  repository <- novarush_semantic_repository(
    list(branch = "main"), tagging_graph_fixture()
  )
  scope <- records$run[[1L]][["@id"]]
  expect_true(repository$exists(scope))
  next_records <- records
  next_records$run[[1L]][["https://schema.org/version"]] <- state$revision + 1L
  repository$save(next_records, scope, revision = state$revision + 1L)
  expect_equal(vector_writes, 0L)
})

test_that("unchanged supporting records are not rewritten", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  graph_writes <- character()
  stored_revision <- NULL
  testthat::local_mocked_bindings(
    .nr_query_run_revision = function(...) stored_revision,
    .nr_upsert_vectors = function(...) list(status = "ok"),
    .nr_upsert_named_graph = function(document, graph, ...) {
      graph_writes <<- c(graph_writes, graph)
      if (identical(graph, tagging_graph_fixture()$run)) {
        stored_revision <<- as.integer(
          document[[1L]][["https://schema.org/version"]]
        )
      }
      list(status = "ok")
    },
    .package = "novaTagger"
  )
  repository <- novarush_semantic_repository(
    list(branch = "main"), tagging_graph_fixture(), batch_size = 100L
  )
  scope <- records$run[[1L]][["@id"]]
  repository$save(records, scope, state$revision)

  graph_writes <- character()
  next_records <- records
  next_records$run[[1L]][["https://schema.org/version"]] <-
    state$revision + 1L
  repository$save(next_records, scope, state$revision + 1L)

  expect_equal(graph_writes, tagging_graph_fixture()$run)
})

test_that("only new or changed supporting records are written", {
  state <- semantic_state_fixture()
  records <- tag_state_to_semantic_records(
    state, question_base_iri = "https://example.org/question/"
  )
  calls <- list()
  stored_revision <- NULL
  testthat::local_mocked_bindings(
    .nr_query_run_revision = function(...) stored_revision,
    .nr_upsert_vectors = function(...) list(status = "ok"),
    .nr_upsert_named_graph = function(document, graph, ...) {
      calls[[length(calls) + 1L]] <<- list(graph = graph, nodes = document)
      if (identical(graph, tagging_graph_fixture()$run)) {
        stored_revision <<- as.integer(
          document[[1L]][["https://schema.org/version"]]
        )
      }
      list(status = "ok")
    },
    .package = "novaTagger"
  )
  repository <- novarush_semantic_repository(
    list(branch = "main"), tagging_graph_fixture(), batch_size = 100L
  )
  scope <- records$run[[1L]][["@id"]]
  repository$save(records, scope, state$revision)

  calls <- list()
  next_records <- records
  next_records$run[[1L]][["https://schema.org/version"]] <-
    state$revision + 1L
  next_records$hierarchy[[2L]][["http://www.w3.org/2000/01/rdf-schema#label"]] <-
    "reviewed label"
  next_records$review[[length(next_records$review) + 1L]] <- list(
    "@id" = "https://example.org/review/new",
    "@type" = "https://data.nova.org/vocabulary/tagging/ReviewEvent"
  )
  repository$save(next_records, scope, state$revision + 1L)

  written_graphs <- vapply(calls, `[[`, character(1), "graph")
  expect_equal(sum(written_graphs == tagging_graph_fixture()$hierarchy), 1L)
  expect_equal(sum(written_graphs == tagging_graph_fixture()$review), 1L)
  expect_equal(sum(written_graphs == tagging_graph_fixture()$run), 1L)
  hierarchy_call <- calls[[which(
    written_graphs == tagging_graph_fixture()$hierarchy
  )]]
  review_call <- calls[[which(written_graphs == tagging_graph_fixture()$review)]]
  expect_length(hierarchy_call$nodes, 1L)
  expect_length(review_call$nodes, 1L)
})

test_that("a not-yet-created named graph is an empty partition", {
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(...) {
      stop("Internal error: Unknown named graph '#https://example.org/new'")
    },
    .package = "novaTagger"
  )
  expect_equal(
    novaTagger:::.nr_query_payloads(
      "https://example.org/run", "https://example.org/new",
      list(branch = "main"), "main"
    ),
    list()
  )
})

test_that("other named-graph query failures remain visible", {
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(...) stop("authentication failed"),
    .package = "novaTagger"
  )
  expect_error(
    novaTagger:::.nr_query_payloads(
      "https://example.org/run", "https://example.org/graph",
      list(branch = "main"), "main"
    ),
    "authentication failed"
  )
})
