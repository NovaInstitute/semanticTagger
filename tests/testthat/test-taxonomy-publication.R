taxonomy_fixture <- function() {
  new_reviewed_taxonomy(
    nodes = tibble::tibble(
      id = c("root", "health", "mobility", "wellbeing"),
      parent_id = c(NA_character_, "root", "health", "health"),
      label = c("Survey questions", "Health", "Mobility", "Wellbeing")
    ),
    assignments = tibble::tibble(
      question_id = c("https://example.org/q/1", "https://example.org/q/2"),
      leaf_id = c("mobility", "wellbeing")
    ),
    run_id = "review-run",
    revision = 3L,
    progress = list(reviewed_splits = "health")
  )
}

test_that("reviewed taxonomy normalizes and checksums current state", {
  taxonomy <- taxonomy_fixture()
  expect_s3_class(taxonomy, "reviewed_taxonomy")
  expect_equal(taxonomy$nodes$id,
               sort(c("root", "health", "mobility", "wellbeing")))
  expect_length(taxonomy$taxonomy_checksum, 1L)
  expect_length(taxonomy$assignment_checksum, 1L)
  expect_equal(taxonomy$progress$reviewed_splits, "health")
})

test_that("reviewed taxonomy omits unassigned questions", {
  taxonomy <- new_reviewed_taxonomy(
    nodes = data.frame(
      id = c("root", "leaf"), parent_id = c(NA, "root"),
      label = c("Root", "Leaf")
    ),
    assignments = data.frame(
      question_id = c("q1", "q2"), leaf_id = c("leaf", NA)
    ),
    run_id = "run"
  )
  expect_equal(taxonomy$assignments$question_id, "q1")
})

test_that("reviewed taxonomy rejects structural corruption", {
  base_nodes <- data.frame(
    id = c("root", "a", "b"), parent_id = c(NA, "root", "a"),
    label = c("Root", "A", "B")
  )
  expect_error(new_reviewed_taxonomy(
    transform(base_nodes, parent_id = c(NA, "b", "a")),
    data.frame(question_id = character(), leaf_id = character()), "run"
  ), "cycle")
  expect_error(new_reviewed_taxonomy(
    rbind(base_nodes, data.frame(id = "c", parent_id = "root", label = "a")),
    data.frame(question_id = character(), leaf_id = character()), "run"
  ), "Sibling")
  expect_error(new_reviewed_taxonomy(
    base_nodes, data.frame(question_id = "q1", leaf_id = "a"), "run"
  ), "leaf")
})

test_that("reviewed taxonomy records use explicit parents and direct assignments", {
  taxonomy <- taxonomy_fixture()
  records <- reviewed_taxonomy_records(
    taxonomy, base_iri = "https://example.org/tagging/"
  )
  expect_length(records$run, 1L)
  expect_length(records$hierarchy, 6L)
  types <- vapply(records$hierarchy, function(node) node[["@type"]], character(1))
  expect_equal(sum(types == "https://data.nova.org/vocabulary/tagging/QuestionCluster"), 4L)
  expect_equal(sum(types == "https://data.nova.org/vocabulary/tagging/TaggedQuestion"), 2L)
  mobility <- Filter(function(node) {
    identical(node[["https://schema.org/identifier"]], "mobility")
  }, records$hierarchy)[[1L]]
  expect_match(
    mobility[["https://data.nova.org/vocabulary/tagging/parentCluster"]][["@id"]],
    "/cluster/health$"
  )
  q1 <- Filter(function(node) identical(node[["@id"]], "https://example.org/q/1"),
               records$hierarchy)[[1L]]
  expect_match(
    q1[["https://data.nova.org/vocabulary/tagging/assignedLeaf"]][["@id"]],
    "/cluster/mobility$"
  )
  restored <- reviewed_taxonomy_from_records(records)
  expect_equal(restored$nodes, taxonomy$nodes)
  expect_equal(restored$assignments, taxonomy$assignments)
  expect_equal(restored$progress, taxonomy$progress)
  expect_equal(restored$taxonomy_checksum, taxonomy$taxonomy_checksum)
})

test_that("reviewed taxonomy diff summarizes publication changes", {
  before <- taxonomy_fixture()
  nodes <- before$nodes
  nodes$label[nodes$id == "health"] <- "Healthy lives"
  nodes <- rbind(nodes, data.frame(
    id = "new-leaf", parent_id = "health", label = "Access"
  ))
  assignments <- before$assignments
  assignments$leaf_id[assignments$question_id == "https://example.org/q/1"] <-
    "new-leaf"
  after <- new_reviewed_taxonomy(
    nodes, assignments, before$run_id, revision = before$revision + 1L
  )
  expect_equal(reviewed_taxonomy_diff(before, after), c(
    clusters_created = 1L,
    clusters_removed = 0L,
    clusters_renamed = 1L,
    assignments_added = 0L,
    assignments_removed = 0L,
    assignments_moved = 1L
  ))
})

test_that("publication records retain compact merge context", {
  taxonomy <- taxonomy_fixture()
  publication <- taxonomy_publication_record(
    taxonomy, publication_id = "publish-1", reviewer = "Alex",
    source_branch = "review-alex", base_main_head = "commit-1",
    changes = c(clusters_renamed = 1L), note = "First checkpoint",
    created_at = as.POSIXct("2026-10-01 08:00:00", tz = "UTC"),
    base_iri = "https://example.org/tagging/"
  )
  expect_equal(publication[["@type"]],
               "https://data.nova.org/vocabulary/tagging/TaxonomyPublication")
  expect_equal(publication[["https://data.nova.org/vocabulary/tagging/reviewer"]],
               "Alex")
  expect_equal(publication[["http://purl.org/dc/terms/description"]],
               "First checkpoint")
  expect_equal(publication[["https://data.nova.org/vocabulary/tagging/taxonomyChecksum"]],
               taxonomy$taxonomy_checksum)
})

test_that("reviewed taxonomy repository replaces hierarchy and publishes run last", {
  taxonomy <- taxonomy_fixture()
  publication <- taxonomy_publication_record(
    taxonomy, "publish-1", "Alex", "review-alex", "commit-1",
    base_iri = "https://example.org/tagging/"
  )
  graphs <- as.list(stats::setNames(
    paste0("https://example.org/graph/", c("run", "embedding", "hierarchy", "review")),
    c("run", "embedding", "hierarchy", "review")
  ))
  config <- novaRush::setConfig(
    baseUrl = "http://fluree.test", ledger = "demo", branch = "review-alex"
  )
  storage <- new.env(parent = emptyenv())
  storage$run <- list(list(
    "@id" = "https://example.org/tagging/run/review-run",
    "@type" = "https://data.nova.org/vocabulary/tagging/TaggingRun",
    "https://schema.org/version" = 2L
  ))
  storage$hierarchy <- list(list(
    "@id" = "https://example.org/old",
    "@type" = "https://data.nova.org/vocabulary/tagging/QuestionCluster"
  ))
  storage$review <- list()
  calls <- character()
  graph_name <- function(graph) names(graphs)[vapply(graphs, identical, logical(1), graph)]
  unpack <- function(records) lapply(records, function(node) {
    novaTagger:::.nr_payload_value(
      node[["https://data.nova.org/vocabulary/tagging/recordPayload"]]
    )
  })
  repository <- testthat::with_mocked_bindings(
    novarush_reviewed_taxonomy_repository(
      config, graphs, branch = "review-alex",
      base_iri = "https://example.org/tagging/", batch_size = 3L
    ),
    .nr_query_payloads = function(scope_iri, graph, config, branch, page_size) {
      storage[[graph_name(graph)]]
    },
    .nr_update_named_graph = function(delete = NULL, insert = NULL, graph,
                                      config, branch) {
      name <- graph_name(graph); calls <<- c(calls, paste0("delete-", name))
      ids <- vapply(unpack(delete), `[[`, character(1), "@id")
      storage[[name]] <- Filter(function(node) !node[["@id"]] %in% ids,
                                storage[[name]])
    },
    .nr_upsert_named_graph = function(document, graph, config, branch) {
      name <- graph_name(graph); calls <<- c(calls, paste0("insert-", name))
      for (node in unpack(document)) {
        storage[[name]] <- Filter(function(old) old[["@id"]] != node[["@id"]],
                                  storage[[name]])
        storage[[name]][[length(storage[[name]]) + 1L]] <- node
      }
    },
    .package = "novaTagger"
  )
  result <- testthat::with_mocked_bindings(
    repository$replace(taxonomy, publication),
    .nr_query_payloads = function(scope_iri, graph, config, branch, page_size) {
      storage[[graph_name(graph)]]
    },
    .nr_update_named_graph = function(delete = NULL, insert = NULL, graph,
                                      config, branch) {
      name <- graph_name(graph); calls <<- c(calls, paste0("delete-", name))
      ids <- vapply(unpack(delete), `[[`, character(1), "@id")
      storage[[name]] <- Filter(function(node) !node[["@id"]] %in% ids,
                                storage[[name]])
    },
    .nr_upsert_named_graph = function(document, graph, config, branch) {
      name <- graph_name(graph); calls <<- c(calls, paste0("insert-", name))
      for (node in unpack(document)) {
        storage[[name]] <- Filter(function(old) old[["@id"]] != node[["@id"]],
                                  storage[[name]])
        storage[[name]][[length(storage[[name]]) + 1L]] <- node
      }
    },
    .package = "novaTagger"
  )
  expect_true(result$verified)
  expect_equal(length(storage$hierarchy), 6L)
  expect_equal(length(storage$review), 1L)
  expect_lt(match("insert-hierarchy", calls), match("insert-run", calls))
  expect_equal(storage$run[[1]][["https://data.nova.org/vocabulary/tagging/taxonomyChecksum"]],
               taxonomy$taxonomy_checksum)
})
