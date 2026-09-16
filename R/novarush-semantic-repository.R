# novaRush composition adapter for authoritative tagging state.

.nr_scope_property <- "https://data.nova.org/vocabulary/tagging/persistenceScope"
.nr_payload_property <- "https://data.nova.org/vocabulary/tagging/recordPayload"
.nr_vector_property <- "https://data.nova.org/vocabulary/tagging/vector"

.nr_upsert_named_graph <- function(document, graph, config, branch) {
  novaRush::upsertNamedGraph(document, graph, config = config, branch = branch)
}

.nr_upsert_vectors <- function(records, graph, vector_property, config, branch) {
  novaRush::upsertVectors(
    records, graph, vector_property, config = config, branch = branch
  )
}

.nr_query_named_graph <- function(query, graph, config, branch) {
  novaRush::queryNamedGraph(query, graph, config = config, branch = branch)
}

.nr_validate_graphs <- function(graphs) {
  required <- c("run", "embedding", "hierarchy", "review")
  if (!is.list(graphs) || !setequal(names(graphs), required)) {
    stop("`graphs` must name exactly: run, embedding, hierarchy, and review.",
         call. = FALSE)
  }
  invalid <- !vapply(graphs, function(graph) {
    is.character(graph) && length(graph) == 1L && !is.na(graph) &&
      grepl("^https?://[^[:space:]]+$", graph, ignore.case = TRUE)
  }, logical(1))
  if (any(invalid)) {
    stop("Every tagging graph must be one absolute HTTP(S) IRI.", call. = FALSE)
  }
  graphs[required]
}

.nr_batches <- function(records, batch_size) {
  if (!length(records)) return(list())
  split(records, ceiling(seq_along(records) / batch_size))
}

.nr_pause <- function(seconds) {
  if (seconds > 0) Sys.sleep(seconds)
  invisible(NULL)
}

.nr_payload_value <- function(value) {
  if (is.list(value) && "@value" %in% names(value)) value <- value[["@value"]]
  if (is.character(value) && length(value) == 1L &&
      jsonlite::validate(value)) {
    return(jsonlite::fromJSON(value, simplifyVector = FALSE))
  }
  value
}

.nr_result_rows <- function(result) {
  if (is.null(result) || !length(result)) return(list())
  if (is.data.frame(result)) {
    return(lapply(seq_len(nrow(result)), function(i) {
      unname(as.list(result[i, , drop = FALSE]))
    }))
  }
  if (!is.list(result)) return(list(list(result)))
  if (length(result) && !is.list(result[[1L]])) return(list(result))
  result
}

.nr_unknown_graph_error <- function(error) {
  grepl("Unknown named graph", conditionMessage(error), fixed = TRUE)
}

.nr_query_pages <- function(query, graph, config, branch, page_size) {
  offset <- 0L
  rows <- list()
  repeat {
    page_query <- query
    page_query$limit <- page_size
    page_query$offset <- offset
    page <- .nr_result_rows(.nr_query_named_graph(
      page_query, graph, config, branch
    ))
    rows <- c(rows, page)
    if (length(page) < page_size) break
    offset <- offset + page_size
  }
  rows
}

.nr_query_payloads <- function(scope_iri, graph, config, branch,
                               page_size = 100L) {
  rows <- tryCatch(
    .nr_query_pages(
      list(
        select = list("?payload"),
        where = list(list(
          "@id" = "?entity",
          "https://data.nova.org/vocabulary/tagging/persistenceScope" =
            list("@id" = scope_iri),
          "https://data.nova.org/vocabulary/tagging/recordPayload" = "?payload"
        )),
        orderBy = list("?entity")
      ),
      graph, config, branch, page_size
    ),
    error = function(error) {
      if (.nr_unknown_graph_error(error)) return(list())
      stop(error)
    }
  )
  unname(Filter(function(node) {
    is.list(node) && !is.null(node[["@id"]])
  }, lapply(rows, function(row) .nr_payload_value(row[[1L]]))))
}

.nr_query_vectors <- function(scope_iri, graph, config, branch,
                              page_size = 100L) {
  rows <- .nr_query_pages(
    list(
      select = list("?entity", "?vector"),
      where = list(list(
        "@id" = "?entity",
        "https://data.nova.org/vocabulary/tagging/persistenceScope" =
          list("@id" = scope_iri),
        "https://data.nova.org/vocabulary/tagging/vector" = "?vector"
      )),
      orderBy = list("?entity")
    ),
    graph, config, branch, page_size
  )
  stats::setNames(lapply(rows, function(row) {
    as.numeric(.nr_payload_value(row[[2L]]))
  }), vapply(rows, function(row) as.character(row[[1L]]), character(1)))
}

.nr_query_run_revision <- function(scope_iri, graph, config, branch) {
  result <- tryCatch(
    .nr_query_named_graph(
      list(
        select = list("?version"),
        where = list(list(
          "@id" = "?entity",
          "https://data.nova.org/vocabulary/tagging/persistenceScope" =
            list("@id" = scope_iri),
          "https://schema.org/version" = "?version"
        )),
        limit = 1L
      ),
      graph, config, branch
    ),
    error = function(error) {
      if (.nr_unknown_graph_error(error)) return(list())
      stop(error)
    }
  )
  rows <- .nr_result_rows(result)
  if (!length(rows)) return(NULL)
  as.integer(.nr_payload_value(rows[[1L]][[1L]]))
}

.nr_load_records <- function(scope_iri, graphs, config, branch, page_size) {
  records <- lapply(graphs, function(graph) {
    .nr_query_payloads(scope_iri, graph, config, branch, page_size)
  })
  if (length(records$embedding)) {
    vectors <- .nr_query_vectors(
      scope_iri, graphs$embedding, config, branch, page_size
    )
    records$embedding <- lapply(records$embedding, function(node) {
      vector <- vectors[[node[["@id"]]]]
      if (!is.null(vector)) node[[.nr_vector_property]] <- .tag_json(vector)
      node
    })
  }
  records
}

.nr_envelope <- function(node, scope_iri, embedding = FALSE) {
  payload <- node
  if (isTRUE(embedding)) payload[[.nr_vector_property]] <- NULL
  node[[.nr_scope_property]] <- .tag_ref(scope_iri)
  node[[.nr_payload_property]] <- .tag_json(payload)
  if (isTRUE(embedding)) {
    node[[.nr_vector_property]] <- as.numeric(.tag_json_value(
      node[[.nr_vector_property]]
    ))
  }
  node
}

.nr_merge_nodes <- function(existing, incoming) {
  for (node in incoming) {
    ids <- vapply(existing, function(value) value[["@id"]], character(1))
    hit <- match(node[["@id"]], ids)
    if (is.na(hit)) existing[[length(existing) + 1L]] <- node else
      existing[[hit]] <- node
  }
  existing
}

.nr_changed_nodes <- function(existing, incoming) {
  if (!length(incoming)) return(list())
  existing_ids <- vapply(
    existing, function(node) as.character(node[["@id"]]), character(1)
  )
  incoming_ids <- vapply(
    incoming, function(node) as.character(node[["@id"]]), character(1)
  )
  hits <- match(incoming_ids, existing_ids)
  changed <- is.na(hits)
  matched <- which(!changed)
  changed[matched] <- !vapply(matched, function(i) {
    identical(existing[[hits[[i]]]], incoming[[i]])
  }, logical(1))
  unname(incoming[changed])
}

#' Create a novaRush-backed semantic tagging repository
#'
#' This composition adapter maps novaTagger's transport-free semantic records
#' to four Fluree named graphs. Embeddings are stored using Fluree's native
#' vector datatype. Supporting partitions are written in bounded batches and
#' the mutable run pointer is published last, so an interrupted save cannot
#' expose an incomplete hierarchy as current.
#'
#' @param config A Fluree configuration created by [novaRush::setConfig()].
#' @param graphs Named list containing absolute graph IRIs for `run`,
#'   `embedding`, `hierarchy`, and `review`.
#' @param branch Fluree branch. Defaults to `config$branch`.
#' @param batch_size Maximum resources in each Fluree write.
#' @param query_page_size Maximum records returned by each hydration query.
#' @param transaction_delay_seconds Seconds to pause after every supporting
#'   Fluree transaction. This lets background indexing catch up during large
#'   embedding imports. Defaults to zero.
#'
#' @return Repository callbacks accepted by [semantic_tag_store()].
#' @export
#' @importFrom novaRush queryNamedGraph upsertNamedGraph upsertVectors
novarush_semantic_repository <- function(
    config, graphs, branch = config$branch, batch_size = 250L,
    query_page_size = 100L, transaction_delay_seconds = 0) {
  graphs <- .nr_validate_graphs(graphs)
  if (length(branch) != 1L || is.na(branch) || !nzchar(branch)) {
    stop("`branch` must be one non-empty branch name.", call. = FALSE)
  }
  batch_size <- suppressWarnings(as.integer(batch_size))
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
    stop("`batch_size` must be one positive integer.", call. = FALSE)
  }
  query_page_size <- suppressWarnings(as.integer(query_page_size))
  if (length(query_page_size) != 1L || is.na(query_page_size) ||
      query_page_size < 1L) {
    stop("`query_page_size` must be one positive integer.", call. = FALSE)
  }
  transaction_delay_seconds <- suppressWarnings(as.numeric(
    transaction_delay_seconds
  ))
  if (length(transaction_delay_seconds) != 1L ||
      !is.finite(transaction_delay_seconds) || transaction_delay_seconds < 0) {
    stop("`transaction_delay_seconds` must be non-negative numeric.",
         call. = FALSE)
  }
  cache <- new.env(parent = emptyenv())
  cache$scope <- NULL
  cache$records <- NULL

  load_remote <- function(scope_iri, refresh = FALSE) {
    if (!isTRUE(refresh) && identical(cache$scope, scope_iri) &&
        !is.null(cache$records)) return(cache$records)
    cache$scope <- scope_iri
    cache$records <- .nr_load_records(
      scope_iri, graphs, config = config, branch = branch,
      page_size = query_page_size
    )
    cache$records
  }

  cached_revision <- function(scope_iri) {
    if (!identical(cache$scope, scope_iri) || is.null(cache$records) ||
        !length(cache$records$run)) return(NULL)
    version <- cache$records$run[[1L]][["https://schema.org/version"]]
    if (is.list(version) && "@value" %in% names(version)) {
      version <- version[["@value"]]
    }
    version <- suppressWarnings(as.integer(version))
    if (length(version) != 1L || is.na(version)) return(NULL)
    version
  }

  check_revision <- function(scope_iri, revision) {
    # Fluree's query index can briefly lag a just-committed transaction. While
    # this repository remains alive, its cache is the authoritative view of
    # its own successful writes. A fresh repository still checks Fluree.
    stored <- cached_revision(scope_iri)
    if (is.null(stored)) {
      stored <- .nr_query_run_revision(
        scope_iri, graphs$run, config = config, branch = branch
      )
    }
    if (!is.null(stored)) {
      expected <- as.integer(revision) - 1L
      if (!identical(stored, expected)) {
        stop(
          "Cannot save stale tagging state: stored revision is ", stored,
          " but the preceding revision is ", expected, ".", call. = FALSE
        )
      }
    }
  }

  write_records <- function(records, scope_iri) {
    for (batch in .nr_batches(records$embedding, batch_size)) {
      prepared <- lapply(batch, .nr_envelope, scope_iri = scope_iri,
                         embedding = TRUE)
      .nr_upsert_vectors(prepared, graphs$embedding, .nr_vector_property,
                         config = config, branch = branch)
      .nr_pause(transaction_delay_seconds)
    }
    for (partition in c("hierarchy", "review")) {
      for (batch in .nr_batches(records[[partition]], batch_size)) {
        prepared <- lapply(batch, .nr_envelope, scope_iri = scope_iri)
        .nr_upsert_named_graph(prepared, graphs[[partition]],
                               config = config, branch = branch)
        .nr_pause(transaction_delay_seconds)
      }
    }
    prepared_run <- lapply(records$run, .nr_envelope, scope_iri = scope_iri)
    .nr_upsert_named_graph(prepared_run, graphs$run,
                           config = config, branch = branch)
  }

  list(
    exists = function(scope_iri) {
      length(load_remote(scope_iri)$run) > 0L
    },
    load = function(scope_iri) load_remote(scope_iri),
    save = function(records, scope_iri, revision) {
      check_revision(scope_iri, revision)

      current <- if (identical(cache$scope, scope_iri) &&
                     !is.null(cache$records)) cache$records else
        list(run = list(), embedding = list(), hierarchy = list(), review = list())
      new_embeddings <- .nr_changed_nodes(
        current$embedding %||% list(), records$embedding
      )

      changed_records <- list(embedding = new_embeddings)
      for (partition in c("hierarchy", "review")) {
        changed_records[[partition]] <- .nr_changed_nodes(
          current[[partition]] %||% list(), records[[partition]]
        )
      }
      changed_records$run <- records$run
      write_records(changed_records, scope_iri)

      cache$scope <- scope_iri
      cache$records <- lapply(names(records), function(partition) {
        .nr_merge_nodes(current[[partition]] %||% list(), records[[partition]])
      })
      names(cache$records) <- names(records)
      invisible(TRUE)
    },
    save_delta = function(records, scope_iri, revision) {
      check_revision(scope_iri, revision)
      write_records(records, scope_iri)
      current <- if (identical(cache$scope, scope_iri) &&
                     !is.null(cache$records)) cache$records else
        list(run = list(), embedding = list(), hierarchy = list(), review = list())
      cache$scope <- scope_iri
      cache$records <- lapply(names(current), function(partition) {
        .nr_merge_nodes(current[[partition]], records[[partition]] %||% list())
      })
      names(cache$records) <- names(current)
      invisible(TRUE)
    },
    metadata = list(
      backend = "novaRush", branch = branch, graphs = graphs,
      batch_size = batch_size, query_page_size = query_page_size,
      transaction_delay_seconds = transaction_delay_seconds
    )
  )
}
