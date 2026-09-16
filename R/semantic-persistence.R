# Transport-free semantic projections for tagging knowledge.

.tag_vocab <- "https://data.nova.org/vocabulary/tagging/"
.tag_xsd <- "http://www.w3.org/2001/XMLSchema#"

.tag_term <- function(term) paste0(.tag_vocab, term)

.tag_absolute_base <- function(value, argument = "base_iri") {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) ||
      !grepl("^https?://", value, ignore.case = TRUE)) {
    stop("`", argument, "` must be one absolute HTTP(S) IRI.", call. = FALSE)
  }
  paste0(sub("/+$", "", value), "/")
}

.tag_iri_token <- function(value) {
  utils::URLencode(as.character(value), reserved = TRUE)
}

.tag_ref <- function(iri) list("@id" = iri)

.tag_time <- function(value) {
  format(as.POSIXct(value), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

.tag_parse_time <- function(value) {
  as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

.tag_json <- function(value) list("@value" = value, "@type" = "@json")

.tag_json_value <- function(value, default = NULL) {
  if (is.null(value)) return(default)
  if (is.list(value) && "@value" %in% names(value)) return(value[["@value"]])
  value
}

.tag_node_type <- function(node) as.character(node[["@type"]] %||% "")

.tag_nodes_of_type <- function(records, type) {
  nodes <- if (length(records) && all(vapply(records, function(value) {
    is.list(value) && !is.null(value[["@id"]])
  }, logical(1)))) records else unlist(records, recursive = FALSE)
  unname(Filter(function(node) {
    identical(.tag_node_type(node), .tag_term(type))
  }, nodes))
}

#' Construct a stable tagging-domain IRI
#'
#' @param kind Entity kind such as `run`, `hierarchy`, or `proposal`.
#' @param ... Stable identifier components.
#' @param base_iri Base IRI owned by the deployment.
#' @return Absolute IRI.
#' @export
tagging_entity_iri <- function(kind, ...,
                               base_iri = "https://data.nova.org/tagger/") {
  base_iri <- .tag_absolute_base(base_iri)
  components <- c(as.character(kind), unlist(list(...), use.names = FALSE))
  if (!length(components) || anyNA(components) || any(!nzchar(components))) {
    stop("IRI components must be non-empty.", call. = FALSE)
  }
  paste0(base_iri, paste(vapply(components, .tag_iri_token, character(1)),
                         collapse = "/"))
}

.tag_question_iris <- function(state, question_base_iri, base_iri) {
  if ("iri" %in% names(state$questions)) {
    iris <- as.character(state$questions$iri)
  } else {
    question_base_iri <- .tag_absolute_base(
      question_base_iri %||% paste0(.tag_absolute_base(base_iri), "question/"),
      "question_base_iri"
    )
    iris <- paste0(question_base_iri,
                   vapply(state$questions$id, .tag_iri_token, character(1)))
  }
  if (anyNA(iris) || any(!grepl("^https?://", iris, ignore.case = TRUE))) {
    stop("Question IRIs must be absolute HTTP(S) IRIs.", call. = FALSE)
  }
  stats::setNames(iris, as.character(state$questions$id))
}

.tag_embedding_rows <- function(state) {
  embeddings <- state$embeddings
  if (is.null(embeddings)) return(list())
  if (is.matrix(embeddings)) {
    return(lapply(seq_len(nrow(embeddings)), function(i) embeddings[i, ]))
  }
  embeddings
}

#' Project tagging state into partitioned semantic records
#'
#' This function performs no database or named-graph operations. The returned
#' partitions describe the intended graph roles for a persistence adapter.
#'
#' @param state A `tag_state`.
#' @param base_iri Base IRI for tagging-domain entities.
#' @param question_base_iri Base IRI used for question references when the
#'   question table has no `iri` column. Production callers should use the same
#'   base as `novaGraphDB`.
#' @return A `tag_semantic_records` list with `run`, `embedding`, `hierarchy`,
#'   and `review` partitions.
#' @export
tag_state_to_semantic_records <- function(
    state, base_iri = "https://data.nova.org/tagger/",
    question_base_iri = NULL) {
  state <- validate_tag_state(state)
  base_iri <- .tag_absolute_base(base_iri)
  question_iris <- .tag_question_iris(state, question_base_iri, base_iri)
  run_iri <- tagging_entity_iri("run", state$run_id, base_iri = base_iri)
  has_hierarchy <- is.data.frame(state$clusters) && nrow(state$clusters) > 0L
  hierarchy_version <- as.integer(
    state$workflow$hierarchy_version %||% state$revision
  )
  hierarchy_iri <- tagging_entity_iri(
    "hierarchy", state$run_id, hierarchy_version, base_iri = base_iri
  )
  cluster_iri <- function(level, cluster_id) tagging_entity_iri(
    "hierarchy", state$run_id, hierarchy_version, "cluster", level, cluster_id,
    base_iri = base_iri
  )

  run_node <- list(
    "@id" = run_iri,
    "@type" = .tag_term("TaggingRun"),
    "https://schema.org/identifier" = state$run_id,
    "https://schema.org/status" = state$status,
    "https://schema.org/version" = as.integer(state$revision),
    "http://purl.org/dc/terms/created" = .tag_time(state$created_at),
    "http://purl.org/dc/terms/modified" = .tag_time(state$updated_at),
    "https://data.nova.org/vocabulary/tagging/workflow" =
      .tag_json(state$workflow %||% list())
  )
  if (has_hierarchy) {
    run_node[["https://www.w3.org/ns/prov#hadRevision"]] <- .tag_ref(hierarchy_iri)
  }

  embedding_model <- as.character(
    state$workflow$embedding_model %||% NA_character_
  )
  embeddings <- .tag_embedding_rows(state)
  embedding_nodes <- unlist(lapply(seq_along(embeddings), function(i) {
    vector <- embeddings[[i]]
    if (is.null(vector)) return(list())
    question_id <- as.character(state$questions$id[[i]])
    list(list(
      "@id" = tagging_entity_iri(
        "embedding", state$run_id, question_id,
        ifelse(is.na(embedding_model), "unknown-model", embedding_model),
        base_iri = base_iri
      ),
      "@type" = .tag_term("QuestionEmbedding"),
      "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
      "https://data.nova.org/vocabulary/tagging/question" =
        .tag_ref(question_iris[[question_id]]),
      "https://schema.org/identifier" = question_id,
      "https://data.nova.org/vocabulary/tagging/model" = embedding_model,
      "https://data.nova.org/vocabulary/tagging/dimension" = length(vector),
      "https://data.nova.org/vocabulary/tagging/vector" =
        .tag_json(as.numeric(vector))
    ))
  }), recursive = FALSE)

  hierarchy_node <- if (has_hierarchy) list(
    "@id" = hierarchy_iri,
    "@type" = .tag_term("HierarchyVersion"),
    "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
    "https://schema.org/version" = hierarchy_version,
    "https://data.nova.org/vocabulary/tagging/method" =
      as.character(state$workflow$clustering_method %||% NA_character_),
    "https://data.nova.org/vocabulary/tagging/clustersByLevel" =
      .tag_json(as.integer(state$clusters_by_level %||% integer()))
  ) else NULL

  cluster_nodes <- list()
  membership_nodes <- list()
  if (has_hierarchy) {
    cluster_nodes <- lapply(seq_len(nrow(state$clusters)), function(i) {
      row <- state$clusters[i, , drop = FALSE]
      node <- list(
        "@id" = cluster_iri(row$level[[1]], row$cluster_id[[1]]),
        "@type" = .tag_term("QuestionCluster"),
        "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
        "https://data.nova.org/vocabulary/tagging/inHierarchy" =
          .tag_ref(hierarchy_iri),
        "https://data.nova.org/vocabulary/tagging/level" =
          as.integer(row$level[[1]]),
        "https://schema.org/identifier" = as.character(row$cluster_id[[1]])
      )
      if (!is.na(row$tag[[1]]) && nzchar(row$tag[[1]])) {
        node[["http://www.w3.org/2000/01/rdf-schema#label"]] <- row$tag[[1]]
      }
      if ("parent_cluster" %in% names(row) && !is.na(row$parent_cluster[[1]])) {
        node[["https://data.nova.org/vocabulary/tagging/parentCluster"]] <-
          .tag_ref(cluster_iri(row$level[[1]] + 1L, row$parent_cluster[[1]]))
      }
      node
    })
    if (is.data.frame(state$assignments) && "cluster_level_1" %in% names(state$assignments)) {
      membership_nodes <- lapply(seq_len(nrow(state$assignments)), function(i) {
        question_id <- as.character(state$assignments$id[[i]])
        leaf_id <- state$assignments$cluster_level_1[[i]]
        list(
          "@id" = tagging_entity_iri(
            "hierarchy", state$run_id, hierarchy_version,
            "membership", question_id,
            base_iri = base_iri
          ),
          "@type" = .tag_term("LeafClusterMembership"),
          "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
          "https://data.nova.org/vocabulary/tagging/inHierarchy" =
            .tag_ref(hierarchy_iri),
          "https://data.nova.org/vocabulary/tagging/question" =
            .tag_ref(question_iris[[question_id]]),
          "https://schema.org/identifier" = question_id,
          "https://data.nova.org/vocabulary/tagging/cluster" =
            .tag_ref(cluster_iri(1L, leaf_id))
        )
      })
    }
  }

  proposal_nodes <- unname(lapply(state$proposals, function(proposal) {
    proposal_iri <- tagging_entity_iri(
      "proposal", proposal$proposal_id, base_iri = base_iri
    )
    node <- list(
      "@id" = proposal_iri,
      "@type" = .tag_term("TagProposal"),
      "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
      "https://schema.org/identifier" = proposal$proposal_id,
      "https://data.nova.org/vocabulary/tagging/cluster" =
        .tag_ref(cluster_iri(proposal$level, proposal$cluster_id)),
      "https://data.nova.org/vocabulary/tagging/level" = as.integer(proposal$level),
      "https://data.nova.org/vocabulary/tagging/clusterId" =
        as.character(proposal$cluster_id),
      "http://www.w3.org/2000/01/rdf-schema#label" = proposal$tag,
      "https://schema.org/status" = proposal$status,
      "https://data.nova.org/vocabulary/tagging/confidence" = proposal$confidence,
      "https://data.nova.org/vocabulary/tagging/rationale" = proposal$rationale,
      "https://data.nova.org/vocabulary/tagging/needsReview" =
        isTRUE(proposal$needs_review),
      "https://data.nova.org/vocabulary/tagging/provider" = proposal$provider,
      "https://data.nova.org/vocabulary/tagging/model" = proposal$model,
      "https://data.nova.org/vocabulary/tagging/embeddingModel" =
        proposal$embedding_model,
      "http://purl.org/dc/terms/created" = .tag_time(proposal$created_at),
      "https://data.nova.org/vocabulary/tagging/evidence" =
        .tag_json(proposal$evidence),
      "https://data.nova.org/vocabulary/tagging/prompt" = proposal$prompt,
      "https://data.nova.org/vocabulary/tagging/rawResponse" = proposal$raw_response
    )
    if (!is.null(proposal$tag_embedding)) {
      node[["https://data.nova.org/vocabulary/tagging/labelVector"]] <-
        .tag_json(as.numeric(proposal$tag_embedding))
    }
    node
  }))

  review_nodes <- lapply(state$review_events, function(event) list(
    "@id" = tagging_entity_iri("review", event$event_id, base_iri = base_iri),
    "@type" = .tag_term("ReviewDecision"),
    "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
    "https://schema.org/identifier" = event$event_id,
    "https://data.nova.org/vocabulary/tagging/reviewsProposal" = .tag_ref(
      tagging_entity_iri("proposal", event$proposal_id, base_iri = base_iri)
    ),
    "https://data.nova.org/vocabulary/tagging/proposalId" = event$proposal_id,
    "https://data.nova.org/vocabulary/tagging/level" = as.integer(event$level),
    "https://data.nova.org/vocabulary/tagging/clusterId" = event$cluster_id,
    "https://data.nova.org/vocabulary/tagging/decision" = event$decision,
    "https://data.nova.org/vocabulary/tagging/reviewer" = event$reviewer_id,
    "https://data.nova.org/vocabulary/tagging/rationale" = event$rationale,
    "https://data.nova.org/vocabulary/tagging/originalLabel" = event$original_tag,
    "https://data.nova.org/vocabulary/tagging/resultingLabel" = event$resulting_tag,
    "https://data.nova.org/vocabulary/tagging/similarity" =
      .tag_json(event$similarity),
    "http://purl.org/dc/terms/created" = .tag_time(event$created_at)
  ))

  structure_nodes <- lapply(state$structure_events, function(event) list(
    "@id" = tagging_entity_iri("structure-change", event$event_id,
                                base_iri = base_iri),
    "@type" = .tag_term("QuestionReclassification"),
    "https://data.nova.org/vocabulary/tagging/inRun" = .tag_ref(run_iri),
    "https://schema.org/identifier" = event$event_id,
    "https://data.nova.org/vocabulary/tagging/event" = .tag_json(event)
  ))

  structure(list(
    run = list(run_node),
    embedding = embedding_nodes,
    hierarchy = c(if (is.null(hierarchy_node)) list() else list(hierarchy_node),
                  cluster_nodes, membership_nodes),
    review = c(proposal_nodes, review_nodes, structure_nodes)
  ), class = c("tag_semantic_records", "list"))
}

.tag_id_from_ref <- function(value) value[["@id"]] %||% as.character(value)

.tag_restore_frame <- function(value) {
  if (is.null(value) || is.data.frame(value)) return(value)
  if (!is.list(value)) return(value)
  if (length(value) && is.null(names(value)) &&
      all(vapply(value, is.list, logical(1)))) {
    return(dplyr::bind_rows(value))
  }
  lengths <- lengths(value)
  if (length(value) && !is.null(names(value)) &&
      length(unique(lengths)) == 1L) {
    return(tibble::as_tibble(value))
  }
  value
}

#' Reconstruct tagging state from semantic records
#'
#' @param records Output from [tag_state_to_semantic_records()] or equivalent
#'   query-result nodes grouped by the same partitions.
#' @param questions Normalized question table containing `id` and `caption`.
#' @return Reconstructed `tag_state` without an R serialization projection.
#' @export
tag_state_from_semantic_records <- function(records, questions) {
  questions <- tibble::as_tibble(questions)
  all_nodes <- unlist(records, recursive = FALSE)
  runs <- .tag_nodes_of_type(all_nodes, "TaggingRun")
  hierarchies <- .tag_nodes_of_type(all_nodes, "HierarchyVersion")
  if (length(runs) != 1L || length(hierarchies) > 1L) {
    stop("Semantic records require exactly one run and at most one hierarchy version.",
         call. = FALSE)
  }
  run <- runs[[1]]
  hierarchy <- if (length(hierarchies)) hierarchies[[1]] else NULL
  run_id <- as.character(run[["https://schema.org/identifier"]])
  state <- new_tag_state(questions, run_id)
  state$status <- as.character(run[["https://schema.org/status"]])
  state$revision <- as.integer(run[["https://schema.org/version"]])
  state$created_at <- .tag_parse_time(run[["http://purl.org/dc/terms/created"]])
  state$updated_at <- .tag_parse_time(run[["http://purl.org/dc/terms/modified"]])
  state$workflow <- .tag_json_value(
    run[["https://data.nova.org/vocabulary/tagging/workflow"]], list()
  )

  embedding_nodes <- .tag_nodes_of_type(all_nodes, "QuestionEmbedding")
  if (length(embedding_nodes)) {
    embeddings <- vector("list", nrow(questions))
    for (node in embedding_nodes) {
      id <- as.character(node[["https://schema.org/identifier"]])
      hit <- match(id, questions$id)
      if (!is.na(hit)) embeddings[[hit]] <- as.numeric(.tag_json_value(
        node[["https://data.nova.org/vocabulary/tagging/vector"]]
      ))
    }
    state$embeddings <- if (any(vapply(embeddings, is.null, logical(1)))) {
      embeddings
    } else do.call(rbind, embeddings)
  }

  cluster_nodes <- .tag_nodes_of_type(all_nodes, "QuestionCluster")
  cluster_ids_by_iri <- stats::setNames(
    lapply(cluster_nodes, function(node) list(
      level = as.integer(node[["https://data.nova.org/vocabulary/tagging/level"]]),
      cluster_id = as.character(node[["https://schema.org/identifier"]])
    )),
    vapply(cluster_nodes, `[[`, character(1), "@id")
  )
  parent_for <- function(node) {
    ref <- node[["https://data.nova.org/vocabulary/tagging/parentCluster"]]
    if (is.null(ref)) return(NA_integer_)
    parent <- cluster_ids_by_iri[[.tag_id_from_ref(ref)]]
    if (is.null(parent)) NA_integer_ else as.integer(parent$cluster_id)
  }
  state$clusters <- if (!length(cluster_nodes)) NULL else tibble::tibble(
      level = vapply(cluster_nodes, function(node) as.integer(
        node[["https://data.nova.org/vocabulary/tagging/level"]]), integer(1)),
      cluster_id = vapply(cluster_nodes, function(node) as.integer(
        node[["https://schema.org/identifier"]]), integer(1)),
      parent_cluster = vapply(cluster_nodes, parent_for, integer(1)),
      question_ids = replicate(length(cluster_nodes), character(), simplify = FALSE),
      tag = vapply(cluster_nodes, function(node) as.character(
        node[["http://www.w3.org/2000/01/rdf-schema#label"]] %||% NA_character_
      ), character(1))
    )
  state$clusters_by_level <- if (is.null(hierarchy)) NULL else
    as.integer(.tag_json_value(
      hierarchy[["https://data.nova.org/vocabulary/tagging/clustersByLevel"]],
      integer()
    ))

  memberships <- .tag_nodes_of_type(all_nodes, "LeafClusterMembership")
  max_level <- if (!is.null(state$clusters) && nrow(state$clusters)) {
    max(state$clusters$level)
  } else 0L
  assignments <- if (max_level) questions else NULL
  for (level in seq_len(max_level)) {
    assignments[[paste0("cluster_level_", level)]] <- NA_integer_
  }
  for (node in memberships) {
    question_id <- as.character(node[["https://schema.org/identifier"]])
    row <- match(question_id, assignments$id)
    cluster <- cluster_ids_by_iri[[.tag_id_from_ref(
      node[["https://data.nova.org/vocabulary/tagging/cluster"]]
    )]]
    level <- 1L
    while (!is.null(cluster) && level <= max_level) {
      assignments[[paste0("cluster_level_", level)]][[row]] <-
        as.integer(cluster$cluster_id)
      cluster_node <- cluster_nodes[[which(vapply(
        cluster_nodes, function(candidate) {
          as.integer(candidate[["https://data.nova.org/vocabulary/tagging/level"]]) == level &&
            as.character(candidate[["https://schema.org/identifier"]]) == cluster$cluster_id
        }, logical(1)))[[1]]]]
      parent_ref <- cluster_node[["https://data.nova.org/vocabulary/tagging/parentCluster"]]
      cluster <- if (is.null(parent_ref)) NULL else
        cluster_ids_by_iri[[.tag_id_from_ref(parent_ref)]]
      level <- level + 1L
    }
  }
  state$assignments <- assignments
  if (!is.null(state$clusters) && nrow(state$clusters)) {
    state$clusters$question_ids <- lapply(seq_len(nrow(state$clusters)), function(i) {
      column <- paste0("cluster_level_", state$clusters$level[[i]])
      as.character(assignments$id[
        assignments[[column]] == state$clusters$cluster_id[[i]]
      ])
    })
  }

  proposal_nodes <- .tag_nodes_of_type(all_nodes, "TagProposal")
  for (node in proposal_nodes) {
    id <- as.character(node[["https://schema.org/identifier"]])
    state$proposals[[id]] <- list(
      proposal_id = id, run_id = run_id,
      level = as.integer(node[["https://data.nova.org/vocabulary/tagging/level"]]),
      cluster_id = as.character(node[["https://data.nova.org/vocabulary/tagging/clusterId"]]),
      tag = as.character(node[["http://www.w3.org/2000/01/rdf-schema#label"]]),
      confidence = as.numeric(node[["https://data.nova.org/vocabulary/tagging/confidence"]]),
      rationale = as.character(node[["https://data.nova.org/vocabulary/tagging/rationale"]]),
      needs_review = isTRUE(node[["https://data.nova.org/vocabulary/tagging/needsReview"]]),
      status = as.character(node[["https://schema.org/status"]]),
      provider = as.character(node[["https://data.nova.org/vocabulary/tagging/provider"]]),
      model = as.character(node[["https://data.nova.org/vocabulary/tagging/model"]]),
      embedding_model = as.character(node[["https://data.nova.org/vocabulary/tagging/embeddingModel"]]),
      tag_embedding = if (is.null(node[["https://data.nova.org/vocabulary/tagging/labelVector"]])) NULL else
        as.numeric(.tag_json_value(node[["https://data.nova.org/vocabulary/tagging/labelVector"]])),
      evidence = .tag_json_value(node[["https://data.nova.org/vocabulary/tagging/evidence"]]),
      prompt = node[["https://data.nova.org/vocabulary/tagging/prompt"]],
      raw_response = node[["https://data.nova.org/vocabulary/tagging/rawResponse"]],
      created_at = .tag_parse_time(node[["http://purl.org/dc/terms/created"]])
    )
  }

  review_nodes <- .tag_nodes_of_type(all_nodes, "ReviewDecision")
  state$review_events <- lapply(review_nodes, function(node) {
    similarity <- .tag_json_value(
      node[["https://data.nova.org/vocabulary/tagging/similarity"]]
    )
    if (is.list(similarity) && !is.null(similarity$questions)) {
      similarity$questions <- .tag_restore_frame(similarity$questions)
    }
    list(
    event_id = as.character(node[["https://schema.org/identifier"]]),
    proposal_id = as.character(node[["https://data.nova.org/vocabulary/tagging/proposalId"]]),
    run_id = run_id,
    level = as.integer(node[["https://data.nova.org/vocabulary/tagging/level"]]),
    cluster_id = as.character(node[["https://data.nova.org/vocabulary/tagging/clusterId"]]),
    decision = as.character(node[["https://data.nova.org/vocabulary/tagging/decision"]]),
    reviewer_id = as.character(node[["https://data.nova.org/vocabulary/tagging/reviewer"]]),
    rationale = as.character(node[["https://data.nova.org/vocabulary/tagging/rationale"]]),
    original_tag = as.character(node[["https://data.nova.org/vocabulary/tagging/originalLabel"]]),
    resulting_tag = as.character(node[["https://data.nova.org/vocabulary/tagging/resultingLabel"]]),
    similarity = similarity,
    created_at = .tag_parse_time(node[["http://purl.org/dc/terms/created"]])
    )
  })
  state$structure_events <- lapply(
    .tag_nodes_of_type(all_nodes, "QuestionReclassification"),
    function(node) .tag_json_value(
      node[["https://data.nova.org/vocabulary/tagging/event"]]
    )
  )
  state$tag_matrix <- if (!is.null(state$clusters) && nrow(state$clusters)) {
    build_question_tag_matrix(state$assignments, state$clusters)
  } else NULL
  validate_tag_state(state)
}
