.taxonomy_term <- function(term) {
  paste0("https://data.nova.org/vocabulary/tagging/", term)
}

.nr_update_named_graph <- function(delete = NULL, insert = NULL, graph,
                                   config, branch) {
  novaRush::updateNamedGraph(
    delete = delete, insert = insert, graph = graph,
    config = config, branch = branch
  )
}

.taxonomy_scalar_character <- function(value, name) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    stop("`", name, "` must be one non-empty string.", call. = FALSE)
  }
  value
}

.taxonomy_checksum_value <- function(value) {
  json <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", na = "null", digits = NA,
    dataframe = "rows"
  )
  digest::digest(as.character(json), algo = "sha256", serialize = FALSE)
}

.taxonomy_normalize_nodes <- function(nodes) {
  nodes <- tibble::as_tibble(nodes)
  required <- c("id", "parent_id", "label")
  if (!all(required %in% names(nodes))) {
    stop("Taxonomy nodes require `id`, `parent_id`, and `label` columns.",
         call. = FALSE)
  }
  nodes <- nodes[, required, drop = FALSE]
  nodes$id <- as.character(nodes$id)
  nodes$parent_id <- as.character(nodes$parent_id)
  nodes$parent_id[is.na(nodes$parent_id) | !nzchar(nodes$parent_id)] <- NA_character_
  nodes$label <- trimws(as.character(nodes$label))
  nodes[order(nodes$id), , drop = FALSE]
}

.taxonomy_normalize_assignments <- function(assignments) {
  assignments <- tibble::as_tibble(assignments)
  required <- c("question_id", "leaf_id")
  if (!all(required %in% names(assignments))) {
    stop("Taxonomy assignments require `question_id` and `leaf_id` columns.",
         call. = FALSE)
  }
  assignments <- assignments[, required, drop = FALSE]
  assignments$question_id <- as.character(assignments$question_id)
  assignments$leaf_id <- as.character(assignments$leaf_id)
  assignments <- assignments[
    !is.na(assignments$leaf_id) & nzchar(assignments$leaf_id), , drop = FALSE
  ]
  assignments[order(assignments$question_id), , drop = FALSE]
}

.taxonomy_normalize_progress <- function(progress) {
  progress <- progress %||% list()
  if (!is.list(progress)) {
    stop("`progress` must be a list.", call. = FALSE)
  }
  fields <- c("reviewed_splits", "deferred_splits", "outstanding_questions")
  out <- stats::setNames(vector("list", length(fields)), fields)
  for (field in fields) {
    values <- as.character(progress[[field]] %||% character())
    out[[field]] <- sort(unique(values[!is.na(values) & nzchar(values)]))
  }
  out
}

.taxonomy_cycle <- function(nodes) {
  parents <- stats::setNames(nodes$parent_id, nodes$id)
  for (start in nodes$id) {
    seen <- character()
    current <- start
    while (!is.na(current)) {
      if (current %in% seen) return(c(seen, current))
      seen <- c(seen, current)
      current <- unname(parents[[current]])
      if (is.null(current)) current <- NA_character_
    }
  }
  character()
}

#' Validate a reviewed taxonomy
#'
#' A reviewed taxonomy is a transport-free current-state tree with explicit
#' parent links and one direct leaf assignment per assigned question.
#'
#' @param taxonomy Object created by [new_reviewed_taxonomy()].
#' @return `taxonomy`, invisibly, when valid.
#' @export
validate_reviewed_taxonomy <- function(taxonomy) {
  if (!is.list(taxonomy) ||
      !all(c("run_id", "revision", "nodes", "assignments", "progress") %in%
           names(taxonomy))) {
    stop("A reviewed taxonomy requires run metadata, nodes, assignments, and progress.",
         call. = FALSE)
  }
  nodes <- taxonomy$nodes
  assignments <- taxonomy$assignments
  if (!nrow(nodes)) stop("A reviewed taxonomy requires at least one node.", call. = FALSE)
  if (anyNA(nodes$id) || any(!nzchar(nodes$id)) || anyDuplicated(nodes$id)) {
    stop("Taxonomy node IDs must be unique non-empty strings.", call. = FALSE)
  }
  if (anyNA(nodes$label) || any(!nzchar(nodes$label))) {
    stop("Taxonomy labels must be non-empty.", call. = FALSE)
  }
  roots <- which(is.na(nodes$parent_id))
  if (length(roots) != 1L) {
    stop("A reviewed taxonomy requires exactly one root node.", call. = FALSE)
  }
  missing_parents <- setdiff(stats::na.omit(nodes$parent_id), nodes$id)
  if (length(missing_parents)) {
    stop("Taxonomy parent IDs were not found: ",
         paste(missing_parents, collapse = ", "), ".", call. = FALSE)
  }
  cycle <- .taxonomy_cycle(nodes)
  if (length(cycle)) {
    stop("The taxonomy contains a parent cycle involving: ",
         paste(unique(cycle), collapse = ", "), ".", call. = FALSE)
  }
  sibling_key <- paste(ifelse(is.na(nodes$parent_id), "<root>", nodes$parent_id),
                       tolower(nodes$label), sep = "\r")
  if (anyDuplicated(sibling_key)) {
    stop("Sibling taxonomy labels must be unique ignoring case.", call. = FALSE)
  }
  if (anyNA(assignments$question_id) || any(!nzchar(assignments$question_id)) ||
      anyDuplicated(assignments$question_id)) {
    stop("Each assigned question must have one unique non-empty ID.",
         call. = FALSE)
  }
  leaves <- setdiff(nodes$id, stats::na.omit(nodes$parent_id))
  invalid_leaves <- setdiff(assignments$leaf_id, leaves)
  if (length(invalid_leaves)) {
    stop("Question assignments must reference existing leaf nodes: ",
         paste(invalid_leaves, collapse = ", "), ".", call. = FALSE)
  }
  invisible(taxonomy)
}

#' Create a canonical reviewed taxonomy
#'
#' @param nodes Data frame with stable `id`, `parent_id`, and `label` columns.
#' @param assignments Data frame with `question_id` and `leaf_id` columns.
#' @param run_id Tagging run identifier.
#' @param revision Non-negative current-state revision.
#' @param progress Optional reviewed/deferred/outstanding identifiers.
#' @return A validated `reviewed_taxonomy` object.
#' @export
new_reviewed_taxonomy <- function(nodes, assignments, run_id, revision = 0L,
                                  progress = list()) {
  run_id <- .taxonomy_scalar_character(run_id, "run_id")
  revision <- suppressWarnings(as.integer(revision))
  if (length(revision) != 1L || is.na(revision) || revision < 0L) {
    stop("`revision` must be one non-negative integer.", call. = FALSE)
  }
  taxonomy <- structure(list(
    run_id = run_id,
    revision = revision,
    nodes = .taxonomy_normalize_nodes(nodes),
    assignments = .taxonomy_normalize_assignments(assignments),
    progress = .taxonomy_normalize_progress(progress)
  ), class = c("reviewed_taxonomy", "list"))
  validate_reviewed_taxonomy(taxonomy)
  taxonomy$taxonomy_checksum <- .taxonomy_checksum_value(taxonomy$nodes)
  taxonomy$assignment_checksum <- .taxonomy_checksum_value(taxonomy$assignments)
  taxonomy
}

#' Project a reviewed taxonomy to transport-free JSON-LD records
#'
#' @param taxonomy A [new_reviewed_taxonomy()] result.
#' @param base_iri Absolute base IRI for the tagging run.
#' @return Named list containing `run` and `hierarchy` record lists.
#' @export
reviewed_taxonomy_records <- function(
    taxonomy, base_iri = "https://data.nova.org/tagger/") {
  validate_reviewed_taxonomy(taxonomy)
  base_iri <- .tag_absolute_base(base_iri)
  run_iri <- tagging_entity_iri("run", taxonomy$run_id, base_iri = base_iri)
  ref <- function(id) list("@id" = id)
  cluster_iri <- stats::setNames(vapply(taxonomy$nodes$id, function(id) {
    tagging_entity_iri("taxonomy", taxonomy$run_id, "cluster", id,
                       base_iri = base_iri)
  }, character(1)), taxonomy$nodes$id)
  clusters <- lapply(seq_len(nrow(taxonomy$nodes)), function(i) {
    row <- taxonomy$nodes[i, , drop = FALSE]
    node <- list(
      "@id" = unname(cluster_iri[[row$id[[1L]]]]),
      "@type" = .taxonomy_term("QuestionCluster"),
      "https://schema.org/identifier" = row$id[[1L]],
      "http://www.w3.org/2000/01/rdf-schema#label" = row$label[[1L]]
    )
    node[[.taxonomy_term("inRun")]] <- ref(run_iri)
    if (!is.na(row$parent_id[[1L]])) {
      node[[.taxonomy_term("parentCluster")]] <-
        ref(unname(cluster_iri[[row$parent_id[[1L]]]]))
    }
    node
  })
  assignments <- lapply(seq_len(nrow(taxonomy$assignments)), function(i) {
    row <- taxonomy$assignments[i, , drop = FALSE]
    node <- list(
      "@id" = row$question_id[[1L]],
      "@type" = .taxonomy_term("TaggedQuestion"),
      "https://schema.org/identifier" = row$question_id[[1L]]
    )
    node[[.taxonomy_term("inRun")]] <- ref(run_iri)
    node[[.taxonomy_term("assignedLeaf")]] <-
      ref(unname(cluster_iri[[row$leaf_id[[1L]]]]))
    node
  })
  run <- list(
    "@id" = run_iri,
    "@type" = .taxonomy_term("TaggingRun"),
    "https://schema.org/identifier" = taxonomy$run_id,
    "https://schema.org/version" = taxonomy$revision
  )
  run[[.taxonomy_term("taxonomyChecksum")]] <- taxonomy$taxonomy_checksum
  run[[.taxonomy_term("assignmentChecksum")]] <- taxonomy$assignment_checksum
  run[[.taxonomy_term("reviewProgress")]] <- list(
    "@value" = taxonomy$progress, "@type" = "@json"
  )
  structure(list(run = list(run), hierarchy = c(clusters, assignments)),
            class = c("reviewed_taxonomy_records", "list"))
}

#' Reconstruct a reviewed taxonomy from semantic records
#'
#' @param records List containing `run` and `hierarchy` record lists.
#' @return A validated `reviewed_taxonomy`.
#' @export
reviewed_taxonomy_from_records <- function(records) {
  if (!is.list(records) ||
      !all(c("run", "hierarchy") %in% names(records))) {
    stop("Reviewed taxonomy records require `run` and `hierarchy` partitions.",
         call. = FALSE)
  }
  runs <- Filter(function(node) {
    identical(node[["@type"]], .taxonomy_term("TaggingRun")) &&
      !is.null(node[[.taxonomy_term("taxonomyChecksum")]])
  }, records$run)
  if (!length(runs)) {
    stop("The records do not contain a published reviewed taxonomy.",
         call. = FALSE)
  }
  versions <- vapply(runs, function(node) suppressWarnings(as.integer(
    node[["https://schema.org/version"]]
  )), integer(1))
  run <- runs[[which.max(versions)]]
  clusters <- Filter(function(node) {
    identical(node[["@type"]], .taxonomy_term("QuestionCluster"))
  }, records$hierarchy)
  tagged <- Filter(function(node) {
    identical(node[["@type"]], .taxonomy_term("TaggedQuestion"))
  }, records$hierarchy)
  if (!length(clusters)) {
    stop("The records do not contain current taxonomy clusters.", call. = FALSE)
  }
  ids_by_iri <- stats::setNames(vapply(clusters, function(node) {
    as.character(node[["https://schema.org/identifier"]])
  }, character(1)), vapply(clusters, `[[`, character(1), "@id"))
  nodes <- tibble::tibble(
    id = unname(ids_by_iri),
    parent_id = vapply(clusters, function(node) {
      ref <- node[[.taxonomy_term("parentCluster")]]
      if (is.null(ref)) return(NA_character_)
      value <- ids_by_iri[[.tag_id_from_ref(ref)]]
      if (is.null(value)) NA_character_ else unname(value)
    }, character(1)),
    label = vapply(clusters, function(node) as.character(
      node[["http://www.w3.org/2000/01/rdf-schema#label"]] %||% ""
    ), character(1))
  )
  assignments <- if (!length(tagged)) {
    tibble::tibble(question_id = character(), leaf_id = character())
  } else tibble::tibble(
    question_id = vapply(tagged, `[[`, character(1), "@id"),
    leaf_id = vapply(tagged, function(node) {
      ref <- node[[.taxonomy_term("assignedLeaf")]]
      value <- if (is.null(ref)) NULL else ids_by_iri[[.tag_id_from_ref(ref)]]
      if (is.null(value)) NA_character_ else unname(value)
    }, character(1))
  )
  progress_value <- run[[.taxonomy_term("reviewProgress")]]
  progress <- if (is.null(progress_value)) list() else
    .tag_json_value(progress_value, list())
  taxonomy <- new_reviewed_taxonomy(
    nodes = nodes, assignments = assignments,
    run_id = as.character(run[["https://schema.org/identifier"]]),
    revision = as.integer(run[["https://schema.org/version"]]),
    progress = progress
  )
  if (!identical(taxonomy$taxonomy_checksum,
                 as.character(run[[.taxonomy_term("taxonomyChecksum")]])) ||
      !identical(taxonomy$assignment_checksum,
                 as.character(run[[.taxonomy_term("assignmentChecksum")]]))) {
    stop("Published taxonomy checksums do not match its semantic records.",
         call. = FALSE)
  }
  taxonomy
}

#' Compare two reviewed taxonomies
#'
#' @param before Earlier taxonomy.
#' @param after Later taxonomy.
#' @return Named integer counts suitable for publication previews.
#' @export
reviewed_taxonomy_diff <- function(before, after) {
  validate_reviewed_taxonomy(before)
  validate_reviewed_taxonomy(after)
  before_nodes <- stats::setNames(before$nodes$label, before$nodes$id)
  after_nodes <- stats::setNames(after$nodes$label, after$nodes$id)
  shared <- intersect(names(before_nodes), names(after_nodes))
  before_assign <- stats::setNames(before$assignments$leaf_id,
                                   before$assignments$question_id)
  after_assign <- stats::setNames(after$assignments$leaf_id,
                                  after$assignments$question_id)
  shared_questions <- intersect(names(before_assign), names(after_assign))
  c(
    clusters_created = length(setdiff(names(after_nodes), names(before_nodes))),
    clusters_removed = length(setdiff(names(before_nodes), names(after_nodes))),
    clusters_renamed = sum(before_nodes[shared] != after_nodes[shared]),
    assignments_added = length(setdiff(names(after_assign), names(before_assign))),
    assignments_removed = length(setdiff(names(before_assign), names(after_assign))),
    assignments_moved = sum(before_assign[shared_questions] !=
                            after_assign[shared_questions])
  )
}

#' Create a compact taxonomy publication record
#'
#' @param taxonomy Published reviewed taxonomy.
#' @param publication_id Deterministic publication identifier.
#' @param reviewer Reviewer identity.
#' @param source_branch Review branch being promoted.
#' @param base_main_head Main commit from which the review branch was created.
#' @param changes Optional named change counts.
#' @param note Optional publication note.
#' @param created_at Publication preparation time.
#' @param base_iri Absolute base IRI for tagging records.
#' @return One transport-free JSON-LD publication resource.
#' @export
taxonomy_publication_record <- function(
    taxonomy, publication_id, reviewer, source_branch, base_main_head,
    changes = integer(), note = "", created_at = Sys.time(),
    base_iri = "https://data.nova.org/tagger/") {
  validate_reviewed_taxonomy(taxonomy)
  publication_id <- .taxonomy_scalar_character(publication_id, "publication_id")
  reviewer <- .taxonomy_scalar_character(reviewer, "reviewer")
  source_branch <- .taxonomy_scalar_character(source_branch, "source_branch")
  base_main_head <- .taxonomy_scalar_character(base_main_head, "base_main_head")
  base_iri <- .tag_absolute_base(base_iri)
  node <- list(
    "@id" = tagging_entity_iri(
      "taxonomy-publication", taxonomy$run_id, publication_id,
      base_iri = base_iri
    ),
    "@type" = .taxonomy_term("TaxonomyPublication"),
    "https://schema.org/identifier" = publication_id,
    "https://schema.org/version" = taxonomy$revision,
    "http://purl.org/dc/terms/created" = .tag_time(created_at)
  )
  node[[.taxonomy_term("inRun")]] <- list(
    "@id" = tagging_entity_iri("run", taxonomy$run_id, base_iri = base_iri)
  )
  node[[.taxonomy_term("reviewer")]] <- reviewer
  node[[.taxonomy_term("sourceBranch")]] <- source_branch
  node[[.taxonomy_term("baseMainHead")]] <- base_main_head
  node[[.taxonomy_term("taxonomyChecksum")]] <- taxonomy$taxonomy_checksum
  node[[.taxonomy_term("assignmentChecksum")]] <- taxonomy$assignment_checksum
  node[[.taxonomy_term("changes")]] <- list(
    "@value" = as.list(changes), "@type" = "@json"
  )
  if (nzchar(trimws(as.character(note)))) {
    node[["http://purl.org/dc/terms/description"]] <- trimws(as.character(note))
  }
  node
}

.taxonomy_repository_batches <- function(records, batch_size) {
  if (!length(records)) return(list())
  split(records, ceiling(seq_along(records) / batch_size))
}

#' Create a novaRush-backed current-taxonomy repository
#'
#' The repository replaces the current run-scoped hierarchy on an isolated
#' branch, publishes the small run header last, and appends a compact
#' publication record. It does not merge branches.
#'
#' @param config A Fluree configuration created by [novaRush::setConfig()].
#' @param graphs Named tagging graph list containing `run`, `embedding`,
#'   `hierarchy`, and `review`.
#' @param branch Review branch to prepare.
#' @param base_iri Tagging entity base IRI.
#' @param batch_size Maximum hierarchy records per write.
#' @param query_page_size Maximum records per verification query.
#' @return Repository callbacks `replace()` and `load()`.
#' @export
novarush_reviewed_taxonomy_repository <- function(
    config, graphs, branch = config$branch,
    base_iri = "https://data.nova.org/tagger/", batch_size = 250L,
    query_page_size = 500L) {
  graphs <- .nr_validate_graphs(graphs)
  base_iri <- .tag_absolute_base(base_iri)
  batch_size <- suppressWarnings(as.integer(batch_size))
  query_page_size <- suppressWarnings(as.integer(query_page_size))
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L ||
      length(query_page_size) != 1L || is.na(query_page_size) ||
      query_page_size < 1L) {
    stop("Batch and query page sizes must be positive integers.", call. = FALSE)
  }
  scope_for <- function(run_id) tagging_entity_iri(
    "run", run_id, base_iri = base_iri
  )
  load <- function(run_id) {
    scope <- scope_for(run_id)
    list(
      run = .nr_query_payloads(
        scope, graphs$run, config, branch, query_page_size
      ),
      hierarchy = .nr_query_payloads(
        scope, graphs$hierarchy, config, branch, query_page_size
      ),
      review = .nr_query_payloads(
        scope, graphs$review, config, branch, query_page_size
      )
    )
  }
  delete_records <- function(records, graph, scope) {
    for (batch in .taxonomy_repository_batches(records, batch_size)) {
      .nr_update_named_graph(
        delete = lapply(batch, .nr_envelope, scope_iri = scope),
        graph = graph, config = config, branch = branch
      )
    }
  }
  insert_records <- function(records, graph, scope) {
    for (batch in .taxonomy_repository_batches(records, batch_size)) {
      .nr_upsert_named_graph(
        lapply(batch, .nr_envelope, scope_iri = scope), graph,
        config = config, branch = branch
      )
    }
  }
  replace <- function(taxonomy, publication) {
    validate_reviewed_taxonomy(taxonomy)
    if (!is.list(publication) ||
        !identical(publication[["@type"]],
                   .taxonomy_term("TaxonomyPublication"))) {
      stop("`publication` must be a taxonomy publication record.",
           call. = FALSE)
    }
    records <- reviewed_taxonomy_records(taxonomy, base_iri = base_iri)
    scope <- scope_for(taxonomy$run_id)
    current <- load(taxonomy$run_id)
    if (length(current$run)) {
      versions <- vapply(current$run, function(node) suppressWarnings(as.integer(
        node[["https://schema.org/version"]]
      )), integer(1))
      preserved <- current$run[[which.max(versions)]]
      preserved[["https://www.w3.org/ns/prov#hadRevision"]] <- NULL
      records$run[[1L]] <- utils::modifyList(preserved, records$run[[1L]])
    }
    records$run[[1L]][["http://purl.org/dc/terms/modified"]] <- .tag_time(Sys.time())
    records$run[[1L]][[.taxonomy_term("latestPublication")]] <-
      list("@id" = publication[["@id"]])

    # The review branch is intentionally isolated while these bounded writes
    # replace its current projection. The small run header is switched last.
    delete_records(current$hierarchy, graphs$hierarchy, scope)
    insert_records(records$hierarchy, graphs$hierarchy, scope)
    delete_records(current$run, graphs$run, scope)
    insert_records(records$run, graphs$run, scope)
    insert_records(list(publication), graphs$review, scope)

    verified <- load(taxonomy$run_id)
    expected_run <- records$run[[1L]]
    matching_run <- Filter(function(node) {
      identical(node[[.taxonomy_term("taxonomyChecksum")]],
                taxonomy$taxonomy_checksum) &&
        identical(node[[.taxonomy_term("assignmentChecksum")]],
                  taxonomy$assignment_checksum)
    }, verified$run)
    if (length(matching_run) != 1L ||
        length(verified$hierarchy) != length(records$hierarchy)) {
      stop("The prepared taxonomy could not be verified on the review branch.",
           call. = FALSE)
    }
    invisible(list(
      taxonomy = taxonomy, publication = publication,
      hierarchy_records = length(records$hierarchy), verified = TRUE
    ))
  }
  list(replace = replace, load = load,
       metadata = list(branch = branch, graphs = graphs, base_iri = base_iri))
}
