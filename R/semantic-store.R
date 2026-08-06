.validate_semantic_repository_callbacks <- function(repository) {
  if (!is.list(repository) ||
      !all(vapply(repository[c("exists", "load", "save")], is.function,
                  logical(1)))) {
    stop("A semantic repository requires exists, load, and save callbacks.",
         call. = FALSE)
  }
  invisible(repository)
}

.current_semantic_records <- function(records) {
  runs <- .tag_nodes_of_type(records$run, "TaggingRun")
  if (length(runs) != 1L) {
    stop("The semantic repository did not return exactly one tagging run.",
         call. = FALSE)
  }
  current_ref <- runs[[1]][["https://www.w3.org/ns/prov#hadRevision"]]
  if (is.null(current_ref)) {
    return(list(
      run = runs, embedding = records$embedding %||% list(),
      hierarchy = list(), review = records$review %||% list()
    ))
  }
  current_iri <- .tag_id_from_ref(current_ref)
  hierarchy <- Filter(function(node) {
    type <- .tag_node_type(node)
    if (identical(type, .tag_term("HierarchyVersion"))) {
      return(identical(node[["@id"]], current_iri))
    }
    if (type %in% c(.tag_term("QuestionCluster"),
                    .tag_term("LeafClusterMembership"))) {
      ref <- node[["https://data.nova.org/vocabulary/tagging/inHierarchy"]]
      return(!is.null(ref) && identical(.tag_id_from_ref(ref), current_iri))
    }
    FALSE
  }, records$hierarchy %||% list())
  list(
    run = runs,
    embedding = records$embedding %||% list(),
    hierarchy = unname(hierarchy),
    review = records$review %||% list()
  )
}

#' Create a semantic-repository-backed tagging store
#'
#' The repository is injected as three generic callbacks. This adapter knows
#' tagging records but has no database, graph, branch, or HTTP behavior.
#'
#' @param repository List containing `exists(scope_iri)`, `load(scope_iri)`, and
#'   `save(records, scope_iri, revision)` functions.
#' @param questions Normalized question table used during reconstruction.
#' @param run_id Stable run identifier.
#' @param base_iri Base IRI for tagging entities.
#' @param question_base_iri Survey question base IRI when questions have no
#'   explicit `iri` column.
#' @return A `tag_store` suitable for the resumable workflow.
#' @export
semantic_tag_store <- function(
    repository, questions, run_id,
    base_iri = "https://data.nova.org/tagger/", question_base_iri = NULL) {
  .validate_semantic_repository_callbacks(repository)
  questions <- tibble::as_tibble(questions)
  scope_iri <- tagging_entity_iri("run", run_id, base_iri = base_iri)
  new_tag_store(
    exists = function() isTRUE(repository$exists(scope_iri)),
    load = function() {
      records <- .current_semantic_records(repository$load(scope_iri))
      tag_state_from_semantic_records(records, questions)
    },
    save = function(state) {
      records <- tag_state_to_semantic_records(
        state, base_iri = base_iri, question_base_iri = question_base_iri
      )
      repository$save(records, scope_iri, state$revision)
      invisible(state)
    },
    label = "semantic records",
    metadata = c(
      list(scope_iri = scope_iri, run_id = as.character(run_id)),
      repository$metadata %||% list()
    )
  )
}
