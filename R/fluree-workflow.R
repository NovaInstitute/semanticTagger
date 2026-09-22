# High-level composition of survey retrieval and authoritative tagging state.

.fluree_workflow_questions <- function(...) query_taggable_questions(...)

.fluree_workflow_repository <- function(...) {
  novarush_semantic_repository(...)
}

#' Start or resume a Fluree-backed tagging workflow
#'
#' This is the primary integration entry point for applications. It retrieves
#' normalized survey questions from a caller-selected survey named graph,
#' creates a novaRush-backed semantic store for the tagging graphs, and either
#' starts a new run or resumes the authoritative state already in Fluree.
#'
#' @param config A Fluree configuration created by [novaRush::setConfig()].
#' @param survey_graph Absolute IRI of the novaGraphDB survey named graph.
#' @param tagging_graphs Named list of `run`, `embedding`, `hierarchy`, and
#'   `review` graph IRIs.
#' @param run_id Stable non-empty tagging-run identifier. Reuse it to resume.
#' @param branch Fluree branch used for both survey reads and tagging writes.
#'   Defaults to `config$branch`.
#' @param procedure_id Optional survey-procedure IRI to retain.
#' @param source_form_id Optional source-form IRI to retain.
#' @param page_size Maximum rows retrieved in each survey query.
#' @param batch_size Maximum semantic resources in each tagging write.
#' @param query_page_size Maximum records per tagging-state hydration query.
#' @param transaction_delay_seconds Seconds to pause after each supporting
#'   Fluree transaction during large writes.
#' @param base_iri Base IRI for tagging-domain entities.
#' @param event_callback Optional callback passed when starting a new workflow.
#'
#' @return A `tagging_workflow`, newly checkpointed or reconstructed from
#'   Fluree.
#' @export
fluree_tagging_workflow <- function(
    config, survey_graph, tagging_graphs, run_id,
    branch = config$branch, procedure_id = NULL, source_form_id = NULL,
    page_size = 500L, batch_size = 250L, query_page_size = 100L,
    transaction_delay_seconds = 0,
    base_iri = "https://data.nova.org/tagger/", event_callback = NULL) {
  run_id <- as.character(run_id)
  if (length(run_id) != 1L || is.na(run_id) || !nzchar(run_id)) {
    stop("`run_id` must be one non-empty stable identifier.", call. = FALSE)
  }
  questions <- .fluree_workflow_questions(
    config = config, graph = survey_graph, branch = branch,
    procedure_id = procedure_id, source_form_id = source_form_id,
    page_size = page_size
  )
  if (!nrow(questions)) {
    stop("The selected survey graph contains no taggable questions.",
         call. = FALSE)
  }
  repository <- .fluree_workflow_repository(
    config = config, graphs = tagging_graphs, branch = branch,
    batch_size = batch_size, query_page_size = query_page_size,
    transaction_delay_seconds = transaction_delay_seconds
  )
  store <- semantic_tag_store(
    repository = repository, questions = questions, run_id = run_id,
    base_iri = base_iri
  )
  if (tag_store_exists(store)) {
    resume_tagging_workflow(store)
  } else {
    new_tagging_workflow(
      questions, store = store, run_id = run_id,
      event_callback = event_callback
    )
  }
}
