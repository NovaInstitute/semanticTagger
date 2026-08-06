#' Create a persistence-neutral tagging evidence repository
#'
#' The callbacks may be backed by `novaRush`, an in-memory fixture, or another
#' store. `novaTagger` therefore owns evidence meaning without owning transport.
#'
#' @param similar_questions Function accepting an evidence request.
#' @param reviewed_precedents Function accepting an evidence request.
#' @param approved_guidance Function accepting an evidence request.
#'
#' @return A `tagging_evidence_repository`.
#' @export
new_tagging_evidence_repository <- function(
    similar_questions, reviewed_precedents, approved_guidance) {
  callbacks <- list(
    similar_questions = similar_questions,
    reviewed_precedents = reviewed_precedents,
    approved_guidance = approved_guidance
  )
  invalid <- !vapply(callbacks, is.function, logical(1))
  if (any(invalid)) {
    stop(
      "Evidence repository callbacks must be functions: ",
      paste(names(callbacks)[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  structure(callbacks, class = "tagging_evidence_repository")
}

#' Construct a tagging evidence request
#'
#' @param query_vector Numeric embedding representing the current question or
#'   cluster.
#' @param limit Maximum records requested from each evidence source.
#' @param exclude_question_ids Question identifiers to omit.
#' @param embedding_model Optional model identifier used to prevent comparisons
#'   between incompatible embedding spaces.
#' @param run_id Optional tagging-run identifier.
#'
#' @return A validated `tagging_evidence_request`.
#' @export
new_tagging_evidence_request <- function(
    query_vector, limit = 8L, exclude_question_ids = character(),
    embedding_model = NULL, run_id = NULL) {
  query_vector <- as.numeric(query_vector)
  if (!length(query_vector) || any(!is.finite(query_vector))) {
    stop("`query_vector` must be non-empty and finite.", call. = FALSE)
  }
  limit <- as.integer(limit)
  if (length(limit) != 1L || is.na(limit) || limit < 0L) {
    stop("`limit` must be a non-negative integer.", call. = FALSE)
  }
  structure(list(
    query_vector = query_vector,
    limit = limit,
    exclude_question_ids = unique(as.character(exclude_question_ids)),
    embedding_model = if (is.null(embedding_model)) NULL else as.character(embedding_model),
    run_id = if (is.null(run_id)) NULL else as.character(run_id)
  ), class = "tagging_evidence_request")
}

#' Retrieve evidence for a tagging decision
#'
#' @param repository A [new_tagging_evidence_repository()] result.
#' @param request A [new_tagging_evidence_request()] result.
#'
#' @return A named list containing similar questions, reviewed precedents, and
#'   approved guidance. Result schemas remain owned by their domain callbacks.
#' @export
retrieve_tagging_evidence <- function(repository, request) {
  if (!inherits(repository, "tagging_evidence_repository")) {
    stop("`repository` must be a tagging evidence repository.", call. = FALSE)
  }
  if (!inherits(request, "tagging_evidence_request")) {
    stop("`request` must be a tagging evidence request.", call. = FALSE)
  }
  list(
    similar_questions = repository$similar_questions(request),
    reviewed_precedents = repository$reviewed_precedents(request),
    approved_guidance = repository$approved_guidance(request)
  )
}
