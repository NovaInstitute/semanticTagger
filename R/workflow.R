# Provider-neutral, resumable tagging workflow orchestration.

.workflow_emit <- function(callback, operation, state, body = list()) {
  if (is.function(callback)) callback(list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    system = "novaTagger", direction = "state", operation = operation,
    stage = state$workflow$stage %||% NA_character_, body = body
  ))
  invisible(state)
}

.validate_tagging_workflow <- function(workflow) {
  if (!inherits(workflow, "tagging_workflow")) {
    stop("`workflow` must be a tagging_workflow.", call. = FALSE)
  }
  validate_tag_store(workflow$store)
  workflow$state <- validate_tag_state(workflow$state)
  workflow
}

.workflow_checkpoint <- function(workflow) {
  workflow <- .validate_tagging_workflow(workflow)
  workflow$state <- tag_store_save(workflow$store, workflow$state)
  workflow
}

#' Start a resumable tagging workflow
#'
#' The input must already be normalized by the survey extraction package.
#'
#' @param questions Data frame containing unique `id` and `caption` columns.
#' @param store Persistence-neutral [new_tag_store()] implementation.
#' @param run_id Optional stable run identifier.
#' @param event_callback Optional function receiving structured state events.
#' @return A `tagging_workflow` checkpointed at `questions_ready`.
#' @export
new_tagging_workflow <- function(questions, store = memory_tag_store(),
                                 run_id = NULL, event_callback = NULL) {
  questions <- tibble::as_tibble(questions)
  if (!all(c("id", "caption") %in% names(questions))) {
    stop("Questions require `id` and `caption` columns.", call. = FALSE)
  }
  if (!nrow(questions) || anyNA(questions$id) || anyNA(questions$caption) ||
      any(!nzchar(as.character(questions$id))) ||
      any(!nzchar(as.character(questions$caption))) || anyDuplicated(questions$id)) {
    stop("Question IDs and captions must be non-empty and IDs must be unique.", call. = FALSE)
  }
  if (tag_store_exists(store)) {
    stop("The tagging store already contains a run; resume it instead.", call. = FALSE)
  }
  state <- new_tag_state(questions, run_id)
  state$workflow <- list(
    version = 1L, stage = "questions_ready", model_provider = NULL,
    embedding_model = NULL, generation_model = NULL,
    clustering_method = NULL
  )
  workflow <- structure(list(state = state, store = store), class = "tagging_workflow")
  workflow <- .workflow_checkpoint(workflow)
  .workflow_emit(event_callback, "questions_ready", workflow$state,
                 list(question_count = nrow(questions)))
  workflow
}

#' Resume a tagging workflow
#'
#' @param store A tagging store containing a previously checkpointed run.
#' @return A `tagging_workflow` at its last completed checkpoint.
#' @export
resume_tagging_workflow <- function(store) {
  validate_tag_store(store)
  structure(list(state = tag_store_load(store), store = store),
            class = "tagging_workflow")
}

#' Embed workflow questions in resumable batches
#'
#' Each successful batch is checkpointed, so a later provider failure does not
#' discard earlier batches.
#'
#' @param workflow A `tagging_workflow`.
#' @param provider A [new_model_provider()] implementation.
#' @param batch_size Maximum questions per provider request.
#' @param checkpoint_size Approximate number of newly embedded questions between
#'   persistence checkpoints. Defaults to `batch_size`, preserving the behavior
#'   of checkpointing every provider request. A larger value reduces persistence
#'   traffic at the cost of repeating at most that many embeddings after a crash.
#' @param event_callback Optional structured event callback.
#' @param progress_callback Optional function accepting `(completed, total)`.
#' @return Updated workflow.
#' @export
workflow_embed_questions <- function(workflow, provider, batch_size = 100L,
                                     checkpoint_size = batch_size,
                                     event_callback = NULL,
                                     progress_callback = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  validate_model_provider(provider)
  state <- workflow$state
  total <- nrow(state$questions)
  embeddings <- state$embeddings %||% vector("list", total)
  if (is.matrix(embeddings)) {
    embeddings <- lapply(seq_len(nrow(embeddings)), function(i) embeddings[i, ])
  }
  if (length(embeddings) != total) {
    stop("Checkpoint embedding count does not match the questions.", call. = FALSE)
  }
  pending <- which(vapply(embeddings, is.null, logical(1)))
  if (!length(pending) && identical(state$workflow$stage, "embedded")) return(workflow)
  batch_size <- max(1L, as.integer(batch_size))
  checkpoint_size <- suppressWarnings(as.integer(checkpoint_size))
  if (length(checkpoint_size) != 1L || is.na(checkpoint_size) ||
      checkpoint_size < 1L) {
    stop("`checkpoint_size` must be one positive integer.", call. = FALSE)
  }
  batches <- split(pending, ceiling(seq_along(pending) / batch_size))
  since_checkpoint <- 0L
  for (batch_number in seq_along(batches)) {
    indices <- batches[[batch_number]]
    if (!length(indices)) next
    embeddings[indices] <- model_embed_batch(
      provider, state$questions$caption[indices], event_callback
    )
    state$embeddings <- embeddings
    state$workflow$stage <- "embedding"
    state$workflow$model_provider <- provider$name
    state$workflow$embedding_model <- provider$embedding_model
    state$workflow$generation_model <- provider$generation_model
    since_checkpoint <- since_checkpoint + length(indices)
    should_checkpoint <- since_checkpoint >= checkpoint_size ||
      batch_number == length(batches)
    if (should_checkpoint) {
      workflow$state <- state
      workflow <- .workflow_checkpoint(workflow)
      state <- workflow$state
      completed <- sum(!vapply(embeddings, is.null, logical(1)))
      if (is.function(progress_callback)) progress_callback(completed, total)
      .workflow_emit(event_callback, "embedding_checkpoint", state,
                     list(completed = completed, total = total))
      since_checkpoint <- 0L
    }
  }
  state$embeddings <- do.call(rbind, .validate_embedding_vectors(embeddings, total))
  state$workflow$stage <- "embedded"
  workflow$state <- state
  .workflow_checkpoint(workflow)
}

#' Build a hierarchical clustering checkpoint
#'
#' @param workflow An embedded `tagging_workflow`.
#' @param clusters_by_level Cluster counts ordered from leaf to root.
#' @param event_callback Optional structured event callback.
#' @return Updated workflow at `clustered`.
#' @export
workflow_cluster_hierarchical <- function(workflow, clusters_by_level,
                                          event_callback = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  state <- workflow$state
  if (!is.matrix(state$embeddings) || nrow(state$embeddings) != nrow(state$questions)) {
    stop("Embed all questions before clustering.", call. = FALSE)
  }
  clusters_by_level <- as.integer(clusters_by_level)
  if (!length(clusters_by_level) || anyNA(clusters_by_level) ||
      any(clusters_by_level < 1L) || any(clusters_by_level > nrow(state$questions))) {
    stop("Invalid cluster counts for this question set.", call. = FALSE)
  }
  fit <- cluster_embeddings(state$embeddings, clusters_by_level)
  state$hclust <- fit$hclust
  state$assignments <- add_cluster_assignments(
    state$questions, fit$hclust, clusters_by_level
  )
  state$clusters <- build_cluster_index(state$assignments, clusters_by_level)
  state$clusters_by_level <- clusters_by_level
  state$workflow$stage <- "clustered"
  state$workflow$clustering_method <- "hierarchical_ward_d2"
  state$workflow$hierarchy_version <- state$revision + 1L
  workflow$state <- state
  workflow <- .workflow_checkpoint(workflow)
  .workflow_emit(event_callback, "hierarchy_checkpoint", workflow$state,
                 list(clusters_by_level = clusters_by_level))
  workflow
}

#' Build a BERTopic hierarchy checkpoint
#'
#' @param workflow An embedded `tagging_workflow`.
#' @param bertopic_kwargs Named arguments passed to the BERTopic constructor.
#' @param event_callback Optional structured event callback.
#' @return Updated workflow at `clustered`.
#' @export
workflow_cluster_bertopic <- function(workflow, bertopic_kwargs = list(),
                                      event_callback = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  state <- workflow$state
  if (!is.matrix(state$embeddings) || nrow(state$embeddings) != nrow(state$questions)) {
    stop("Embed all questions before clustering.", call. = FALSE)
  }
  fit <- fit_bertopic_topics(
    state$questions$caption, state$embeddings, bertopic_kwargs
  )
  questions <- state$questions
  questions$topic_id <- fit$topic_ids
  hierarchy <- build_bertopic_hierarchy(
    questions, state$embeddings, topic_levels = NULL
  )
  state$questions <- hierarchy$questions
  state$assignments <- hierarchy$assignments
  state$clusters <- hierarchy$clusters
  state$clusters_by_level <- hierarchy$clusters_by_level
  state$workflow$stage <- "clustered"
  state$workflow$clustering_method <- "bertopic"
  state$workflow$hierarchy_version <- state$revision + 1L
  workflow$state <- state
  workflow <- .workflow_checkpoint(workflow)
  .workflow_emit(event_callback, "hierarchy_checkpoint", workflow$state,
                 list(clusters_by_level = hierarchy$clusters_by_level,
                      method = "bertopic"))
  workflow
}

#' Find the next bottom-up cluster needing a proposal
#'
#' @param workflow A clustered `tagging_workflow`.
#' @param level Optional hierarchy-level restriction.
#' @return One cluster row, or a zero-row table when none is ready.
#' @export
workflow_next_cluster <- function(workflow, level = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  clusters <- workflow$state$clusters
  if (is.null(clusters)) stop("Build a hierarchy before proposing tags.", call. = FALSE)
  pending <- is.na(clusters$tag) | !nzchar(clusters$tag) | clusters$tag == "untagged"
  awaiting <- vapply(seq_len(nrow(clusters)), function(i) {
    proposals <- Filter(function(proposal) {
      identical(as.integer(proposal$level), as.integer(clusters$level[[i]])) &&
        identical(as.character(proposal$cluster_id), as.character(clusters$cluster_id[[i]]))
    }, workflow$state$proposals)
    any(vapply(proposals, function(proposal) proposal$status %in% c("proposed", "deferred"), logical(1)))
  }, logical(1))
  pending <- pending & !awaiting
  if (!is.null(level)) pending <- pending & clusters$level == as.integer(level)
  hit <- which(pending)
  if (!length(hit)) return(clusters[0, , drop = FALSE])
  clusters[hit[[1]], , drop = FALSE]
}

#' Propose a tag for the next ready cluster
#'
#' Evidence is supplied by the caller, keeping retrieval and persistence out of
#' the workflow controller.
#'
#' @param workflow A clustered `tagging_workflow`.
#' @param provider A model provider.
#' @param level Optional hierarchy-level restriction.
#' @param evidence,precedents,guidance Provider-neutral evidence tables.
#' @param sample_size Questions sampled into the prompt.
#' @param event_callback Optional structured event callback.
#' @return Updated workflow with a checkpointed proposal.
#' @export
workflow_propose_next <- function(
    workflow, provider, level = NULL,
    evidence = tibble::tibble(
      question_id = character(), question_text = character(), score = numeric()
    ),
    precedents = .empty_tag_precedents(), guidance = .empty_tagging_guidance(),
    sample_size = 5L, event_callback = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  validate_model_provider(provider)
  target <- workflow_next_cluster(workflow, level)
  if (!nrow(target)) return(workflow)
  cluster_level <- target$level[[1]]
  cluster_id <- target$cluster_id[[1]]
  profile <- get_cluster_profile(
    workflow$state, cluster_id, cluster_level, sample_size
  )
  prompt <- cluster_tag_prompt(profile, evidence, precedents, guidance)
  raw <- model_generate(provider, prompt, format = "json",
                        trace_callback = event_callback)
  proposal <- parse_tag_proposal_response(raw)
  tag_embedding <- model_embed(provider, proposal$tag, event_callback)
  proposal_ids_before <- names(workflow$state$proposals)
  workflow$state <- register_tag_proposal(
    workflow$state, cluster_level, cluster_id, proposal$tag,
    proposal$confidence, proposal$rationale, proposal$needs_review,
    provider$name, provider$generation_model, provider$embedding_model,
    tag_embedding,
    evidence = list(
      questions = evidence, precedents = precedents, guidance = guidance
    ),
    prompt = prompt, raw_response = raw
  )
  proposal_id <- setdiff(names(workflow$state$proposals), proposal_ids_before)
  if (length(proposal_id) != 1L) {
    stop("Exactly one proposal must be registered per checkpoint.", call. = FALSE)
  }
  proposal_id <- proposal_id[[1L]]
  workflow$state$workflow$stage <- "review"
  workflow$state$workflow$model_provider <- provider$name
  workflow$state$workflow$generation_model <- provider$generation_model
  workflow$state <- .tag_store_save_focused(
    workflow$store, workflow$state, "save_proposal", proposal_id
  )
  .workflow_emit(event_callback, "proposal_checkpoint", workflow$state,
                 list(level = cluster_level, cluster_id = cluster_id))
  workflow
}

.workflow_stage_after_review <- function(workflow) {
  if (nrow(workflow_next_cluster(workflow))) return("tagging")
  unresolved <- any(vapply(workflow$state$proposals, function(proposal) {
    proposal$status %in% c("proposed", "deferred")
  }, logical(1)))
  if (unresolved) "review" else "complete"
}

#' Apply a reviewer decision and checkpoint it
#'
#' @param workflow A `tagging_workflow` with the proposal.
#' @param proposal_id Proposal identifier.
#' @param decision Review decision accepted by [review_tag_proposal()].
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Optional explanation.
#' @param tag Edited tag when `decision = "edited"`.
#' @param provider Required for edited-label embeddings.
#' @param event_callback Optional structured event callback.
#' @return Updated workflow.
#' @export
workflow_review_proposal <- function(workflow, proposal_id, decision,
                                     reviewer_id, rationale = "", tag = NULL,
                                     provider = NULL, event_callback = NULL) {
  workflow <- .validate_tagging_workflow(workflow)
  embed_tag <- NULL
  if (identical(decision, "edited")) {
    validate_model_provider(provider)
    embed_tag <- function(value) model_embed(provider, value, event_callback)
  }
  event_count <- length(workflow$state$review_events)
  workflow$state <- review_tag_proposal(
    workflow$state, proposal_id, decision, reviewer_id, rationale, tag,
    embed_tag = embed_tag
  )
  workflow$state$workflow$stage <- .workflow_stage_after_review(workflow)
  event <- workflow$state$review_events[[event_count + 1L]]
  workflow$state <- .tag_store_save_focused(
    workflow$store, workflow$state, "save_review", proposal_id,
    event$event_id, event$level, event$cluster_id,
    decision %in% c("accepted", "edited", "rejected")
  )
  .workflow_emit(event_callback, "review_checkpoint", workflow$state,
                 list(proposal_id = proposal_id, decision = decision))
  workflow
}
