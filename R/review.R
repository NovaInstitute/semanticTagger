.tag_proposal_id <- function(run_id, level, cluster_id) {
  paste(
    as.character(run_id),
    as.integer(level),
    as.character(cluster_id),
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    sprintf("%06d", sample.int(999999L, 1L)),
    sep = ":"
  )
}

#' Register an AI tag proposal without accepting it
#'
#' @param state A `tag_state`.
#' @param level,cluster_id Cluster identity.
#' @param tag Proposed label.
#' @param confidence Optional numeric confidence.
#' @param rationale Model rationale.
#' @param needs_review Logical review flag.
#' @param provider,model Provider and model provenance.
#' @param embedding_model Embedding model used for `tag_embedding`.
#' @param tag_embedding Optional embedding for the proposed label.
#' @param evidence,prompt,raw_response Proposal evidence and generation trace.
#'
#' @return Updated `tag_state`.
#' @export
register_tag_proposal <- function(state, level, cluster_id, tag,
                                  confidence = NA_real_, rationale = "",
                                  needs_review = TRUE, provider = NA_character_,
                                  model = NA_character_,
                                  embedding_model = NA_character_,
                                  tag_embedding = NULL,
                                  evidence = NULL, prompt = NULL,
                                  raw_response = NULL) {
  state <- validate_tag_state(state)
  level <- as.integer(level)
  hit <- which(
    state$clusters$level == level &
      as.character(state$clusters$cluster_id) == as.character(cluster_id)
  )
  if (length(hit) != 1L) stop("Proposal cluster was not found.", call. = FALSE)
  tag <- sanitize_label(as.character(tag))
  if (length(tag) != 1L || is.na(tag) || !nzchar(tag) || identical(tag, "untagged")) {
    stop("A proposal requires a non-empty tag.", call. = FALSE)
  }
  proposal_id <- .tag_proposal_id(state$run_id, level, cluster_id)
  proposal <- list(
    proposal_id = proposal_id,
    run_id = state$run_id,
    level = level,
    cluster_id = as.character(cluster_id),
    tag = tag,
    confidence = as.numeric(confidence),
    rationale = as.character(rationale),
    needs_review = isTRUE(needs_review),
    status = "proposed",
    provider = as.character(provider),
    model = as.character(model),
    embedding_model = as.character(embedding_model),
    tag_embedding = if (is.null(tag_embedding)) NULL else as.numeric(tag_embedding),
    evidence = evidence,
    prompt = prompt,
    raw_response = raw_response,
    created_at = Sys.time()
  )
  state$proposals[[proposal_id]] <- proposal
  state
}

#' Apply an immutable reviewer decision to a tag proposal
#'
#' @param state A `tag_state`.
#' @param proposal_id Proposal identifier.
#' @param decision One of `accepted`, `edited`, `rejected`, or `deferred`.
#' @param reviewer_id Stable reviewer identifier.
#' @param rationale Optional reviewer explanation.
#' @param tag Edited label when `decision = "edited"`.
#' @param embed_tag Function accepting a label and returning its embedding.
#' @param low_similarity Threshold used in similarity summaries.
#'
#' @return Updated `tag_state`.
#' @export
review_tag_proposal <- function(state, proposal_id,
                                decision = c("accepted", "edited", "rejected", "deferred"),
                                reviewer_id, rationale = "", tag = NULL,
                                embed_tag = NULL, low_similarity = 0.5) {
  state <- validate_tag_state(state)
  decision <- match.arg(decision)
  proposal <- state$proposals[[proposal_id]] %||% NULL
  if (is.null(proposal)) stop("Tag proposal was not found.", call. = FALSE)
  if (!proposal$status %in% c("proposed", "deferred")) {
    stop("Only proposed or deferred tags can receive a new decision.", call. = FALSE)
  }
  reviewer_id <- trimws(as.character(reviewer_id))
  if (length(reviewer_id) != 1L || is.na(reviewer_id) || !nzchar(reviewer_id)) {
    stop("A non-empty reviewer ID is required.", call. = FALSE)
  }

  original_tag <- proposal$tag
  original_embedding <- proposal$tag_embedding
  resulting_tag <- original_tag
  resulting_embedding <- original_embedding
  similarity <- NULL

  if (identical(decision, "edited")) {
    resulting_tag <- sanitize_label(as.character(tag %||% ""))
    if (length(resulting_tag) != 1L || is.na(resulting_tag) || !nzchar(resulting_tag)) {
      stop("An edited decision requires a non-empty tag.", call. = FALSE)
    }
    if (!is.function(embed_tag)) {
      stop("An edited decision requires an `embed_tag` function.", call. = FALSE)
    }
    resulting_embedding <- as.numeric(embed_tag(resulting_tag))
    after <- score_cluster_tag_similarity(
      state, proposal$level, proposal$cluster_id, resulting_embedding, low_similarity
    )
    if (!is.null(original_embedding)) {
      before <- score_cluster_tag_similarity(
        state, proposal$level, proposal$cluster_id, original_embedding, low_similarity
      )
      similarity <- compare_tag_similarity(before, after)
    } else {
      similarity <- list(questions = after, before = NULL, after = list(
        mean = mean(after$cosine_similarity, na.rm = TRUE),
        minimum = min(after$cosine_similarity, na.rm = TRUE),
        low_count = sum(after$low_similarity)
      ), mean_change = NA_real_)
    }
  } else if (!is.null(original_embedding)) {
    scores <- score_cluster_tag_similarity(
      state, proposal$level, proposal$cluster_id, original_embedding, low_similarity
    )
    summary <- list(
      mean = mean(scores$cosine_similarity, na.rm = TRUE),
      minimum = min(scores$cosine_similarity, na.rm = TRUE),
      low_count = sum(scores$low_similarity)
    )
    similarity <- list(questions = scores, before = summary, after = summary, mean_change = 0)
  }

  event_id <- paste0(
    proposal_id, ":review:",
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), ":",
    sprintf("%06d", sample.int(999999L, 1L))
  )
  event <- list(
    event_id = event_id,
    proposal_id = proposal_id,
    run_id = state$run_id,
    level = proposal$level,
    cluster_id = proposal$cluster_id,
    decision = decision,
    reviewer_id = reviewer_id,
    rationale = as.character(rationale),
    original_tag = original_tag,
    resulting_tag = if (decision == "rejected") NA_character_ else resulting_tag,
    similarity = similarity,
    created_at = Sys.time()
  )
  state$review_events[[length(state$review_events) + 1L]] <- event
  proposal$status <- decision
  proposal$tag <- resulting_tag
  proposal$tag_embedding <- resulting_embedding
  proposal$reviewed_at <- event$created_at
  state$proposals[[proposal_id]] <- proposal

  cluster_hit <- which(
    state$clusters$level == proposal$level &
      as.character(state$clusters$cluster_id) == proposal$cluster_id
  )
  if (decision %in% c("accepted", "edited")) {
    state$clusters$tag[[cluster_hit]] <- resulting_tag
  } else if (identical(decision, "rejected")) {
    state$clusters$tag[[cluster_hit]] <- NA_character_
  }
  state
}
