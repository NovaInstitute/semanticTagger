#' Create a tagging-state store
#'
#' A store isolates tagging workflows from the persistence technology. A
#' Fluree-backed implementation can satisfy this contract without changing the
#' clustering or tagging algorithms.
#'
#' @param exists Function with no arguments that returns whether state exists.
#' @param load Function with no arguments that returns a `tag_state`.
#' @param save Function accepting a `tag_state`.
#' @param label Short implementation label used in messages and diagnostics.
#' @param metadata Optional implementation metadata for diagnostics.
#'
#' @return A `tag_store` object.
#' @export
new_tag_store <- function(exists, load, save, label = "custom", metadata = list(),
                          save_proposal = NULL, save_review = NULL) {
  functions <- list(exists = exists, load = load, save = save)
  if (!all(vapply(functions, is.function, logical(1)))) {
    stop("`exists`, `load`, and `save` must be functions.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }
  optional <- list(save_proposal = save_proposal, save_review = save_review)
  if (!all(vapply(optional, function(x) is.null(x) || is.function(x), logical(1)))) {
    stop("Focused save callbacks must be NULL or functions.", call. = FALSE)
  }
  structure(
    c(functions, optional,
      list(label = as.character(label)[[1]], metadata = metadata)),
    class = "tag_store"
  )
}

.tag_store_save_focused <- function(store, state, callback, ...) {
  validate_tag_store(store)
  state <- validate_tag_state(state)
  current <- if (tag_store_exists(store)) tag_store_load(store) else NULL
  if (!is.null(current) && !identical(current$run_id, state$run_id)) {
    stop("Tag store contains a different run ID.", call. = FALSE)
  }
  if (!is.null(current) &&
      !identical(as.integer(current$revision), as.integer(state$revision))) {
    stop("Cannot save stale tagging state: stored revision is ",
         current$revision, " but supplied revision is ", state$revision, ".",
         call. = FALSE)
  }
  state$revision <- state$revision + 1L
  state$updated_at <- Sys.time()
  focused <- store[[callback]]
  if (is.function(focused)) focused(state, ...) else store$save(state)
  invisible(state)
}

#' Create an in-memory tagging-state store
#'
#' @param state Optional initial `tag_state`.
#'
#' @return A `tag_store`. Its contents persist for the lifetime of the object.
#' @export
memory_tag_store <- function(state = NULL) {
  storage <- new.env(parent = emptyenv())
  storage$state <- if (is.null(state)) NULL else validate_tag_state(state)

  new_tag_store(
    exists = function() !is.null(storage$state),
    load = function() storage$state,
    save = function(value) {
      storage$state <- value
      invisible(value)
    },
    label = "memory"
  )
}

#' Test whether a tagging store contains state
#'
#' @param store A `tag_store`.
#' @return A logical scalar.
#' @export
tag_store_exists <- function(store) {
  validate_tag_store(store)
  value <- store$exists()
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("A tag store's `exists` function must return one logical value.", call. = FALSE)
  }
  value
}

#' Load state from a tagging store
#'
#' @param store A `tag_store`.
#' @return A validated `tag_state`.
#' @export
tag_store_load <- function(store) {
  validate_tag_store(store)
  if (!tag_store_exists(store)) {
    stop("The tagging store does not contain state.", call. = FALSE)
  }
  validate_tag_state(store$load())
}

#' Save state to a tagging store
#'
#' Each save advances the state revision and update timestamp.
#'
#' @param store A `tag_store`.
#' @param state A `tag_state`.
#' @return The saved state, invisibly.
#' @export
tag_store_save <- function(store, state) {
  validate_tag_store(store)
  state <- validate_tag_state(state)
  if (tag_store_exists(store)) {
    current <- validate_tag_state(store$load())
    if (!identical(current$run_id, state$run_id)) {
      stop("Tag store contains a different run ID.", call. = FALSE)
    }
    if (!identical(as.integer(current$revision), as.integer(state$revision))) {
      stop(
        "Cannot save stale tagging state: stored revision is ",
        current$revision, " but supplied revision is ", state$revision, ".",
        call. = FALSE
      )
    }
  }
  state$revision <- state$revision + 1L
  state$updated_at <- Sys.time()
  store$save(state)
  invisible(state)
}

validate_tag_store <- function(store) {
  if (!inherits(store, "tag_store")) {
    stop("`store` must be a tag_store.", call. = FALSE)
  }
  invisible(store)
}
