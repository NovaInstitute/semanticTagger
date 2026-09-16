memory_semantic_repository <- function() {
  storage <- new.env(parent = emptyenv())
  storage$partitions <- list(
    run = list(), embedding = list(), hierarchy = list(), review = list()
  )
  merge_nodes <- function(existing, incoming) {
    for (node in incoming) {
      ids <- vapply(existing, function(value) value[["@id"]], character(1))
      hit <- match(node[["@id"]], ids)
      if (is.na(hit)) existing[[length(existing) + 1L]] <- node else
        existing[[hit]] <- node
    }
    existing
  }
  list(
    exists = function(scope_iri) length(storage$partitions$run) > 0L,
    load = function(scope_iri) storage$partitions,
    save = function(records, scope_iri, revision) {
      for (partition in names(storage$partitions)) {
        storage$partitions[[partition]] <- merge_nodes(
          storage$partitions[[partition]], records[[partition]]
        )
      }
      invisible(TRUE)
    },
    metadata = list(backend = "fixture"),
    storage = storage
  )
}
