# Tag labels, matrices, and hierarchy-path helpers.

sanitize_label <- function(raw) {
  if (is.null(raw) || length(raw) == 0L || is.na(raw[[1]])) {
    return(NA_character_)
  }
  cleaned <- stringr::str_squish(tolower(as.character(raw[[1]])))
  cleaned <- stringr::str_replace_all(cleaned, "[_-]+", " ")
  words <- unlist(stringr::str_extract_all(cleaned, "[a-z]+"), use.names = FALSE)
  words <- words[nzchar(words)]
  if (length(words) == 0L) return(NA_character_)
  paste(utils::head(words, 3L), collapse = " ")
}

#' Build a question tag matrix from cluster tags
#'
#' @param assignments Question-to-cluster assignments.
#' @param clusters Cluster tags with level and cluster ID.
#' @return Tag matrix with `tag_level_*` columns.
#' @export
build_question_tag_matrix <- function(assignments, clusters) {
  tag_lookup <- clusters |>
    dplyr::select("level", "cluster_id", "tag")

  assignments |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cluster_level_"),
      names_to = "level_name",
      values_to = "cluster_id"
    ) |>
    dplyr::mutate(
      level = as.integer(stringr::str_remove(
        .data$level_name, "cluster_level_"
      ))
    ) |>
    dplyr::left_join(tag_lookup, by = c("level", "cluster_id")) |>
    dplyr::select("id", "caption", "level", "tag") |>
    dplyr::mutate(level_name = paste0("tag_level_", .data$level)) |>
    dplyr::select(-"level") |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = "level_name", values_from = "tag") |>
    dplyr::arrange(.data$id)
}

cluster_level_cols <- function(assignments) {
  columns <- grep(
    "^cluster_level_[0-9]+$", names(assignments), value = TRUE
  )
  columns[order(as.integer(sub("^cluster_level_", "", columns)))]
}

get_cluster_tag <- function(clusters, level, cluster_id) {
  hit <- clusters$tag[
    clusters$level == level & clusters$cluster_id == cluster_id
  ]
  hit <- hit[!is.na(hit) & nzchar(hit)]
  if (!length(hit)) return("untagged")
  hit[[1]]
}

question_tag_path <- function(q_id, assignments, clusters, upto_level) {
  row_idx <- match(q_id, assignments$id)
  if (is.na(row_idx)) return(character())
  level_columns <- cluster_level_cols(assignments)
  upto_level <- min(upto_level, length(level_columns))
  if (upto_level < 1L) return(character())
  path <- character()
  for (level in seq_len(upto_level)) {
    cluster_id <- assignments[[level_columns[[level]]]][row_idx]
    if (is.na(cluster_id) || !nzchar(as.character(cluster_id))) {
      path <- c(path, "untagged")
    } else {
      path <- c(path, get_cluster_tag(clusters, level, cluster_id))
    }
  }
  path
}

format_question_with_path <- function(q_id, questions, assignments, clusters,
                                      upto_level) {
  caption <- questions$caption[match(q_id, questions$id)]
  caption <- caption[!is.na(caption)]
  if (!length(caption)) caption <- ""
  path <- question_tag_path(
    q_id, assignments, clusters, upto_level = upto_level
  )
  if (!length(path)) return(caption[[1]])
  paste0(caption[[1]], "  {path: ", paste(path, collapse = " > "), "}")
}
