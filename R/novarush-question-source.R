# Query and interpret novaGraphDB's taggable-question projection.

.question_sur <- "https://w3id.org/survey-ontology#"
.question_nova <- "https://data.nova.org/vocabulary/survey/"
.question_prov <- "http://www.w3.org/ns/prov#"

.question_property <- function(namespace, term) paste0(namespace, term)

.taggable_question_type_iris <- paste0(.question_sur, c(
  "Question", "OpenQuestion", "SingleInputQuestion",
  "MultipleInputQuestion", "ClosedQuestion", "MultipleChoiceQuestion",
  "CheckboxQuestion"
))

.query_result_value <- function(value, default = NA_character_) {
  if (is.null(value) || !length(value)) return(default)
  if (is.list(value) && "@value" %in% names(value)) value <- value[["@value"]]
  if (is.list(value) && "@id" %in% names(value)) value <- value[["@id"]]
  if (is.null(value) || !length(value)) default else value
}

.source_form_query <- function(limit, offset) {
  list(
    select = list("?sourceForm"),
    where = list(list(
      "@id" = "?sourceForm",
      "@type" = .question_property(.question_nova, "SourceForm")
    )),
    orderBy = list("?sourceForm"), limit = limit, offset = offset
  )
}

.question_query <- function(limit, offset, source_form_id = NULL) {
  source_value <- if (is.null(source_form_id)) {
    "?sourceForm"
  } else {
    list("@id" = source_form_id)
  }
  list(
    select = c(list(
      "?question", "?questionType", "?text", "?class", "?cardinality",
      "?datatype", "?procedure"
    ), if (is.null(source_form_id)) list("?sourceForm") else list(), list(
      "?sourceName", "?order", "?repeatGroup"
    )),
    where = list(
      list(
        "@id" = "?question", "@type" = "?questionType",
        "https://w3id.org/survey-ontology#hasText" = "?text",
        "https://data.nova.org/vocabulary/survey/questionClass" = "?class",
        "https://data.nova.org/vocabulary/survey/responseCardinality" =
          "?cardinality",
        "https://data.nova.org/vocabulary/survey/responseDatatype" = "?datatype",
        "https://w3id.org/survey-ontology#inSurveyProcedure" = "?procedure",
        "http://www.w3.org/ns/prov#wasDerivedFrom" = source_value,
        "https://data.nova.org/vocabulary/survey/sourceElementName" = "?sourceName",
        "https://data.nova.org/vocabulary/survey/elementOrder" = "?order"
      ),
      list("optional", list(
        "@id" = "?question",
        "https://data.nova.org/vocabulary/survey/inRepeatGroup" = "?repeatGroup"
      ))
    ),
    orderBy = list("?question"), limit = limit, offset = offset
  )
}

.answer_option_query <- function(limit, offset, source_form_id = NULL) {
  question_pattern <- list(
    "@id" = "?question",
    "https://w3id.org/survey-ontology#leadsTo" = "?option"
  )
  if (!is.null(source_form_id)) {
    question_pattern[["http://www.w3.org/ns/prov#wasDerivedFrom"]] <-
      list("@id" = source_form_id)
  }
  list(
    select = list(
      "?question", "?option", "?text", "?sourceCode", "?value", "?order"
    ),
    where = list(
      question_pattern,
      list(
        "@id" = "?option",
        "@type" = .question_property(.question_sur, "ClosedAnswer"),
        "https://w3id.org/survey-ontology#hasText" = "?text"
      ),
      list("optional", list(
        "@id" = "?option",
        "https://data.nova.org/vocabulary/survey/sourceResponseCode" =
          "?sourceCode"
      )),
      list("optional", list(
        "@id" = "?option",
        "https://w3id.org/survey-ontology#hasValue" = "?value"
      )),
      list("optional", list(
        "@id" = "?option",
        "https://w3id.org/survey-ontology#hasOrderNumber" = "?order"
      ))
    ),
    orderBy = list("?question", "?order", "?option"),
    limit = limit, offset = offset
  )
}

.query_pages <- function(query_builder, config, graph, branch, page_size, ...) {
  offset <- 0L
  rows <- list()
  repeat {
    page <- .nr_query_named_graph(
      query_builder(page_size, offset, ...), graph, config, branch
    )
    page <- .nr_result_rows(page)
    rows <- c(rows, page)
    if (length(page) < page_size) break
    offset <- offset + page_size
  }
  rows
}

.source_form_rows <- function(rows) {
  if (!length(rows)) return(character())
  unique(vapply(rows, function(row) {
    as.character(.query_result_value(row[[1L]], ""))
  }, character(1)))
}

.question_rows <- function(rows, source_form_id = NULL) {
  if (!length(rows)) return(tibble::tibble(
    id = character(), iri = character(), caption = character(),
    question_type = character(), question_class = character(),
    response_cardinality = character(), response_datatype = character(),
    procedure_id = character(), source_form_id = character(),
    source_element_name = character(), element_order = integer(),
    repeat_group_id = character()
  ))
  value <- function(row, index, default = NA_character_) {
    .query_result_value(if (length(row) >= index) row[[index]] else NULL, default)
  }
  partitioned <- !is.null(source_form_id)
  source_index <- if (partitioned) NA_integer_ else 8L
  source_name_index <- if (partitioned) 8L else 9L
  order_index <- if (partitioned) 9L else 10L
  repeat_index <- if (partitioned) 10L else 11L
  result <- tibble::tibble(
    id = vapply(rows, value, character(1), index = 1L),
    iri = vapply(rows, value, character(1), index = 1L),
    caption = vapply(rows, value, character(1), index = 3L),
    question_type = vapply(rows, value, character(1), index = 2L),
    question_class = vapply(rows, value, character(1), index = 4L),
    response_cardinality = vapply(rows, value, character(1), index = 5L),
    response_datatype = vapply(rows, value, character(1), index = 6L),
    procedure_id = vapply(rows, value, character(1), index = 7L),
    source_form_id = if (partitioned) {
      rep(as.character(source_form_id), length(rows))
    } else {
      vapply(rows, value, character(1), index = source_index)
    },
    source_element_name = vapply(
      rows, value, character(1), index = source_name_index
    ),
    element_order = vapply(rows, function(row) {
      as.integer(value(row, order_index))
    }, integer(1)),
    repeat_group_id = vapply(rows, value, character(1), index = repeat_index)
  )
  result[result$question_type %in% .taggable_question_type_iris, , drop = FALSE]
}

.option_rows <- function(rows) {
  empty <- tibble::tibble(
    question_id = character(), option_id = character(),
    option_text = character(), option_code = character(),
    option_order = integer()
  )
  if (!length(rows)) return(empty)
  value <- function(row, index, default = NA_character_) {
    .query_result_value(if (length(row) >= index) row[[index]] else NULL, default)
  }
  tibble::tibble(
    question_id = vapply(rows, value, character(1), index = 1L),
    option_id = vapply(rows, value, character(1), index = 2L),
    option_text = vapply(rows, value, character(1), index = 3L),
    option_code = vapply(rows, function(row) {
      source <- value(row, 4L)
      as.character(if (is.na(source) || !nzchar(source)) value(row, 5L) else source)
    }, character(1)),
    option_order = vapply(rows, function(row) {
      as.integer(value(row, 6L))
    }, integer(1))
  )
}

.validate_queried_questions <- function(questions) {
  if (!nrow(questions)) return(invisible(questions))
  required <- c(
    "id", "caption", "question_class", "response_cardinality",
    "response_datatype", "procedure_id", "source_form_id",
    "source_element_name"
  )
  invalid <- vapply(required, function(column) {
    anyNA(questions[[column]]) || any(!nzchar(trimws(questions[[column]])))
  }, logical(1))
  if (any(invalid)) {
    stop("Queried questions violate required fields: ",
         paste(required[invalid], collapse = ", "), ".", call. = FALSE)
  }
  if (any(!grepl("^https?://", questions$id, ignore.case = TRUE)) ||
      anyNA(questions$element_order) || any(questions$element_order < 1L)) {
    stop("Queried questions require absolute IRIs and positive element order.",
         call. = FALSE)
  }
  closed_without_options <- questions$question_class == "closed" &
    questions$answer_count < 1L
  open_with_options <- questions$question_class != "closed" &
    questions$answer_count > 0L
  if (any(closed_without_options) || any(open_with_options)) {
    stop("Queried question/answer-option relationships are inconsistent.",
         call. = FALSE)
  }
  invisible(questions)
}

#' Query taggable survey questions through novaRush
#'
#' Retrieves the normalized question projection produced by `novaGraphDB` from
#' one Fluree named graph. The query first discovers source forms, then pages
#' questions and closed answers within each source form. This bounds server
#' query work for large corpora and avoids treating answer options as questions.
#'
#' @param config A Fluree configuration created by [novaRush::setConfig()].
#' @param graph Absolute IRI of the survey knowledge named graph.
#' @param branch Fluree branch. Defaults to `config$branch`.
#' @param procedure_id Optional survey-procedure IRI to retain.
#' @param source_form_id Optional source-form IRI to retain.
#' @param page_size Maximum rows requested per Fluree query.
#'
#' @return A tibble accepted by novaTagger workflows, including `id`, `iri`,
#'   `caption`, survey provenance, response semantics, and an `answer_options`
#'   list-column.
#' @export
#' @importFrom novaRush queryNamedGraph
query_taggable_questions <- function(
    config, graph, branch = config$branch, procedure_id = NULL,
    source_form_id = NULL, page_size = 500L) {
  .nr_validate_graphs(list(
    run = graph, embedding = graph, hierarchy = graph, review = graph
  ))
  page_size <- suppressWarnings(as.integer(page_size))
  if (length(page_size) != 1L || is.na(page_size) || page_size < 1L) {
    stop("`page_size` must be one positive integer.", call. = FALSE)
  }
  source_forms <- if (is.null(source_form_id)) {
    .source_form_rows(.query_pages(
      .source_form_query, config, graph, branch, page_size
    ))
  } else {
    unique(as.character(source_form_id))
  }
  source_forms <- source_forms[!is.na(source_forms) & nzchar(source_forms)]
  question_partitions <- lapply(source_forms, function(source_id) {
    .query_pages(
      .question_query, config, graph, branch, page_size,
      source_form_id = source_id
    )
  })
  option_partitions <- lapply(source_forms, function(source_id) {
    .query_pages(
      .answer_option_query, config, graph, branch, page_size,
      source_form_id = source_id
    )
  })
  questions <- do.call(rbind, Map(
    .question_rows, question_partitions, source_forms
  ))
  if (is.null(questions)) questions <- .question_rows(list())
  questions <- tibble::as_tibble(questions)
  option_rows <- unlist(option_partitions, recursive = FALSE)
  options <- .option_rows(option_rows)
  if (!is.null(procedure_id)) {
    questions <- questions[questions$procedure_id %in% as.character(procedure_id), ]
  }
  options <- options[options$question_id %in% questions$id, , drop = FALSE]
  questions$answer_options <- lapply(questions$id, function(id) {
    selected <- options[options$question_id == id, c(
      "option_id", "option_text", "option_code", "option_order"
    ), drop = FALSE]
    selected[order(selected$option_order, selected$option_id), , drop = FALSE]
  })
  questions$answer_count <- vapply(
    questions$answer_options, nrow, integer(1)
  )
  questions <- questions[order(
    questions$procedure_id, questions$element_order, questions$id
  ), , drop = FALSE]
  .validate_queried_questions(questions)
  tibble::as_tibble(questions)
}
