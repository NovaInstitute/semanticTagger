question_query_rows_fixture <- function() {
  sur <- "https://w3id.org/survey-ontology#"
  list(
    list(
      "https://example.org/question/open", paste0(sur, "SingleInputQuestion"),
      "Describe your household", "open", "single", "text",
      "https://example.org/procedure/one", "https://example.org/form/one",
      "household_description", 1L, NA_character_
    ),
    list(
      "https://example.org/question/closed", paste0(sur, "MultipleChoiceQuestion"),
      "Do you consent?", "closed", "single", "categorical",
      "https://example.org/procedure/one", "https://example.org/form/one",
      "consent", 2L, "https://example.org/repeat/member"
    ),
    list(
      "https://example.org/talk/introduction", paste0(sur, "Talk"),
      "Introduction", "open", "single", "text",
      "https://example.org/procedure/one", "https://example.org/form/one",
      "intro", 3L, NA_character_
    )
  )
}

option_query_rows_fixture <- function() {
  list(
    list("https://example.org/question/closed",
         "https://example.org/answer/yes", "Yes", "yes", NA, 1L),
    list("https://example.org/question/closed",
         "https://example.org/answer/no", "No", NA, 0, 2L)
  )
}

partition_question_rows_fixture <- function(source_form_id) {
  rows <- Filter(function(row) identical(row[[8L]], source_form_id),
                 question_query_rows_fixture())
  lapply(rows, function(row) row[-8L])
}

query_source_constraint <- function(query) {
  pattern <- query$where[[1L]]
  value <- pattern[["http://www.w3.org/ns/prov#wasDerivedFrom"]]
  if (is.list(value)) as.character(value[["@id"]]) else NULL
}

test_that("taggable question query paginates and preserves closed options", {
  question_rows <- question_query_rows_fixture()
  option_rows <- option_query_rows_fixture()
  calls <- list()
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(query, graph, config, branch) {
      calls[[length(calls) + 1L]] <<- query
      rows <- if (length(query$select) == 1L) {
        list(list("https://example.org/form/one"))
      } else if (length(query$select) == 10L) {
        partition_question_rows_fixture(query_source_constraint(query))
      } else {
        option_rows
      }
      start <- query$offset + 1L
      if (start > length(rows)) return(list())
      rows[start:min(length(rows), query$offset + query$limit)]
    },
    .package = "novaTagger"
  )
  result <- query_taggable_questions(
    list(branch = "main"), "https://example.org/graph/survey", page_size = 2L
  )
  expect_equal(nrow(result), 2L)
  expect_equal(result$id, c(
    "https://example.org/question/open",
    "https://example.org/question/closed"
  ))
  expect_false(any(grepl("/answer/", result$caption)))
  closed <- result[result$question_class == "closed", ]
  expect_equal(closed$answer_count, 2L)
  expect_equal(closed$answer_options[[1]]$option_text, c("Yes", "No"))
  expect_equal(closed$answer_options[[1]]$option_code, c("yes", "0"))
  expect_true(length(calls) >= 4L)
  expect_true(all(vapply(calls, function(query) query$limit == 2L, logical(1))))
})

test_that("question source supports procedure and source-form filtering", {
  extra <- question_query_rows_fixture()[[1L]]
  extra[[1L]] <- "https://example.org/question/other"
  extra[[7L]] <- "https://example.org/procedure/two"
  extra[[8L]] <- "https://example.org/form/two"
  rows <- c(question_query_rows_fixture()[1:2], list(extra))
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(query, ...) {
      source <- query_source_constraint(query)
      if (length(query$select) == 10L) {
        selected <- Filter(function(row) identical(row[[8L]], source), rows)
        return(lapply(selected, function(row) row[-8L]))
      }
      if (identical(source, "https://example.org/form/one")) {
        option_query_rows_fixture()
      } else list()
    },
    .package = "novaTagger"
  )
  result <- query_taggable_questions(
    list(branch = "main"), "https://example.org/graph/survey",
    procedure_id = "https://example.org/procedure/one",
    source_form_id = "https://example.org/form/one"
  )
  expect_equal(nrow(result), 2L)
  expect_true(all(result$procedure_id == "https://example.org/procedure/one"))
  expect_true(all(result$source_form_id == "https://example.org/form/one"))
})

test_that("question source discovers and combines source-form partitions", {
  form_two <- question_query_rows_fixture()[[1L]]
  form_two[[1L]] <- "https://example.org/question/other"
  form_two[[7L]] <- "https://example.org/procedure/two"
  form_two[[8L]] <- "https://example.org/form/two"
  rows <- c(question_query_rows_fixture(), list(form_two))
  queried_sources <- character()
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(query, ...) {
      if (length(query$select) == 1L) {
        return(list(
          list("https://example.org/form/one"),
          list("https://example.org/form/two")
        ))
      }
      source <- query_source_constraint(query)
      queried_sources <<- c(queried_sources, source)
      if (length(query$select) == 10L) {
        selected <- Filter(function(row) identical(row[[8L]], source), rows)
        return(lapply(selected, function(row) row[-8L]))
      }
      if (identical(source, "https://example.org/form/one")) {
        option_query_rows_fixture()
      } else list()
    },
    .package = "novaTagger"
  )
  result <- query_taggable_questions(
    list(branch = "main"), "https://example.org/graph/survey"
  )
  expect_equal(nrow(result), 3L)
  expect_setequal(unique(result$source_form_id), c(
    "https://example.org/form/one", "https://example.org/form/two"
  ))
  expect_true(all(c(
    "https://example.org/form/one", "https://example.org/form/two"
  ) %in% queried_sources))
  expect_equal(result$answer_count[result$id ==
    "https://example.org/question/closed"], 2L)
})

test_that("question source rejects malformed producer data", {
  rows <- question_query_rows_fixture()[1:2]
  rows[[2L]][[3L]] <- ""
  testthat::local_mocked_bindings(
    .nr_query_named_graph = function(query, ...) {
      if (length(query$select) == 1L) {
        return(list(list("https://example.org/form/one")))
      }
      if (length(query$select) == 10L) {
        return(lapply(rows, function(row) row[-8L]))
      }
      option_query_rows_fixture()
    },
    .package = "novaTagger"
  )
  expect_error(
    query_taggable_questions(
      list(branch = "main"), "https://example.org/graph/survey"
    ),
    "required fields"
  )
})

test_that("question source validates graph and page size", {
  expect_error(
    query_taggable_questions(list(branch = "main"), "relative"),
    "absolute HTTP"
  )
  expect_error(
    query_taggable_questions(
      list(branch = "main"), "https://example.org/graph", page_size = 0L
    ),
    "positive integer"
  )
})
