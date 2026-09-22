test_that("live question source retrieves novaGraphDB-shaped knowledge", {
  testthat::skip_if_not(
    live_tagger_enabled(),
    "Set FLUREE_LIVE_TEST=true to run against a live Fluree server."
  )
  stamp <- gsub("[^A-Za-z0-9]", "", format(
    Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"
  ))
  config <- novaRush::setConfig(
    baseUrl = Sys.getenv("FLUREE_BASE_URL", "http://localhost:8090"),
    ledger = Sys.getenv("FLUREE_TEST_LEDGER", "novatagger-integration"),
    branch = Sys.getenv("FLUREE_TEST_BRANCH", "main"),
    timeout = as.numeric(Sys.getenv("FLUREE_REQUEST_TIMEOUT", "60"))
  )
  novaRush::createLedger(config)
  base <- paste0("https://data.nova.org/test/question-source/", stamp, "/")
  graph <- paste0(base, "graph")
  procedure <- paste0(base, "procedure")
  form <- paste0(base, "form")
  question <- paste0(base, "question/consent")
  answer <- paste0(base, "answer/yes")
  novaRush::upsertNamedGraph(list(
    list(
      "@id" = question,
      "@type" = "https://w3id.org/survey-ontology#MultipleChoiceQuestion",
      "https://w3id.org/survey-ontology#hasText" = "Do you consent?",
      "https://w3id.org/survey-ontology#inSurveyProcedure" =
        list("@id" = procedure),
      "http://www.w3.org/ns/prov#wasDerivedFrom" = list("@id" = form),
      "https://data.nova.org/vocabulary/survey/questionClass" = "closed",
      "https://data.nova.org/vocabulary/survey/responseCardinality" = "single",
      "https://data.nova.org/vocabulary/survey/responseDatatype" = "categorical",
      "https://data.nova.org/vocabulary/survey/sourceElementName" = "consent",
      "https://data.nova.org/vocabulary/survey/elementOrder" = 1L,
      "https://w3id.org/survey-ontology#leadsTo" = list("@id" = answer)
    ),
    list(
      "@id" = answer,
      "@type" = "https://w3id.org/survey-ontology#ClosedAnswer",
      "https://w3id.org/survey-ontology#hasText" = "Yes",
      "https://data.nova.org/vocabulary/survey/sourceResponseCode" = "yes",
      "https://w3id.org/survey-ontology#hasOrderNumber" = 1L
    )
  ), graph, config)

  result <- query_taggable_questions(config, graph, page_size = 1L)
  expect_equal(nrow(result), 1L)
  expect_equal(result$id, question)
  expect_equal(result$caption, "Do you consent?")
  expect_equal(result$answer_options[[1]]$option_code, "yes")
})
