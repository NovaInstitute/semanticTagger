test_that("memory store saves, versions, and resumes tagging state", {
  store <- memory_tag_store()
  state <- new_tag_state(
    tibble::tibble(id = "q_001", caption = "Age?"),
    run_id = "run_test"
  )

  expect_false(tag_store_exists(store))

  saved <- tag_store_save(store, state)
  expect_true(tag_store_exists(store))
  expect_equal(saved$revision, 1L)
  expect_equal(tag_store_load(store)$run_id, "run_test")

  saved$status <- "review"
  saved <- tag_store_save(store, saved)
  expect_equal(saved$revision, 2L)
  expect_equal(tag_store_load(store)$status, "review")
})

test_that("tag stores reject stale revisions", {
  store <- memory_tag_store()
  original <- new_tag_state(
    tibble::tibble(id = "q1", caption = "Age?"),
    run_id = "stale-run"
  )
  saved <- tag_store_save(store, original)
  newer <- tag_store_save(store, saved)

  expect_equal(newer$revision, 2L)
  expect_error(tag_store_save(store, saved), "stale tagging state")
})

test_that("tag state validation rejects malformed domain state", {
  expect_error(validate_tag_state(list()), "questions")
  expect_error(
    validate_tag_state(list(questions = tibble::tibble(id = "q1"))),
    "questions"
  )

  state <- new_tag_state(tibble::tibble(id = "q1", caption = "Age?"))
  state$status <- "unknown"
  expect_error(validate_tag_state(state), "unsupported")
})

test_that("custom stores must obey the contract", {
  invalid_exists <- new_tag_store(
    exists = function() "yes",
    load = function() NULL,
    save = function(state) NULL
  )
  expect_error(tag_store_exists(invalid_exists), "logical")
  expect_error(tag_store_load(memory_tag_store()), "does not contain")
})
