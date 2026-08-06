test_that("tagging guidance requires explicit reviewer approval", {
  candidate <- new_tagging_guidance(
    "Prefer labels that describe the measured concept.",
    kind = "constraint",
    tags = c("labels", "review"),
    rationale = "Avoid procedural labels.",
    severity = "should",
    guidance_id = "guidance-1"
  )

  expect_equal(candidate$status, "candidate")
  approved <- approve_tagging_guidance(candidate, "reviewer-1")
  expect_equal(approved$status, "approved")
  expect_equal(approved$approved_by, "reviewer-1")
  expect_s3_class(approved$approved_at, "POSIXct")
  expect_equal(approved$revision, 1L)
  expect_equal(approved$review_events[[1]]$decision, "approve")
  expect_equal(candidate$status, "candidate")
  expect_error(approve_tagging_guidance(approved, "reviewer-1"), "invalid")
})

test_that("tagging guidance validation rejects ambiguous records", {
  expect_error(
    new_tagging_guidance("", tags = "labels"),
    "non-empty"
  )
  expect_error(
    new_tagging_guidance("Use precise labels.", tags = character()),
    "at least one tag"
  )
  expect_error(
    new_tagging_guidance(
      "A domain fact.", kind = "fact", tags = "domain", severity = "must"
    ),
    "only valid for constraint"
  )
  expect_error(
    approve_tagging_guidance(
      new_tagging_guidance("Use precise labels.", tags = "labels"), ""
    ),
    "reviewer ID"
  )
})

test_that("guidance rejection and retirement require reasons", {
  candidate <- new_tagging_guidance(
    "Always use broad labels.", tags = "labels", guidance_id = "guidance-2"
  )
  expect_error(
    review_tagging_guidance(candidate, "reject", "reviewer-1"),
    "requires a rationale"
  )
  rejected <- review_tagging_guidance(
    candidate, "reject", "reviewer-1", "This loses useful specificity."
  )
  expect_equal(rejected$status, "rejected")
  expect_equal(rejected$review_events[[1]]$status_before, "candidate")

  approved <- approve_tagging_guidance(candidate, "reviewer-1")
  expect_error(
    review_tagging_guidance(approved, "retire", "reviewer-1"),
    "requires a rationale"
  )
  retired <- review_tagging_guidance(
    approved, "retire", "reviewer-2", "Ontology has changed."
  )
  expect_equal(retired$status, "retired")
  expect_equal(retired$revision, 2L)
  expect_length(retired$review_events, 2L)
})

test_that("supersession retains history and creates an unapproved replacement", {
  approved <- approve_tagging_guidance(
    new_tagging_guidance(
      "Use broad health labels.", tags = "health", guidance_id = "guidance-3"
    ),
    "reviewer-1"
  )
  result <- supersede_tagging_guidance(
    approved,
    "Use the most specific measured health concept.",
    "reviewer-2",
    "Specific concepts improve inference."
  )

  expect_equal(result$guidance$status, "superseded")
  expect_equal(
    result$guidance$superseded_by, result$replacement$guidance_id
  )
  expect_equal(result$replacement$status, "candidate")
  expect_equal(
    result$guidance$review_events[[2]]$decision, "supersede"
  )
})

test_that("guidance reviews reject stale local revisions", {
  candidate <- new_tagging_guidance("Use precise labels.", tags = "labels")
  expect_error(
    review_tagging_guidance(
      candidate, "approve", "reviewer-1", expected_revision = 9L
    ),
    "changed since it was loaded"
  )
})
