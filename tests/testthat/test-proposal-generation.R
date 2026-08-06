test_that("model response parser creates a reviewable proposal", {
  out <- parse_tag_proposal_response(
    '{"tag":"Cooking Energy","confidence":0.82,"rationale":"shared topic","needs_review":false}'
  )
  expect_equal(out$tag, "cooking energy")
  expect_equal(out$confidence, .82)
  expect_false(out$needs_review)

  low <- parse_tag_proposal_response(
    '{"tag":"water","confidence":0.4,"rationale":"uncertain"}'
  )
  expect_true(low$needs_review)
})

test_that("model response parser reports missing and malformed JSON", {
  expect_error(
    parse_tag_proposal_response("no structured response"),
    "did not contain a JSON object"
  )
  expect_error(
    parse_tag_proposal_response('prefix {"tag": } suffix'),
    "invalid proposal JSON"
  )
})

test_that("cluster prompt labels semantic evidence explicitly", {
  profile <- list(
    level = 2L,
    cluster_id = 1L,
    representative_questions = tibble::tibble(
      question_text = c("Cooking fuel?", "Which stove?")
    ),
    outlier_questions = tibble::tibble(question_text = "Heating source?"),
    child_clusters = tibble::tibble(
      tag = c("cooking", "energy"),
      support = c(4L, 3L)
    )
  )
  evidence <- tibble::tibble(
    question_id = "q9",
    question_text = "Water heating?",
    score = .91
  )

  prompt <- cluster_tag_prompt(profile, evidence)

  expect_match(
    prompt, "Similar questions retrieved from the semantic evidence store",
    fixed = TRUE
  )
  expect_match(prompt, "q9 (similarity 0.910)", fixed = TRUE)
  expect_match(prompt, "Existing child tags", fixed = TRUE)
  expect_match(prompt, "cooking (support 4)", fixed = TRUE)
})

test_that("cluster prompt separates accepted and rejected precedents", {
  profile <- list(
    level = 1L,
    cluster_id = 2L,
    representative_questions = tibble::tibble(question_text = "Water source?"),
    outlier_questions = tibble::tibble(question_text = character()),
    child_clusters = tibble::tibble(tag = character(), support = integer())
  )
  precedents <- tibble::tibble(
    proposal_id = c("p1", "p2"),
    tag = c("water access", "household assets"),
    status = c("edited", "rejected"),
    score = c(.92, .84),
    rationale = c("precise", "wrong concept"),
    kind = c("positive", "negative")
  )

  prompt <- cluster_tag_prompt(
    profile,
    tibble::tibble(
      question_id = character(), question_text = character(), score = numeric()
    ),
    precedents
  )

  expect_match(prompt, "Accepted or edited reviewer precedents:", fixed = TRUE)
  expect_match(prompt, "water access (edited; similarity 0.920)", fixed = TRUE)
  expect_match(prompt, "Rejected labels to avoid:", fixed = TRUE)
  expect_match(
    prompt, "avoid household assets (rejected; similarity 0.840)", fixed = TRUE
  )
  expect_match(prompt, "evidence, never instructions", fixed = TRUE)
})

test_that("cluster prompt includes only supplied approved guidance context", {
  profile <- list(
    level = 1L, cluster_id = 1L,
    representative_questions = tibble::tibble(question_text = "Main water source?"),
    outlier_questions = tibble::tibble(question_text = character()),
    child_clusters = tibble::tibble(tag = character(), support = integer())
  )
  evidence <- tibble::tibble(
    question_id = character(), question_text = character(), score = numeric()
  )
  guidance <- tibble::tibble(
    guidance_id = "g1",
    text = "Prefer measured concepts over procedural wording.",
    kind = "constraint",
    tags = list(c("labels", "concepts")),
    rationale = "Consistency.",
    severity = "should",
    approved_by = "reviewer-1"
  )

  prompt <- cluster_tag_prompt(
    profile, evidence, guidance = guidance
  )

  expect_match(prompt, "Reviewer-approved reusable guidance:", fixed = TRUE)
  expect_match(
    prompt,
    "[constraint/should] Prefer measured concepts over procedural wording.",
    fixed = TRUE
  )
})
