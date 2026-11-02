review_tree_fixture <- function() {
  list(
    nodes = data.frame(
      id = 1:4,
      parent = c(NA_integer_,1L,1L,1L),
      label = c('Questions','Health','Housing','New empty cluster'),
      stable_id = paste0('node-',1:4)
    ),
    questions = data.frame(id=c('q1','q2'),text=c('Question one','Question two'),leaf=c(2L,3L)),
    reviewed=integer(),deferred=integer(),affected=character(),resolved=character(),unresolved=character(),
    placements=data.frame(id=character(),decision=character()),new_count=0L,next_node_id=4L,
    history=data.frame(action=character(),detail=character()),editor_undo=list(),editor_redo=list()
  )
}

test_that('individual questions can move to an existing empty leaf and be undone', {
  state <- review_tree_fixture()
  moved <- edit_review_tree(state,list(action='move_question',question_id='q1',destination=4L))

  expect_equal(moved$questions$leaf[moved$questions$id=='q1'],4L)
  expect_true('q1' %in% moved$resolved)
  expect_equal(moved$placements$decision[moved$placements$id=='q1'],'moved')
  expect_match(tail(moved$history$detail,1),'Health.*New empty cluster')

  restored <- edit_review_tree(moved,list(action='undo'))
  expect_equal(restored$questions$leaf[restored$questions$id=='q1'],2L)
})

test_that('question moves reject missing questions, branches and unchanged destinations', {
  state <- review_tree_fixture()
  state$nodes <- rbind(state$nodes,data.frame(id=5L,parent=2L,label='Health leaf',stable_id='node-5'))

  expect_error(edit_review_tree(state,list(action='move_question',question_id='missing',destination=4L)),
    'no longer available')
  expect_error(edit_review_tree(state,list(action='move_question',question_id='q1',destination=2L)),
    'leaf category')
  state$questions$leaf[state$questions$id=='q1'] <- 5L
  expect_error(edit_review_tree(state,list(action='move_question',question_id='q1',destination=5L)),
    'already in that category')
})
