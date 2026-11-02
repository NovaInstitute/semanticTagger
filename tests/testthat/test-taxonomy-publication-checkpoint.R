checkpoint_taxonomy_fixture <- function(destination='a',revision=1L) {
  new_reviewed_taxonomy(
    nodes=data.frame(id=c('root','a','b'),parent_id=c(NA,'root','root'),label=c('Questions','A','B')),
    assignments=data.frame(question_id='question-1',leaf_id=destination),
    run_id='checkpoint-run',revision=revision
  )
}

test_that('reviewed records coexist with and ignore immutable pipeline hierarchy', {
  taxonomy <- checkpoint_taxonomy_fixture()
  records <- reviewed_taxonomy_records(taxonomy)
  pipeline <- list('@id'='https://example.org/pipeline-cluster',
    '@type'='https://data.nova.org/vocabulary/tagging/QuestionCluster',
    'https://schema.org/identifier'='legacy')
  mixed <- records;mixed$hierarchy <- c(list(pipeline),mixed$hierarchy)

  restored <- reviewed_taxonomy_from_records(mixed)

  expect_equal(restored$nodes,taxonomy$nodes)
  expect_equal(restored$assignments,taxonomy$assignments)
  expect_true(all(vapply(records$hierarchy[seq_len(nrow(taxonomy$nodes))],function(node)
    identical(node[['@type']],'https://data.nova.org/vocabulary/tagging/ReviewedTaxonomyNode'),logical(1))))
})

test_that('checkpoint repository preserves pipeline records and updates only canonical deltas', {
  graphs <- as.list(stats::setNames(paste0('https://example.org/graph/',c('run','embedding','hierarchy','review')),
    c('run','embedding','hierarchy','review')))
  taxonomy <- checkpoint_taxonomy_fixture()
  scope <- tagging_entity_iri('run',taxonomy$run_id)
  pipeline <- list('@id'='https://example.org/pipeline-cluster',
    '@type'='https://data.nova.org/vocabulary/tagging/QuestionCluster')
  original_run <- list('@id'=scope,'@type'='https://data.nova.org/vocabulary/tagging/TaggingRun',
    'https://schema.org/identifier'=taxonomy$run_id,'https://schema.org/version'=0L)
  stored <- stats::setNames(list(list(original_run),list(),list(pipeline),list()),unlist(graphs,use.names=FALSE))
  calls <- list()
  payloads <- function(document) {
    if(is.null(document)) return(list())
    lapply(document,function(node) .nr_payload_value(node[[.nr_payload_property]]))
  }
  apply_write <- function(delete=NULL,insert=NULL,graph) {
    old <- payloads(delete);new <- payloads(insert)
    old_ids <- if(length(old)) vapply(old,`[[`,character(1),'@id') else character()
    if(length(old_ids)) stored[[graph]] <<- Filter(function(node) !node[['@id']] %in% old_ids,stored[[graph]])
    for(node in new) {
      ids <- if(length(stored[[graph]])) vapply(stored[[graph]],`[[`,character(1),'@id') else character()
      hit <- match(node[['@id']],ids)
      if(is.na(hit)) stored[[graph]][[length(stored[[graph]])+1L]] <<- node else stored[[graph]][[hit]] <<- node
    }
    calls[[length(calls)+1L]] <<- list(graph=graph,deleted=old,inserted=new)
    list(ok=TRUE)
  }
  testthat::local_mocked_bindings(
    .nr_query_payloads=function(scope_iri,graph,...) stored[[graph]],
    .nr_query_payloads_of_type=function(scope_iri,graph,types,...) Filter(
      function(node) node[['@type']] %in% types,stored[[graph]]),
    .nr_update_named_graph=function(delete=NULL,insert=NULL,graph,...) apply_write(delete,insert,graph),
    .nr_upsert_named_graph=function(document,graph,...) apply_write(insert=document,graph=graph),
    .package='novaTagger'
  )
  repository <- novarush_reviewed_taxonomy_repository(list(branch='review'),graphs,branch='review')
  publication <- taxonomy_publication_record(taxonomy,'publication-1','reviewer','review','main-head')
  first <- repository$checkpoint(taxonomy)
  expect_equal(first$delta$added,nrow(taxonomy$nodes)+nrow(taxonomy$assignments))
  expect_true(any(vapply(stored[[graphs$hierarchy]],function(node) identical(node[['@id']],pipeline[['@id']]),logical(1))))
  expect_length(stored[[graphs$review]],0L)
  repository$record_publication(taxonomy,publication)
  expect_true(any(vapply(stored[[graphs$review]],function(node)
    identical(node[['https://schema.org/identifier']],'publication-1'),logical(1))))

  calls <- list();next_taxonomy <- checkpoint_taxonomy_fixture('b',2L)
  second <- repository$checkpoint(next_taxonomy)
  hierarchy_calls <- Filter(function(call) identical(call$graph,graphs$hierarchy),calls)
  expect_equal(second$delta,list(changed=1L,removed=0L,added=0L))
  expect_equal(sum(vapply(hierarchy_calls,function(call) length(call$inserted),integer(1))),1L)
})
