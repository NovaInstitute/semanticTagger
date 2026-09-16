# novaTagger

`novaTagger` owns provider-neutral question embedding, clustering, tagging,
evidence selection, reviewer decisions, hierarchy diagnostics, and structural
editing. It has no Shiny interface and performs no direct Fluree HTTP requests.

It consumes normalized question entities, produces tagging-domain JSON-LD and
operations, and delegates Fluree execution to `novaRush`.

The provider-neutral core is now staged here with its tests. It includes the
model-provider and persistence contracts, hierarchical and BERTopic clustering,
evidence selection, proposal parsing, reviewer decisions, guidance, diagnostics,
hierarchy editing, provider adapters, and resumable domain orchestration. Fluree
projections, legacy cleaning, and application/UI orchestration remain outside
this package for later batches.

## Model providers

The tagging core uses one provider contract. OpenAI is the intended hosted
provider, while Ollama remains available for inexpensive local testing.

Configure OpenAI through environment variables rather than source files:

```r
Sys.setenv(
  OPENAI_API_KEY = "...",
  OPENAI_EMBED_MODEL = "text-embedding-3-small",
  OPENAI_TAGGER_MODEL = "gpt-5.4-mini"
)
provider <- novaTagger::openai_model_provider()
```

Optional overrides are `OPENAI_BASE_URL`, `OLLAMA_BASE_URL`,
`OLLAMA_EMBED_MODEL`, and `OLLAMA_TAGGER_MODEL`. Provider metadata records model
names and base URLs but never API keys. Applications should obtain the provider
from configuration and pass it into the tagging workflow; domain functions do
not select a provider themselves.

## Resumable workflow

The public controller accepts normalized questions and explicit provider/store
dependencies:

```r
questions <- novaTagger::query_taggable_questions(
  config,
  graph = "https://data.nova.org/graphs/surveys",
  branch = "main",
  page_size = 500L
)
store <- novaTagger::memory_tag_store()
run <- novaTagger::new_tagging_workflow(questions, store, run_id = "review-001")
run <- novaTagger::workflow_embed_questions(run, provider, batch_size = 100)
run <- novaTagger::workflow_cluster_hierarchical(run, clusters_by_level = c(8, 4, 2))
run <- novaTagger::workflow_propose_next(run, provider, evidence = evidence)
```

Each successful embedding batch and every hierarchy, proposal, and review
transition is saved through the `tag_store` contract. A caller resumes with
`resume_tagging_workflow(store)`. Evidence retrieval is supplied by the caller,
so the workflow contains no Fluree transport or branch assumptions.

For the normal Fluree-backed path, one composition function performs the
question read and automatically starts or resumes the requested run:

```r
run <- novaTagger::fluree_tagging_workflow(
  config = config,
  survey_graph = "https://data.nova.org/graphs/surveys",
  tagging_graphs = graphs,
  run_id = "review-001"
)
run <- novaTagger::workflow_embed_questions(run, provider, batch_size = 100L)
run <- novaTagger::workflow_cluster_bertopic(run)
```

Calling `fluree_tagging_workflow()` again with the same ledger, branch, graph
IRIs, and `run_id` reconstructs the last published checkpoint from Fluree.

## Semantic persistence boundary

`tag_state_to_semantic_records()` projects a run into separate run, embedding,
hierarchy, and review record sets. `tag_state_from_semantic_records()` rebuilds
authoritative state from those records plus normalized questions. The model
uses absolute IRIs, leaf-only question membership, parent-cluster links, and
separately addressable embedding records. A persistence adapter—ultimately
`novaRush`—is responsible for graph names, branches, batching, and mapping
numeric arrays to Fluree's vector datatype.

Use the novaRush composition adapter when Fluree should be authoritative:

```r
config <- novaRush::setConfig(
  baseUrl = "http://localhost:8090",
  ledger = "survey-tagger",
  branch = "review-candidate"
)
graphs <- list(
  run = "https://data.nova.org/graphs/tagging/run",
  embedding = "https://data.nova.org/graphs/tagging/embedding",
  hierarchy = "https://data.nova.org/graphs/tagging/hierarchy",
  review = "https://data.nova.org/graphs/tagging/review"
)
repository <- novaTagger::novarush_semantic_repository(
  config, graphs, batch_size = 250L
)
store <- novaTagger::semantic_tag_store(
  repository, questions, run_id = "review-001"
)
```

The adapter stores embeddings as native Fluree vectors, writes supporting
records in bounded batches, and publishes the current run pointer last. A new
R process can construct the same repository and call
`resume_tagging_workflow(store)` to reconstruct the authoritative state.
