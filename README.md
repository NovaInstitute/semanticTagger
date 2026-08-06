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

## Semantic persistence boundary

`tag_state_to_semantic_records()` projects a run into separate run, embedding,
hierarchy, and review record sets. `tag_state_from_semantic_records()` rebuilds
authoritative state from those records plus normalized questions. The model
uses absolute IRIs, leaf-only question membership, parent-cluster links, and
separately addressable embedding records. A persistence adapter—ultimately
`novaRush`—is responsible for graph names, branches, batching, and mapping
numeric arrays to Fluree's vector datatype.
