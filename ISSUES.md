# novaTagger implementation backlog

## Required for the architecture

- [x] Define the normalized question input contract independently of forms.
- [x] Accept question records rather than reading `forms.Rda`.
- [x] Move provider-neutral embedding, clustering, evidence, tagging, review,
  diagnostics, and hierarchy-editing code with tests.
- [x] Keep the package free of direct Fluree HTTP calls and compose semantic
  persistence through public `novaRush` operations.
- [x] Retain a persistence-neutral tagging-store contract.
- [x] Provide OpenAI and optional Ollama adapters through one provider contract.
- [x] Provide provider- and persistence-neutral resumable workflow orchestration.
- [x] Define transport-free tagging-domain JSON-LD records and reconstruction.
- [x] Query novaGraphDB taggable-question knowledge through novaRush with
  paginated question and closed-answer retrieval.
- [x] Provide one Fluree-backed application entry point that retrieves
  questions and starts or resumes an authoritative tagging workflow.

## Persistence redesign

- [x] Model each hierarchy as first-class, versioned knowledge.
- [x] Persist question membership only at leaf clusters in the domain model.
- [x] Preserve higher levels through explicit parent-cluster relationships.
- [x] Separate run, embedding, hierarchy, and review records through configurable
  named graphs; survey knowledge remains independently owned by `novaGraphDB`.
- [ ] Keep R serialization only as a temporary recovery cache.
- [x] Reconstruct authoritative state from semantic graph entities.
- [x] Batch hierarchy writes and publish the run pointer only after completion.
- [ ] Preserve BERTopic output before attempting remote persistence.

## Tagging quality and review

- [ ] Reintroduce only the useful parts of deterministic tag normalization and
  synonym review behind the current model-provider contract; the untested
  combined-package implementation was removed during repository separation.
- [ ] Define and test a provider-neutral tag-path audit if support-monotonicity
  remains a useful reviewer diagnostic.
- [ ] Test clustering with full-scale real OpenAI embeddings.
- [ ] Establish benchmark questions and reviewer-agreement measures.
- [ ] Validate cohesion in original embedding dimensions.
- [ ] Add optional UMAP navigation alongside PCA.
- [ ] Implement split, merge, outlier handling, and scoped reclustering.
- [ ] Benchmark Leiden against BERTopic/HDBSCAN on a defined neighbour graph.
- [x] Recompute similarities after accepted tag edits.
- [x] Define a persistence-neutral repository contract for similar questions,
  positive/negative reviewer precedents, and approved guidance.
- [ ] Implement the evidence composition adapter that maps similarity,
  precedent, and guidance requests onto public `novaRush` operations.
- [x] Implement the semantic-state composition adapter using novaRush named
  graphs and native vectors.
- [ ] Validate precedent ranking and filtering against real reviewer history.
- [ ] Define explicit guidance promotion into Developer Memory.

## Before production use

- [ ] Add cost, rate-limit, and remote-persistence interruption tests (provider
  conformance and in-memory interruption/resume tests are complete).
- [ ] Add user-facing documentation, examples, and CI (`R CMD check` is clean
  for the currently staged core).
