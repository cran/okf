# okf 0.7.0

* New `okf_diff()`: deterministic concept-level changelog between two states
  of a bundle. Each side can be a bundle directory, an `okf_read()` bundle, a
  DuckDB catalog path, or an open connection — so it covers both "what drifted
  since the last ingest" (catalog vs directory) and snapshot-vs-snapshot
  comparison. Reports concepts added/removed/changed (by `content_hash`),
  frontmatter `type`/`title` changes, and link-graph deltas (edges
  added/removed, links newly broken or fixed).
* CLI: new `diff` verb (`okf diff <a> <b> [--json]`), exit 0 when identical /
  1 when different, for use as a CI change gate.

# okf 0.6.0

* `[[wikilink]]` support: `[[target]]` / `[[target|display]]` references are
  resolved by name — `id`, then `aliases`, then `title`, then filename stem —
  making Obsidian/Logseq/Foam-style vaults ingestible. Ambiguous names resolve
  to nothing (deterministic) rather than guessing. Markdown `](path)` links
  are unchanged.
* New `okf_extract_wikilinks()`; `okf_links()` now returns both link kinds.

# okf 0.5.2

* First CRAN release. Read, validate, and load OKF bundles into a portable
  DuckDB catalog; concept graph (`okf_links()`, `okf_backlinks()`,
  `okf_impact()`, `okf_clusters()`); HTML rendering (`okf_html()`,
  `okf_graph_html()`, Mermaid export); index-first context assembly
  (`okf_context()`); health checks (`okf_doctor()`, `okf_doctor_fix()`);
  incremental re-ingest/re-embed; optional local-embedder semantic search
  (`okf_embed()`, `okf_rag()`).
