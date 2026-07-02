# ============================================================================
# okf -- diff: a DETERMINISTIC concept-level changelog between two states of a
# knowledge base.
#
# `git diff` shows text hunks; `okf_diff()` shows what changed as *knowledge
# structure*: concepts added/removed, bodies changed (by content_hash),
# frontmatter `type`/`title` changes, and graph deltas (edges added/removed,
# links newly broken / fixed). Each side can be a bundle directory, an
# okf_read() bundle, a .duckdb catalog path, or an open DBI connection -- so
# `okf_diff(catalog, dir)` answers "what drifted since the last ingest" and
# `okf_diff(old_dir, new_dir)` compares two snapshots. Pure hash/set
# comparison, outputs sorted by path: no model, no wall clock.
#
# Mirrors py/okf/diff.py.
# ============================================================================

# Normalize one diff side to a comparable state:
#   concepts: data.frame(path, type, title, content_hash)
#   edges:    data.frame(src_path, dst_path)   -- unique resolved link pairs
#   broken:   data.frame(src_path, dst_raw)    -- unique unresolved links
.okf_diff_state <- function(x, bundle_id = NULL) {
  if (inherits(x, "DBIConnection")) return(.okf_state_from_con(x, bundle_id))
  if (is.character(x) && length(x) == 1L && grepl("\\.duckdb$", x) && file.exists(x)) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = x, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.okf_state_from_con(con, bundle_id))
  }
  rd <- if (is.list(x) && !is.null(x$concepts)) x
        else if (is.character(x) && length(x) == 1L && dir.exists(x)) okf_read(x)
        else stop("diff side must be a bundle dir, okf_read() bundle, .duckdb path, or DBI connection")
  lk <- okf_links(rd)
  concepts <- data.frame(
    path         = vapply(rd$concepts, function(c) c$path, character(1)),
    type         = vapply(rd$concepts, function(c) c$type, character(1)),
    title        = vapply(rd$concepts, function(c) c$title, character(1)),
    content_hash = vapply(rd$concepts, function(c) c$content_hash, character(1)),
    stringsAsFactors = FALSE)
  list(concepts = concepts[order(concepts$path), , drop = FALSE],
       edges  = unique(lk[lk$resolved,  c("src_path", "dst_path"), drop = FALSE]),
       broken = unique(lk[!lk$resolved, c("src_path", "dst_raw"),  drop = FALSE]))
}

.okf_state_from_con <- function(con, bundle_id = NULL) {
  if (is.null(bundle_id)) {
    bids <- DBI::dbGetQuery(con, "SELECT DISTINCT bundle_id FROM okf_concept")$bundle_id
    if (length(bids) > 1) stop("catalog contains multiple bundles; pass a bundle_id")
    if (length(bids) == 0) stop("catalog contains no ingested bundle")
    bundle_id <- bids
  }
  cps <- DBI::dbGetQuery(con,
    "SELECT path, type, title, content_hash FROM okf_concept WHERE bundle_id = ? ORDER BY path",
    params = list(bundle_id))
  lks <- DBI::dbGetQuery(con,
    "SELECT src_path, dst_raw, dst_path, resolved FROM okf_link WHERE bundle_id = ?",
    params = list(bundle_id))
  res <- as.logical(lks$resolved)
  list(concepts = cps,
       edges  = unique(lks[res,  c("src_path", "dst_path"), drop = FALSE]),
       broken = unique(lks[!res, c("src_path", "dst_raw"),  drop = FALSE]))
}

# setdiff on composite keys, returned as a data.frame sorted by key.
.okf_key_diff <- function(a, b, cols) {
  key <- function(df) if (nrow(df)) do.call(paste, c(df[cols], sep = "\r")) else character(0)
  only <- sort(setdiff(key(a), key(b)))
  if (!length(only)) {
    out <- a[0, cols, drop = FALSE]; rownames(out) <- NULL; return(out)
  }
  parts <- strsplit(only, "\r", fixed = TRUE)
  out <- as.data.frame(do.call(rbind, parts), stringsAsFactors = FALSE)
  names(out) <- cols
  out
}

# frontmatter field delta over common paths -> data.frame(path, from, to)
.okf_field_diff <- function(common, va, vb) {
  neq <- vapply(common, function(p) {
    x <- va[[p]]; y <- vb[[p]]
    if (is.na(x) && is.na(y)) FALSE else if (is.na(x) || is.na(y)) TRUE else x != y
  }, logical(1))
  ps <- sort(common[neq])
  data.frame(path = ps, from = unname(vapply(ps, function(p) va[[p]], character(1))),
             to = unname(vapply(ps, function(p) vb[[p]], character(1))),
             stringsAsFactors = FALSE)
}

#' Concept-level diff between two states of an OKF knowledge base.
#'
#' Compares two bundle states and reports what changed as knowledge structure
#' rather than text hunks: concepts added / removed / changed (by
#' `content_hash` of the body), frontmatter `type` and `title` changes, and
#' concept-graph deltas (resolved edges added/removed, links newly broken or
#' fixed). Fully deterministic: pure hash and set comparison, all output sorted
#' by path, no wall clock.
#'
#' Each side can be a bundle directory, a bundle from [okf_read()], a path to a
#' `.duckdb` catalog, or an open DBI connection to one. Diffing a catalog
#' against its source directory answers "what drifted since the last ingest";
#' diffing two directories compares two snapshots.
#'
#' @param a The "before" side.
#' @param b The "after" side.
#' @param bundle_id_a,bundle_id_b Optional bundle id when a side is a catalog
#'   holding more than one bundle.
#' @return A list with `identical` (logical), `added`/`removed`/`changed`
#'   (character path vectors), `type_changed`/`retitled` (data.frames of
#'   `path`/`from`/`to`), `links_added`/`links_removed` (data.frames of
#'   `src_path`/`dst_path`), `broken_added`/`broken_fixed` (data.frames of
#'   `src_path`/`dst_raw`), and `summary` (named counts, including
#'   `unchanged`).
#' @export
okf_diff <- function(a, b, bundle_id_a = NULL, bundle_id_b = NULL) {
  sa <- .okf_diff_state(a, bundle_id_a)
  sb <- .okf_diff_state(b, bundle_id_b)
  ca <- sa$concepts; cb <- sb$concepts

  added   <- sort(setdiff(cb$path, ca$path))
  removed <- sort(setdiff(ca$path, cb$path))
  common  <- intersect(ca$path, cb$path)

  ha <- as.list(setNames(ca$content_hash, ca$path))
  hb <- as.list(setNames(cb$content_hash, cb$path))
  changed <- sort(common[vapply(common, function(p) !identical(ha[[p]], hb[[p]]), logical(1))])

  type_changed <- .okf_field_diff(common, as.list(setNames(ca$type, ca$path)),
                                  as.list(setNames(cb$type, cb$path)))
  retitled     <- .okf_field_diff(common, as.list(setNames(ca$title, ca$path)),
                                  as.list(setNames(cb$title, cb$path)))

  links_added   <- .okf_key_diff(sb$edges,  sa$edges,  c("src_path", "dst_path"))
  links_removed <- .okf_key_diff(sa$edges,  sb$edges,  c("src_path", "dst_path"))
  broken_added  <- .okf_key_diff(sb$broken, sa$broken, c("src_path", "dst_raw"))
  broken_fixed  <- .okf_key_diff(sa$broken, sb$broken, c("src_path", "dst_raw"))

  ident <- !length(added) && !length(removed) && !length(changed) &&
    !nrow(type_changed) && !nrow(retitled) &&
    !nrow(links_added) && !nrow(links_removed) &&
    !nrow(broken_added) && !nrow(broken_fixed)

  list(identical = ident, added = added, removed = removed, changed = changed,
       type_changed = type_changed, retitled = retitled,
       links_added = links_added, links_removed = links_removed,
       broken_added = broken_added, broken_fixed = broken_fixed,
       summary = list(
         added = length(added), removed = length(removed), changed = length(changed),
         unchanged = length(common) - length(changed),
         type_changed = nrow(type_changed), retitled = nrow(retitled),
         links_added = nrow(links_added), links_removed = nrow(links_removed),
         broken_added = nrow(broken_added), broken_fixed = nrow(broken_fixed)))
}
