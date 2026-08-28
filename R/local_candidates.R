#' Generate candidate genera for a query name
#'
#' Internal.  R equivalent of \code{Queries::genus_cur3()}, the blocking step
#' that narrows the reference list before any edit distance is computed.  Getting
#' this right is what keeps matching from being a full scan of the backbone.
#'
#' Candidates are the union of three sets:
#' \enumerate{
#'   \item genera whose phonetic key equals the query's;
#'   \item genera within a length window of the query whose leading or trailing
#'     characters agree, with the number of characters compared depending on the
#'     shorter of the two names;
#'   \item genera that carry a species epithet phonetically equal to the query's
#'     epithet, within a wider length window.  This is what rescues a badly
#'     misspelled genus when the epithet is right.
#' }
#'
#' @param genus Query genus, as submitted.
#' @param epithet Query specific epithet, or "" if none.
#' @param index Genus index from \code{tnrs_build_rank_index()}.
#' @param species_index Optional species index, used for the epithet rescue.
#'   Must carry a \code{parent} pointing at positions in \code{index}.
#' @param search_mode "normal" or "extended"; extended widens the length window.
#' @return Integer vector of positions into \code{index}, sorted and unique.
#' @keywords internal
#' @noRd
tnrs_genus_candidates <- function(genus, epithet = "", index,
                                  species_index = NULL,
                                  search_mode = c("normal", "extended")) {
  search_mode <- match.arg(search_mode)

  genus <- tnrs_toupper_ascii(as.character(genus))
  if (is.na(genus) || !nzchar(genus)) {
    return(integer(0))
  }

  query_key <- tnrs_near_match(genus, "genus_only")
  query_len <- nchar(genus, type = "bytes")
  window <- if (search_mode == "extended") 4L else 2L

  # 1. Exact phonetic hit, with no length restriction
  candidates <- tnrs_lookup(index$by_key, query_key)

  # 2. Length window, then the affix test.  Upstream compares one, two or three
  # characters depending on min(query length, candidate length), so the test has
  # to be evaluated per candidate rather than looked up directly.
  window_hits <- tnrs_in_length_window(index, query_len, window)

  if (length(window_hits) > 0) {
    shorter <- pmin(query_len, index$len[window_hits])

    head1 <- substr(genus, 1, 1)
    tail1 <- substr(genus, query_len, query_len)
    head2 <- substr(genus, 1, 2)
    head3 <- substr(genus, 1, 3)
    tail3 <- substr(genus, max(1L, query_len - 2L), query_len)

    affix_ok <-
      (shorter < 5 & (index$h1[window_hits] == head1 | index$t1[window_hits] == tail1)) |
        (shorter == 5 & (index$h2[window_hits] == head2 | index$t3[window_hits] == tail3)) |
        (shorter > 5 & (index$h3[window_hits] == head3 | index$t3[window_hits] == tail3))

    candidates <- c(candidates, window_hits[which(affix_ok)])
  }

  # 3. Epithet rescue: genera carrying a species whose epithet sounds like the
  # query's epithet, within a wider window.  Upstream always uses +/- 4 here,
  # regardless of search mode.
  if (!is.null(species_index) && !is.na(epithet) && nzchar(epithet)) {
    epithet_key <- tnrs_near_match(epithet, "epithet_only")
    species_hits <- tnrs_lookup(species_index$by_key, epithet_key)

    if (length(species_hits) > 0) {
      parents <- unique(species_index$parent[species_hits])
      parents <- parents[!is.na(parents)]
      if (length(parents) > 0) {
        in_window <- abs(index$len[parents] - query_len) <= 4L
        candidates <- c(candidates, parents[which(in_window)])
      }
    }
  }

  sort(unique(candidates))
}

#' Generate candidate families for a query family
#'
#' Internal.  R equivalent of \code{Queries::family_cur()}.  Simpler than the
#' genus query: the affix test uses the leading character only, and there is no
#' epithet rescue.
#'
#' @param family Query family, as submitted.
#' @param index Family index from \code{tnrs_build_rank_index()}.
#' @param search_mode "normal" or "extended".
#' @return Integer vector of positions into \code{index}.
#' @keywords internal
#' @noRd
tnrs_family_candidates <- function(family, index,
                                   search_mode = c("normal", "extended")) {
  search_mode <- match.arg(search_mode)

  family <- tnrs_toupper_ascii(as.character(family))
  if (is.na(family) || !nzchar(family)) {
    return(integer(0))
  }

  query_key <- tnrs_near_match(family, "genus_only")
  query_len <- nchar(family, type = "bytes")
  window <- if (search_mode == "extended") 4L else 2L

  candidates <- tnrs_lookup(index$by_key, query_key)

  window_hits <- tnrs_in_length_window(index, query_len, window)
  if (length(window_hits) > 0) {
    head1 <- substr(family, 1, 1)
    candidates <- c(candidates, window_hits[index$h1[window_hits] == head1])
  }

  sort(unique(candidates))
}

#' Generate candidate names at a rank below the one already matched
#'
#' Internal.  R equivalent of \code{Queries::species_cur_in2()} and the two
#' infraspecific queries: the search is confined to the children of the
#' already-matched parents, within a length window of plus or minus four.
#'
#' @param epithet Query epithet at this rank.
#' @param parents Integer positions of the matched parents, in the rank above.
#' @param index Index for this rank, carrying a \code{parent} vector.
#' @return Integer vector of positions into \code{index}.
#' @keywords internal
#' @noRd
tnrs_child_candidates <- function(epithet, parents, index) {
  epithet <- tnrs_toupper_ascii(as.character(epithet))
  if (is.na(epithet) || !nzchar(epithet) || length(parents) == 0) {
    return(integer(0))
  }

  children <- tnrs_lookup(index$by_parent, as.character(parents))
  if (length(children) == 0) {
    return(integer(0))
  }

  query_len <- nchar(epithet, type = "bytes")
  keep <- abs(index$len[children] - query_len) <= 4L

  sort(unique(children[which(keep)]))
}
