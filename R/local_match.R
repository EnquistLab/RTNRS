#' Acceptance thresholds used by the matcher
#'
#' Internal.  From \code{class.taxamatch.php} lines 41-44.
#' @keywords internal
#' @noRd
tnrs_threshold <- function(search_mode = c("normal", "extended")) {
  search_mode <- match.arg(search_mode)
  list(
    # Ratio of edit distance to length above which a component is rejected
    ratio = if (search_mode == "extended") 0.5 else 0.3334,
    ratio_short = 0.5,
    # Multiplier bounding the summed edit distance across components
    multiplier = if (search_mode == "extended") 3L else 2L
  )
}

#' Decide whether two name components match
#'
#' Internal.  R port of \code{Taxamatch::match_genera()},
#' \code{match_family()} and \code{match_species_epithets()}, which share a
#' structure and differ only in their parameters.
#'
#' A pair is rejected outright when the edit distance is large relative to the
#' shorter string.  Otherwise it is accepted when the phonetic keys agree, or
#' when the edit distance is small, no more than half the shorter string, and
#' the leading characters agree.
#'
#' @param query,candidate Character vectors, recycled to a common length.
#' @param rank One of "family", "genus" or "epithet", selecting the parameters.
#' @param search_mode "normal" or "extended".
#' @param query_key,candidate_key Optional precomputed phonetic keys.  The
#'   reference keys are already held by the blocking index, and recomputing them
#'   for every query dominates the run time, so callers matching many names
#'   against a reference should pass them in.
#' @return A data.frame with columns \code{match}, \code{phonetic} and
#'   \code{edit_distance}.
#' @keywords internal
#' @noRd
tnrs_match_component <- function(query, candidate,
                                 rank = c("genus", "family", "epithet"),
                                 search_mode = c("normal", "extended"),
                                 query_key = NULL, candidate_key = NULL) {
  rank <- match.arg(rank)
  search_mode <- match.arg(search_mode)

  n <- max(length(query), length(candidate))
  query <- tnrs_toupper_ascii(rep_len(as.character(query), n))
  candidate <- tnrs_toupper_ascii(rep_len(as.character(candidate), n))

  # Upstream calls mdld with a block limit of 2 and a cap of 3 for families and
  # genera, and 4 and 4 for specific epithets
  block_limit <- if (rank == "epithet") 4L else 2L
  max_distance <- if (rank == "epithet") 4L else 3L
  # Largest edit distance the post-filter will accept
  max_accepted <- if (rank == "epithet") 4L else 3L

  key_type <- if (rank == "epithet") "epithet_only" else "genus_only"

  out <- data.frame(
    match = rep(FALSE, n), phonetic = rep(FALSE, n),
    edit_distance = rep(NA_integer_, n)
  )
  if (n == 0) {
    return(out)
  }

  ed <- tnrs_mdld(candidate, query, block_limit, max_distance)
  out$edit_distance <- ed

  len_q <- nchar(query, type = "bytes")
  len_c <- nchar(candidate, type = "bytes")
  shorter <- pmin(len_q, len_c)

  th <- tnrs_threshold(search_mode)

  # Rejected outright: nothing to compare, or too many edits for the length
  ratio <- ifelse(shorter > 0, ed / shorter, Inf)
  rejected <- shorter == 0 |
    (shorter < 6 & ratio > th$ratio_short) |
    (shorter >= 6 & ratio > th$ratio)

  if (is.null(query_key)) {
    query_key <- tnrs_near_match(query, key_type)
  }
  if (is.null(candidate_key)) {
    candidate_key <- tnrs_near_match(candidate, key_type)
  }
  phonetic <- rep_len(query_key, n) == rep_len(candidate_key, n)

  first_ok <- ed < 2 | substr(candidate, 1, 1) == substr(query, 1, 1)
  # Epithets additionally require the first three characters to agree at ED 4
  first3_ok <- if (rank == "epithet") {
    ed < 4 | substr(candidate, 1, 3) == substr(query, 1, 3)
  } else {
    rep(TRUE, n)
  }

  by_distance <- ed <= max_accepted & shorter >= 2 * ed & first_ok & first3_ok

  out$match <- !rejected & (phonetic | by_distance)
  out$phonetic <- !rejected & phonetic
  out$match[is.na(out$match)] <- FALSE
  out$phonetic[is.na(out$phonetic)] <- FALSE

  out
}

#' Combine per-component match results into one verdict
#'
#' Internal.  R port of \code{Taxamatch::match_matches()}: the edit distances are
#' summed, and the whole match is rejected if any component failed or if the
#' summed distance exceeds the number of components times the mode's multiplier.
#' The phonetic flag is true only when every component matched phonetically.
#'
#' @param components A list of data.frames from \code{tnrs_match_component()},
#'   all the same length.
#' @param search_mode "normal" or "extended".
#' @return A data.frame with columns \code{match}, \code{phonetic} and
#'   \code{edit_distance}.
#' @keywords internal
#' @noRd
tnrs_combine_matches <- function(components, search_mode = c("normal", "extended")) {
  search_mode <- match.arg(search_mode)
  stopifnot(length(components) > 0)

  n <- nrow(components[[1]])
  total_ed <- rep(0L, n)
  matched <- rep(TRUE, n)
  phonetic <- rep(TRUE, n)

  for (part in components) {
    ed <- part$edit_distance
    ed[is.na(ed)] <- 0L
    total_ed <- total_ed + ed
    matched <- matched & part$match
    phonetic <- phonetic & part$phonetic
  }

  max_ed <- length(components) * tnrs_threshold(search_mode)$multiplier

  data.frame(
    match = matched & total_ed <= max_ed,
    phonetic = phonetic,
    edit_distance = total_ed
  )
}
