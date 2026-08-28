#' Build a hash index from keys to positions
#'
#' Internal.  Returns an environment mapping each distinct key to the integer
#' positions at which it occurs, giving constant-time lookup without taking on a
#' dependency.  Empty keys are dropped, since they are never worth matching on.
#'
#' @param keys Character vector.
#' @return An environment; use \code{tnrs_lookup()} to query it.
#' @keywords internal
#' @noRd
tnrs_hash_index <- function(keys) {
  keep <- which(!is.na(keys) & nzchar(keys))
  groups <- split(keep, keys[keep])

  env <- new.env(hash = TRUE, parent = emptyenv(), size = length(groups))
  for (nm in names(groups)) {
    assign(nm, groups[[nm]], envir = env)
  }
  env
}

#' Look up one or more keys in a hash index
#'
#' @param index An environment from \code{tnrs_hash_index()}.
#' @param keys Character vector of keys.
#' @return Integer vector of positions, empty if nothing matched.
#' @keywords internal
#' @noRd
tnrs_lookup <- function(index, keys) {
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) {
    return(integer(0))
  }
  hits <- mget(keys, envir = index, ifnotfound = list(NULL))
  out <- unlist(hits, use.names = FALSE)
  if (is.null(out)) integer(0) else out
}

#' Build the blocking index used for fuzzy candidate generation
#'
#' Internal.  R equivalent of the denormalised \code{genlist} / \code{famlist} /
#' \code{splist} tables built by \code{tnrs_db/taxamatch_tables/}, together with
#' the indexes the upstream SQL relies on.
#'
#' For each name it stores the byte length, the Rees phonetic key, and the
#' leading and trailing one, two and three characters.  Those are exactly the
#' columns the upstream candidate queries filter on.  Note that the affix
#' columns are taken from the \emph{raw} name while the phonetic and search keys
#' come from the normalized form, which is what upstream does.
#'
#' @param names Character vector of names at a single rank.
#' @param word_type Passed to \code{tnrs_near_match()}: "genus_only" for genera
#'   and families, "epithet_only" for epithets.
#' @param parent Optional integer vector, the same length as \code{names},
#'   giving the position of each name's parent in the rank above.  Used to
#'   restrict the search to the descendants of matched parents.
#' @return A list holding the vectors and their hash indexes.
#' @keywords internal
#' @noRd
tnrs_build_rank_index <- function(names, word_type = "genus_only", parent = NULL) {
  names <- as.character(names)
  upper <- tnrs_toupper_ascii(names)

  out <- list(
    name = names,
    upper = upper,
    len = nchar(upper, type = "bytes"),
    key = tnrs_near_match(names, word_type),
    search = tnrs_normalize(names),
    h1 = substr(upper, 1, 1),
    h2 = substr(upper, 1, 2),
    h3 = substr(upper, 1, 3),
    t1 = substr(upper, nchar(upper), nchar(upper)),
    t2 = substr(upper, pmax(1, nchar(upper) - 1), nchar(upper)),
    t3 = substr(upper, pmax(1, nchar(upper) - 2), nchar(upper)),
    parent = parent
  )

  out$by_key <- tnrs_hash_index(out$key)
  out$by_upper <- tnrs_hash_index(out$upper)
  out$by_h1 <- tnrs_hash_index(out$h1)
  out$by_h2 <- tnrs_hash_index(out$h2)
  out$by_h3 <- tnrs_hash_index(out$h3)
  out$by_t1 <- tnrs_hash_index(out$t1)
  out$by_t3 <- tnrs_hash_index(out$t3)

  # Positions grouped by name length, so a length window is a list slice
  out$by_len <- split(seq_along(out$len), out$len)

  if (!is.null(parent)) {
    out$by_parent <- tnrs_hash_index(as.character(parent))
  }

  out
}

#' Positions whose name length falls within a window
#'
#' @param index A rank index from \code{tnrs_build_rank_index()}.
#' @param length Target length.
#' @param window Half width of the window.
#' @keywords internal
#' @noRd
tnrs_in_length_window <- function(index, length, window) {
  wanted <- as.character(seq.int(max(1L, length - window), length + window))
  out <- unlist(index$by_len[wanted], use.names = FALSE)
  if (is.null(out)) integer(0) else out
}
