#' Build a lookup from keys to positions
#'
#' Internal.  Maps each distinct key to the positions at which it occurs.
#' Implemented as a radix sort plus run boundaries rather than an environment:
#' populating an environment binding by binding is far too slow at backbone
#' scale, where a single rank can carry a million distinct keys, and the sorted
#' form uses much less memory.  Empty keys are dropped, since matching on them
#' is never useful.
#'
#' @param keys Character vector.
#' @return A list; query it with \code{tnrs_lookup()}.
#' @keywords internal
#' @noRd
tnrs_hash_index <- function(keys) {
  keep <- which(!is.na(keys) & nzchar(keys))

  if (length(keep) == 0) {
    return(list(
      keys = character(0), pos = integer(0),
      start = integer(0), end = integer(0)
    ))
  }

  sorted <- order(keys[keep], method = "radix")
  ordered_keys <- keys[keep][sorted]

  first <- !duplicated(ordered_keys)
  start <- which(first)

  list(
    keys = ordered_keys[first],
    pos = keep[sorted],
    start = start,
    end = c(start[-1] - 1L, length(ordered_keys))
  )
}

#' Build a lookup from integer keys to positions
#'
#' Internal.  The character version has to hash its whole key vector on every
#' query, which dominates the run time once a rank carries a million names.
#' Where the keys are already small positive integers - positions in the rank
#' above, or in a table of distinct name parts - they can index directly into a
#' pair of offset vectors instead, turning the lookup into two integer
#' subscripts and no hashing at all.
#'
#' @param ids Integer vector of keys, one per position; NA entries are dropped.
#' @param n_max Largest key value to make room for.
#' @return A list; query it with \code{tnrs_int_lookup()}.
#' @keywords internal
#' @noRd
tnrs_int_index <- function(ids, n_max = NULL) {
  ids <- as.integer(ids)
  keep <- which(!is.na(ids) & ids > 0L)

  if (is.null(n_max)) {
    n_max <- if (length(keep) == 0) 0L else max(ids[keep])
  }

  start <- integer(n_max)
  end <- integer(n_max)

  if (length(keep) == 0) {
    return(list(pos = integer(0), start = start, end = end, n_max = n_max))
  }

  sorted <- order(ids[keep], method = "radix")
  ordered_ids <- ids[keep][sorted]

  first <- !duplicated(ordered_ids)
  run_start <- which(first)
  run_end <- c(run_start[-1] - 1L, length(ordered_ids))

  start[ordered_ids[first]] <- run_start
  end[ordered_ids[first]] <- run_end

  list(pos = keep[sorted], start = start, end = end, n_max = n_max)
}

#' Look up integer keys
#'
#' @param index An index from \code{tnrs_int_index()}.
#' @param ids Integer keys.
#' @return Integer vector of positions.
#' @keywords internal
#' @noRd
tnrs_int_lookup <- function(index, ids) {
  ids <- ids[!is.na(ids) & ids > 0L & ids <= index$n_max]
  if (length(ids) == 0) {
    return(integer(0))
  }

  start <- index$start[ids]
  end <- index$end[ids]
  present <- start > 0L
  if (!any(present)) {
    return(integer(0))
  }
  start <- start[present]
  end <- end[present]

  if (length(start) == 1L) {
    return(index$pos[start:end])
  }

  index$pos[unlist(Map(seq.int, start, end), use.names = FALSE)]
}

#' Look up one or more keys
#'
#' @param index An index from \code{tnrs_hash_index()}.
#' @param keys Character vector of keys.
#' @return Integer vector of positions, empty if nothing matched.
#' @keywords internal
#' @noRd
tnrs_lookup <- function(index, keys) {
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0 || length(index$keys) == 0) {
    return(integer(0))
  }

  hits <- match(keys, index$keys)
  hits <- hits[!is.na(hits)]
  if (length(hits) == 0) {
    return(integer(0))
  }

  # One hit is much the commonest case, so avoid building a list for it
  if (length(hits) == 1L) {
    return(index$pos[index$start[hits]:index$end[hits]])
  }

  unlist(
    lapply(hits, function(i) index$pos[index$start[i]:index$end[i]]),
    use.names = FALSE
  )
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
#' @param affix Build the leading and trailing character columns and their
#'   indexes?  Needed for genera and families, which are searched by affix, but
#'   not for epithets, which are only ever reached through a matched parent.
#'   Skipping them roughly halves the cost at species rank, where the backbone
#'   carries over a million names.
#' @return A list holding the vectors and their indexes.
#' @keywords internal
#' @noRd
tnrs_build_rank_index <- function(names, word_type = "genus_only", parent = NULL,
                                  affix = TRUE) {
  names <- as.character(names)
  upper <- tnrs_toupper_ascii(names)
  width <- nchar(upper)

  out <- list(
    name = names,
    upper = upper,
    len = nchar(upper, type = "bytes"),
    key = tnrs_near_match(names, word_type),
    parent = parent
  )

  out$by_key <- tnrs_hash_index(out$key)
  out$by_upper <- tnrs_hash_index(out$upper)

  if (affix) {
    out$h1 <- substr(upper, 1, 1)
    out$h2 <- substr(upper, 1, 2)
    out$h3 <- substr(upper, 1, 3)
    out$t1 <- substr(upper, width, width)
    out$t3 <- substr(upper, pmax(1, width - 2), width)

    out$by_h1 <- tnrs_hash_index(out$h1)
    out$by_h2 <- tnrs_hash_index(out$h2)
    out$by_h3 <- tnrs_hash_index(out$h3)
    out$by_t1 <- tnrs_hash_index(out$t1)
    out$by_t3 <- tnrs_hash_index(out$t3)

    # Positions grouped by name length, so a length window is a list slice
    out$by_len <- split(seq_along(out$len), out$len)
  }

  if (!is.null(parent)) {
    # Parents are positions in the rank above, so an integer index applies
    out$by_parent <- tnrs_int_index(parent)
  }

  out
}

#' Look up many keys at once, keeping them separate
#'
#' Internal.  \code{match()} rebuilds a hash of the whole key vector on every
#' call, which is the single largest cost in matching once a rank carries a
#' million names.  Doing the whole batch in one call pays that cost once.
#'
#' @param index An index from \code{tnrs_hash_index()}.
#' @param keys Character vector of keys.
#' @return A list the same length as \code{keys}, each element the positions for
#'   that key.
#' @keywords internal
#' @noRd
tnrs_lookup_each <- function(index, keys) {
  out <- vector("list", length(keys))
  out[] <- list(integer(0))

  if (length(keys) == 0 || length(index$keys) == 0) {
    return(out)
  }

  hits <- match(keys, index$keys)
  found <- which(!is.na(hits))

  for (i in found) {
    hit <- hits[i]
    out[[i]] <- index$pos[index$start[hit]:index$end[hit]]
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
