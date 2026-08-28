#' Modified Damerau-Levenshtein distance (reference implementation)
#'
#' Internal.  R port of \code{DamerauLevenshteinMod::mdld_php()} from TNRSbatch.
#' This is the readable reference implementation and the oracle the compiled
#' version in \code{src/mdld.cpp} is checked against; use \code{tnrs_mdld()} for
#' real work.
#'
#' It differs from a standard Damerau-Levenshtein distance in three ways, all of
#' which matter for agreement with the API:
#' \itemize{
#'   \item common leading characters, and then common trailing characters, are
#'     trimmed before the matrix is built;
#'   \item transpositions of blocks of up to \code{block_limit} characters are
#'     recognised, not just adjacent single-character swaps;
#'   \item the calculation aborts early once the smallest value seen so far
#'     reaches \code{max_distance}, returning that value rather than the true
#'     distance.
#' }
#' \code{stringdist(method = "dl")} implements none of these and is not a
#' substitute.
#'
#' Comparison is done on \emph{bytes}, because PHP's \code{strlen()} and string
#' indexing are byte based.  For the upper-cased ASCII strings the matcher
#' actually compares this makes no difference, but it matters for any name that
#' still carries non-ASCII characters.
#'
#' @param s1,s2 Single strings to compare.
#' @param block_limit Longest transposed block to look for.  Upstream uses 2 for
#'   families and genera and 4 for specific epithets.
#' @param max_distance Edit distance at which to abort.  Upstream uses 3 for
#'   families and genera and 4 for specific epithets.
#' @return Integer edit distance.
#' @keywords internal
#' @noRd
tnrs_mdld_r <- function(s1, s2, block_limit = 2, max_distance = 4) {
  if (is.na(s1) || is.na(s2)) {
    return(NA_integer_)
  }
  if (identical(s1, s2)) {
    return(0L)
  }

  a <- as.integer(charToRaw(s1))
  b <- as.integer(charToRaw(s2))
  len1 <- length(a)
  len2 <- length(b)

  if (len1 == 0 || len2 == 0) {
    return(as.integer(max(len1, len2)))
  }
  if (len1 == 1 && len2 == 1) {
    return(1L)
  }

  # Trim common leading bytes, then common trailing bytes

  i <- 1L
  j <- 1L
  while (i <= len1 && j <= len2 && a[i] == b[j]) {
    i <- i + 1L
    j <- j + 1L
  }
  e1 <- len1
  e2 <- len2
  while (e1 >= i && e2 >= j && a[e1] == b[e2]) {
    e1 <- e1 - 1L
    e2 <- e2 - 1L
  }

  ta <- if (e1 >= i) a[i:e1] else integer(0)
  tb <- if (e2 >= j) b[j:e2] else integer(0)
  len1 <- length(ta)
  len2 <- length(tb)

  if (len1 == 0 || len2 == 0) {
    return(as.integer(max(len1, len2)))
  }
  if (len1 == 1 && len2 == 1) {
    # The strings differ, or the identity check above would have returned
    return(1L)
  }

  # Columns are indexed from 0 upstream; add one throughout.
  m <- matrix(0L, nrow = len1 + 1, ncol = len2 + 1)
  m[1, ] <- 0:len2
  m[, 1] <- 0:len1

  base_block <- floor(min(len1 / 2, len2 / 2, block_limit))
  current_distance <- max_distance

  for (s in seq_len(len1)) {
    for (t in seq_len(len2)) {
      this_cost <- if (ta[s] == tb[t]) 0L else 1L
      block_length <- base_block

      if (block_length < 1) {
        m[s + 1, t + 1] <- min(
          m[s + 1, t] + 1L,
          m[s, t + 1] + 1L,
          m[s, t] + this_cost
        )
      }

      while (block_length >= 1) {
        transposed <- s >= block_length * 2 && t >= block_length * 2 &&
          identical(
            ta[(s - block_length * 2 + 1):(s - block_length)],
            tb[(t - block_length + 1):t]
          ) &&
          identical(
            ta[(s - block_length + 1):s],
            tb[(t - block_length * 2 + 1):(t - block_length)]
          )

        if (transposed) {
          m[s + 1, t + 1] <- min(
            m[s + 1, t] + 1L,
            m[s, t + 1] + 1L,
            m[s + 1 - block_length * 2, t + 1 - block_length * 2] +
              this_cost + (block_length - 1L)
          )
          block_length <- 0
        } else if (block_length == 1) {
          m[s + 1, t + 1] <- min(
            m[s + 1, t] + 1L,
            m[s, t + 1] + 1L,
            m[s, t] + this_cost
          )
        } else {
          m[s + 1, t + 1] <- 0L
        }

        block_length <- block_length - 1
      }

      if (current_distance > m[s + 1, t + 1]) {
        current_distance <- m[s + 1, t + 1]
      }
    }

    # Early abort.  current_distance is the smallest cell seen across all
    # columns so far, never reset, so this can only fire on the first column.
    if (current_distance >= max_distance) {
      return(as.integer(current_distance))
    }
  }

  as.integer(m[len1 + 1, len2 + 1])
}

#' Modified Damerau-Levenshtein distance, vectorised
#'
#' Internal.  Compiled implementation of \code{tnrs_mdld_r()}, which is the one
#' the matcher uses.  Vectorised over both arguments, which are recycled to a
#' common length.
#'
#' @param s1,s2 Character vectors to compare element by element.
#' @param block_limit,max_distance See \code{tnrs_mdld_r()}.
#' @return Integer vector of edit distances, NA where either input is NA.
#' @useDynLib TNRS, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
#' @noRd
tnrs_mdld <- function(s1, s2, block_limit = 2L, max_distance = 4L) {
  n <- max(length(s1), length(s2))
  if (n == 0) {
    return(integer(0))
  }
  mdld_cpp(
    rep_len(as.character(s1), n),
    rep_len(as.character(s2), n),
    as.integer(block_limit),
    as.integer(max_distance)
  )
}
