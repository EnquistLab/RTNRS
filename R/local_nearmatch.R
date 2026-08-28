#' Produce the Rees (2007) "near match" phonetic key for a name
#'
#' Internal.  R port of the \code{near_match()} and \code{treat_word()} methods
#' of the TNRS \code{NearMatch} PHP class.  The key is the basis of both
#' candidate generation and the phonetic-match flag, so it must agree with
#' upstream exactly; the substitutions below are order dependent.
#'
#' @param x Character vector of names.
#' @param word_type One of "genus_only", "epithet_only", or NA (the default), the
#'   last of which treats the first word as a genus and all later words as
#'   epithets.
#' @return Character vector of phonetic keys, the same length as \code{x}.
#' @references Rees, T. (2014) Taxamatch, an algorithm for near ('fuzzy')
#'   matching of scientific names in biological databases. ZooKeys 477:1-14.
#' @keywords internal
#' @noRd
tnrs_near_match <- function(x, word_type = NA_character_) {
  x <- as.character(x)
  out <- rep("", length(x))

  todo <- !is.na(x) & trimws(x) != ""
  if (!any(todo)) {
    return(out)
  }

  temp <- tnrs_toupper_ascii(x[todo])

  if (identical(word_type, "genus_only")) {
    out[todo] <- tnrs_treat_word(temp, strip_ending = FALSE)
    return(out)
  }

  if (identical(word_type, "epithet_only")) {
    out[todo] <- tnrs_treat_word(temp, strip_ending = TRUE)
    return(out)
  }

  # Default: first word is treated as a genus (endings left alone), every later
  # word as an epithet.

  words <- strsplit(temp, " ", fixed = TRUE)

  out[todo] <- vapply(
    words,
    function(w) {
      if (length(w) == 0) {
        return("")
      }
      treated <- character(length(w))
      treated[1] <- tnrs_treat_word(w[1], strip_ending = FALSE)
      if (length(w) > 1) {
        treated[-1] <- tnrs_treat_word(w[-1], strip_ending = TRUE)
      }
      trimws(paste(treated, collapse = " "))
    },
    character(1)
  )

  out
}

#' Apply the near-match transformation to single words
#'
#' @param x Character vector of single words, already upper cased.
#' @param strip_ending Should variant epithet endings be normalized? Upstream
#'   does this for epithets but not for genus names.
#' @param normalize Should each word be passed through tnrs_normalize() first?
#' @keywords internal
#' @noRd
tnrs_treat_word <- function(x, strip_ending = FALSE, normalize = TRUE) {
  out <- rep("", length(x))

  todo <- !is.na(x) & trimws(x) != ""
  if (!any(todo)) {
    return(out)
  }

  temp <- x[todo]

  if (normalize) {
    temp <- tnrs_normalize(temp)
  }

  # Selective replacement on the leading letters only ("soundalikes").  Upstream
  # uses an if/elseif chain, so at most one of these fires per word.
  #
  # Upstream also carries a final "^ph" -> "^f" branch.  It is case sensitive and
  # lower case, and near_match() upper cases before calling this, so it can never
  # match.  It is omitted here as dead code.

  leading <- c(
    "^AE" = "E", "^CN" = "N", "^CT" = "Z", "^CZ" = "V", "^DJ" = "J",
    "^EA" = "E", "^EU" = "U", "^GN" = "N", "^KN" = "N", "^MC" = "MAC",
    "^MN" = "N", "^OE" = "E", "^QU" = "Q", "^PS" = "S", "^PT" = "T",
    "^TS" = "S", "^WR" = "R", "^X" = "Z"
  )

  unmatched <- rep(TRUE, length(temp))
  for (i in seq_along(leading)) {
    hit <- unmatched & grepl(names(leading)[i], temp)
    if (any(hit)) {
      temp[hit] <- sub(names(leading)[i], leading[[i]], temp[hit])
      unmatched[hit] <- FALSE
    }
  }

  # Quarantine the leading character, then equate soundalike groups on the
  # remainder.  These are applied in order and the order matters.

  start_letter <- substr(temp, 1, 1)
  rest <- substr(temp, 2, nchar(temp))

  subs <- c(
    "AE" = "I", "IA" = "A", "OE" = "I", "OI" = "A", "SC" = "S",
    "E" = "I", "O" = "A", "U" = "I", "Y" = "I", "K" = "C",
    "Z" = "S", "H" = ""
  )
  for (i in seq_along(subs)) {
    rest <- gsub(names(subs)[i], subs[[i]], rest, fixed = TRUE)
  }

  temp <- paste0(start_letter, rest)

  # Drop repeated characters: AA becomes A, BBB becomes B.

  temp <- gsub("(.)\\1+", "\\1", temp)

  # Variant endings -is (also -us, -ys, -es), -im and -as all become -a.  Only
  # applied to epithets, and only to keys of more than four characters.

  if (strip_ending) {
    long <- nchar(temp) > 4
    if (any(long)) {
      temp[long] <- sub("IS$", "A", temp[long])
      temp[long] <- sub("IM$", "A", temp[long])
      temp[long] <- sub("AS$", "A", temp[long])
    }
  }

  out[todo] <- temp
  out
}
