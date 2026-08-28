#' Normalize a taxonomic name string
#'
#' Internal.  R port of the \code{normalize()} method of the TNRS
#' \code{Normalize} PHP class, used both to build the searchable name columns of
#' the local reference data and as the input to the phonetic key.
#'
#' The port is deliberately faithful rather than sensible, so that locally
#' generated scores agree with those returned by the API.  In particular:
#' characters outside \code{A-Z}, space and full stop are deleted rather than
#' transliterated (so accented letters simply disappear, and hyphens are closed
#' up), only the first parenthesised and first bracketed group is dropped, and
#' the authority portion is returned unaltered.
#'
#' @param x Character vector of names.
#' @return Character vector of normalized names, the same length as \code{x}.
#' @keywords internal
#' @noRd
tnrs_normalize <- function(x) {
  x <- as.character(x)
  out <- rep("", length(x))

  todo <- !is.na(x) & trimws(x) != ""
  if (!any(todo)) {
    return(out)
  }

  temp <- trimws(x[todo])

  # Replace any HTML ampersands.  Upstream applies four replacements in
  # sequence; because "%" is replaced first, the later patterns can no longer
  # match, so the net effect is simply "%" -> "&".

  temp <- gsub("%", "&", temp, fixed = TRUE)

  # Remove any content in angle brackets (html tags and anything else)

  temp <- gsub("<(/?[^>]+)>", "", temp)

  # If the second term is in round brackets, presume it is a subgenus or a
  # comment and remove it.  Likewise for square brackets.  First match only.

  temp <- sub(" \\(\\w*\\W*\\)", "", temp)
  temp <- sub(" \\[\\w*\\W*\\]", "", temp)

  # Drop indicators of questionable identifications.  Upstream treats these as
  # case sensitive and requires the surrounding spaces, so we do too.

  for (pattern in c(" cf ", " cf\\. ", " near ", " aff\\. ", " sp\\. ", " spp\\. ", " spp ")) {
    temp <- gsub(pattern, " ", temp)
  }

  temp <- tnrs_reduce_spaces(temp)

  # First element is taken to be the genus, the second the specific epithet, and
  # anything remaining the authority.

  parts <- tnrs_split_n(temp, 3)
  genus <- parts[, 1]
  species <- parts[, 2]
  authority <- parts[, 3]

  # Genus and epithet are treated together from here on.  Upstream uses PHP's
  # strtoupper(), which only touches ASCII, so we do the same rather than using
  # toupper().

  genus_species <- tnrs_toupper_ascii(sub("[ \t\r\n]+$", "", paste(genus, species)))

  # Replace selected ligatures, then drop anything that is not A-Z, a space or a
  # full stop.  This is a byte-wise filter upstream, so accented characters lose
  # both of their UTF-8 bytes and vanish entirely.

  # The two ligatures upstream replaces, built by code point so that this file
  # stays ASCII: U+00C6 is the AE ligature and U+0152 the OE ligature.

  genus_species <- gsub(intToUtf8(0x00C6), "AE", genus_species, fixed = TRUE)
  genus_species <- gsub(intToUtf8(0x0152), "OE", genus_species, fixed = TRUE)
  genus_species <- tnrs_good_chars(genus_species)
  genus_species <- tnrs_reduce_spaces(genus_species)

  # The authority is appended unaltered: it is neither upper-cased nor filtered.

  out[todo] <- trimws(paste(genus_species, authority))
  out
}

#' Reduce runs of spaces to a single space and trim
#' @keywords internal
#' @noRd
tnrs_reduce_spaces <- function(x) {
  trimws(gsub(" {2,}", " ", x))
}

#' ASCII-only upper casing, matching PHP's strtoupper()
#' @keywords internal
#' @noRd
tnrs_toupper_ascii <- function(x) {
  chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)
}

#' Keep only the bytes A-Z, space and full stop
#'
#' Upstream iterates over bytes and keeps only ordinals 65-90, 32 and 46.
#' Matching on bytes is what makes multi-byte characters disappear completely,
#' so useBytes is set deliberately.
#' @keywords internal
#' @noRd
tnrs_good_chars <- function(x) {
  gsub("[^A-Z .]", "", x, useBytes = TRUE)
}

#' Split each string into at most n space-separated fields
#'
#' Mimics repeated PHP explode(" ", x, 2) calls: the final field absorbs the
#' remainder of the string, and missing fields become empty strings.
#' @return A character matrix with n columns and length(x) rows.
#' @keywords internal
#' @noRd
tnrs_split_n <- function(x, n) {
  out <- matrix("", nrow = length(x), ncol = n)
  rest <- x
  for (i in seq_len(n - 1)) {
    pos <- regexpr(" ", rest, fixed = TRUE)
    has <- pos > 0
    out[, i] <- ifelse(has, substr(rest, 1, pos - 1), rest)
    rest <- ifelse(has, substr(rest, pos + 1, nchar(rest)), "")
  }
  out[, n] <- rest
  out
}
