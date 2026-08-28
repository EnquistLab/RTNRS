#' Transliterate accented characters to ASCII
#'
#' Internal.  R port of \code{Normalize::utf8_to_ascii()} from TNRSbatch.
#'
#' Upstream applies a hand-rolled table rather than a general transliteration, so
#' the table is reproduced verbatim, including its quirks: the sharp s maps to an
#' upper case "B", and a few characters appear twice in the source lists.  Note
#' that this is used for \emph{authority} strings only.  Scientific name parts go
#' through \code{tnrs_normalize()}, which deletes accented characters instead.
#'
#' @param x Character vector.
#' @return Character vector with accented characters replaced.
#' @keywords internal
#' @noRd
tnrs_utf8_to_ascii <- function(x) {
  # Expansions to more than one character, done first.  All of the characters
  # involved are disjoint from the single-character table below, so the order
  # matches upstream despite the regrouping.
  x <- gsub(intToUtf8(0x00C6), "AE", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x0152), "OE", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x00DF), "B", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x00E6), "ae", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x0153), "oe", x, fixed = TRUE)

  tbl <- tnrs_ascii_table()
  chartr(tbl$from, tbl$to, x)
}

#' Single-character transliteration table, built by code point
#'
#' Kept in a function with a cached result so the package source stays ASCII.
#' @keywords internal
#' @noRd
tnrs_ascii_table <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    map <- list(
      A = c(0x00C0, 0x00C2, 0x00C5, 0x00C3, 0x00C4, 0x00C1, 0x1EA4, 0x1EA0),
      E = c(0x00C9, 0x00C8, 0x00CA, 0x00CB),
      I = c(0x00CD, 0x00CC, 0x00CE, 0x00CF),
      O = c(0x00D3, 0x00D2, 0x00D4, 0x00D8, 0x00D5, 0x00D6, 0x1EDA, 0x1ED4),
      U = c(0x00DA, 0x00D9, 0x00DB, 0x00DC),
      Y = c(0x00DD),
      C = c(0x010C, 0x00C7),
      S = c(0x0160, 0x015E),
      D = c(0x0110),
      Z = c(0x017D),
      N = c(0x00D1),
      K = c(0x0136),
      R = c(0x0158),
      a = c(
        0x00E1, 0x00E0, 0x00E2, 0x00E5, 0x00E3, 0x00E4, 0x0103, 0x1EAF,
        0x1EA3, 0x1EA1, 0x1EAD, 0x1EA7, 0x1EB1
      ),
      e = c(
        0x00E9, 0x00E8, 0x00EA, 0x00EB, 0x0115, 0x011B, 0x1EBF, 0x1EC7,
        0x1EC3, 0x1EC5, 0x1EC1, 0x1EBB
      ),
      i = c(0x00ED, 0x00EC, 0x00EE, 0x00EF, 0x01D0, 0x012D, 0x012B, 0x0129, 0x1EC9),
      o = c(
        0x00F3, 0x00F2, 0x00F4, 0x00F8, 0x00F5, 0x00F6, 0x014F, 0x1ECF,
        0x1ED7, 0x1ED9, 0x01A1, 0x1ECD, 0x1EE1, 0x1ED1, 0x1ED3, 0x1EDD,
        0x1EDB, 0x1ED5
      ),
      u = c(
        0x00FA, 0x00F9, 0x00FB, 0x00FC, 0x016F, 0x01B0, 0x1EEB, 0x1EF1,
        0x1EE7, 0x1EE9, 0x1EE5
      ),
      z = c(0x017E, 0x017A),
      y = c(0x00FD, 0x00FF, 0x1EF9),
      d = c(0x0111),
      c = c(0x010D, 0x0107, 0x00E7),
      n = c(0x00F1, 0x0144, 0x0148),
      s = c(0x015B, 0x0161, 0x015F),
      r = c(0x0159),
      g = c(0x011F)
    )

    from_cp <- unlist(map, use.names = FALSE)
    to_ch <- rep(names(map), lengths(map))

    # A few code points appear in more than one upstream list; keep the first.
    keep <- !duplicated(from_cp)

    cache <<- list(
      from = intToUtf8(from_cp[keep]),
      to = paste(to_ch[keep], collapse = "")
    )
    cache
  }
})

#' Normalize an authority string
#'
#' Internal.  R port of \code{Normalize::normalize_auth()} from TNRSbatch.
#'
#' Upstream contains a lookup that expands abbreviated author names against an
#' \code{auth_abbrev} database table, but the call is commented out in the
#' shipped code, so this function needs no reference data.
#'
#' @param x Character vector of authority strings.
#' @param upcase Should the result be upper cased? Upstream defaults to TRUE.
#' @return Character vector of normalized authorities.
#' @keywords internal
#' @noRd
tnrs_normalize_auth <- function(x, upcase = TRUE) {
  x <- as.character(x)
  out <- rep("", length(x))

  todo <- !is.na(x) & trimws(x) != ""
  if (!any(todo)) {
    return(out)
  }

  temp <- trimws(x[todo])

  # Special cases, an if/elseif chain upstream so at most one applies.  The
  # substring offsets deliberately reproduce upstream's, including the missing
  # space after "(Linnaeus)".

  done <- rep(FALSE, length(temp))

  hit <- !done & temp == "L."
  temp[hit] <- "Linnaeus"
  done <- done | hit

  hit <- !done & grepl("^\\(L\\.\\)", temp)
  temp[hit] <- paste0("(Linnaeus)", substring(temp[hit], 6))
  done <- done | hit

  hit <- !done & (grepl("^L\\., 1", temp) | grepl("^L\\. 1", temp))
  temp[hit] <- paste0("Linnaeus ", substring(temp[hit], 4))
  done <- done | hit

  hit <- !done & (grepl("^\\(L\\., 1", temp) | grepl("^\\(L\\. 1", temp))
  temp[hit] <- paste0("(Linnaeus ", substring(temp[hit], 5))
  done <- done | hit

  hit <- !done & (temp == "DC" | temp == "(DC)")
  temp[hit] <- gsub("DC", "de Candolle", temp[hit], fixed = TRUE)
  done <- done | hit

  hit <- !done & (temp == "D.C." | temp == "(D.C.)")
  temp[hit] <- gsub("D.C.", "de Candolle", temp[hit], fixed = TRUE)

  # Add a space after full stops, except at the end

  temp <- sub("[ \t\r\n]+$", "", gsub(".", ". ", temp, fixed = TRUE))

  # Normalize "et" and "and" to an ampersand, protecting "et al" first

  temp <- gsub(" et al", "zzzzz", temp, fixed = TRUE)
  temp <- gsub(" et ", " & ", temp, fixed = TRUE)
  temp <- gsub(" and ", " & ", temp, fixed = TRUE)
  temp <- gsub("zzzzz", " et al", temp, fixed = TRUE)

  # Remove commas before dates only

  for (century in c("17", "18", "19", "20")) {
    temp <- gsub(paste0(", ", century), paste0(" ", century), temp, fixed = TRUE)
  }

  temp <- tnrs_reduce_spaces(temp)
  temp <- gsub(" -", "-", temp, fixed = TRUE)

  # Upstream now loops over words to expand author abbreviations from the
  # database.  That lookup is commented out upstream, which leaves the loop as
  # an identity apart from spacing, so it is omitted here.

  temp <- tnrs_reduce_spaces(gsub(" )", ")", temp, fixed = TRUE))

  out[todo] <- if (upcase) tnrs_toupper_ascii(temp) else temp
  out
}

#' N-gram similarity between two strings
#'
#' Internal.  R port of \code{Taxamatch::ngram()}: Dice's coefficient over
#' n-grams, with the strings padded by \code{n - 1} spaces at each end so that
#' terminal characters are not under-weighted.  Repeated n-grams are counted with
#' multiplicity.  Case sensitive by design.
#'
#' @param source_string,target_string Single strings.
#' @param n Size of the n-grams.
#' @return Similarity on a 0-1 scale, rounded to four decimal places.
#' @keywords internal
#' @noRd
tnrs_ngram <- function(source_string, target_string, n = 1) {
  if (is.na(source_string) || is.na(target_string)) {
    return(NA_real_)
  }

  padding <- strrep(" ", n - 1)

  grams <- function(s) {
    n_grams <- nchar(s) + n - 1
    if (n_grams <= 0) {
      return(character(0))
    }
    padded <- paste0(padding, s, padding)
    substring(padded, seq_len(n_grams), seq_len(n_grams) + n - 1)
  }

  src <- grams(source_string)
  tgt <- grams(target_string)

  src_counts <- table(src)
  tgt_counts <- table(tgt)

  shared <- intersect(names(src_counts), names(tgt_counts))
  match_count <- sum(pmin(src_counts[shared], tgt_counts[shared]))

  denom <- length(src) + length(tgt)
  if (denom == 0) {
    return(0)
  }

  round(2 * match_count / denom, 4)
}

#' Compare two authority strings
#'
#' Internal.  R port of \code{Taxamatch::compare_auth()}, which is what produces
#' the \code{Author_score} column.  A blend of two thirds bigram and one third
#' trigram similarity, averaged across versions with and without diacritics so
#' that accents count for half as much.
#'
#' @param auth1,auth2 Character vectors of authority strings, recycled to a
#'   common length.
#' @return Numeric similarity on a 0-1 scale, or NA where either authority is
#'   missing or empty (upstream returns NULL, and the aggregator then omits the
#'   author term from the overall score entirely).
#' @keywords internal
#' @noRd
tnrs_compare_auth <- function(auth1, auth2) {
  n <- max(length(auth1), length(auth2))
  auth1 <- rep_len(as.character(auth1), n)
  auth2 <- rep_len(as.character(auth2), n)

  out <- rep(NA_real_, n)

  # Upstream compares against NULL loosely, so an empty string counts as absent
  todo <- !is.na(auth1) & !is.na(auth2) & nzchar(auth1) & nzchar(auth2)
  if (!any(todo)) {
    return(out)
  }

  a1 <- tnrs_normalize_auth(auth1[todo])
  a2 <- tnrs_normalize_auth(auth2[todo])

  scores <- numeric(length(a1))

  identical_auth <- a1 == a2
  scores[identical_auth] <- 1

  for (i in which(!identical_auth)) {
    a1b <- tnrs_utf8_to_ascii(a1[i])
    a2b <- tnrs_utf8_to_ascii(a2[i])

    m1 <- (2 * tnrs_ngram(a1[i], a2[i], 2) + tnrs_ngram(a1[i], a2[i], 3)) / 3

    m2 <- if (a1[i] == a1b && a2[i] == a2b) {
      m1
    } else {
      (2 * tnrs_ngram(a1b, a2b, 2) + tnrs_ngram(a1b, a2b, 3)) / 3
    }

    scores[i] <- (m1 + m2) / 2
  }

  out[todo] <- round(scores, 4)
  out
}
