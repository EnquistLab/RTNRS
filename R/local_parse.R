#' Standard infraspecific rank abbreviations
#'
#' Internal.  R port of \code{TnrsAggregator::$standard_rank}: maps the many
#' spellings of each rank indicator onto the canonical abbreviation.
#'
#' @return Named character vector, names being the accepted spellings.
#' @keywords internal
#' @noRd
tnrs_standard_ranks <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    cache <<- c(
      "agsp" = "agsp.", "agsp." = "agsp.",
      "convar" = "convar.", "convar." = "convar.",
      "cult" = "cv.", "cult." = "cv.", "cultivar" = "cv.",
      "cv" = "cv.", "cv." = "cv.",
      "fo" = "fo.", "fo." = "fo.", "f." = "fo.", "forma" = "fo.",
      "grex" = "grex", "lusus" = "lusus", "monstr" = "monstr.",
      "nothogen" = "nothogen.", "nothomorph" = "nothomorph",
      "nothosect" = "nothosect.", "nothosect." = "nothosect.",
      "nothoser" = "nothoser.", "nothoser." = "nothoser.",
      "nothosubgen" = "nothosubgen.", "nothosubgen." = "nothosubgen.",
      "nothosbgen" = "nothosubgen.",
      "nothosubsp" = "nothosubsp.", "nothosubsp." = "nothosubsp.",
      "nothosbsp" = "nothosubsp.", "nothosbsp." = "nothosubsp.",
      "nothossp" = "nothosubsp.", "nothossp." = "nothosubsp.",
      "nothovar" = "nothovar.", "nothovar." = "nothovar.",
      "proles" = "proles", "race" = "race", "rasse" = "race",
      "sect" = "sect.", "sect." = "sect.",
      "ser" = "ser.", "ser." = "ser.",
      "sport" = "sport", "stirps" = "stirps",
      "subfo" = "subfo.", "subfo." = "subfo.", "subf" = "subfo.",
      "subf." = "subfo.", "subforma." = "subfo.", "sbfo." = "subfo.",
      "sbforma" = "subfo.",
      "subgen" = "subgen.", "subgen." = "subgen.",
      "subsect" = "subsect.", "subsect." = "subsect.",
      "subser" = "subser.", "subser." = "subser.",
      "subsp" = "subsp.", "subsp." = "subsp.", "sbsp" = "subsp.",
      "sbsp." = "subsp.", "ssp" = "subsp.", "ssp." = "subsp.",
      "subspecies" = "subsp.",
      "substirps" = "substirps",
      "subvar" = "subvar.", "subvar." = "subvar.",
      "supersect." = "supersect.",
      "var" = "var.", "var." = "var.", "variety" = "var."
    )
    cache
  }
})

#' Does a token look like a family name?
#'
#' Shares its definition with the family prefix stripped in
#' \code{tnrs_preprocess()}, via \code{tnrs_family_pattern()}.
#' @keywords internal
#' @noRd
tnrs_is_family_token <- function(x, codes = "botanical") {
  grepl(paste0("^(?:", tnrs_family_pattern(codes), ")$"), x, ignore.case = TRUE)
}

#' Is a token a standalone hybrid marker?
#'
#' Matches a lone "x" or "X" and the multiplication sign U+00D7, as used to mark
#' a named hybrid.  A marker fused to the epithet is not covered here.
#' @keywords internal
#' @noRd
tnrs_is_hybrid_marker <- function(x) {
  x %in% c("x", "X", intToUtf8(0x00D7))
}

#' Standardize a rank indicator, returning "" if it is not one
#' @keywords internal
#' @noRd
tnrs_standardize_rank <- function(x) {
  ranks <- tnrs_standard_ranks()
  hit <- ranks[tolower(x)]
  unname(ifelse(is.na(hit), "", hit))
}

#' Parse taxonomic names into their components
#'
#' Internal.  Splits pre-processed names into genus, specific epithet, up to two
#' infraspecific epithets with their rank indicators, and the authority.
#'
#' The TNRS server uses GNparser for this step.  Where the \pkg{rgnparser}
#' package and its binary are available they are used, for fidelity; otherwise an
#' internal parser is used, which handles conventionally formed names but will
#' differ from the server on unusual input.  The parser actually used is returned
#' in the \code{parser} column so that any divergence is visible rather than
#' silent.
#'
#' @param x Character vector of names, already passed through
#'   \code{tnrs_preprocess()}.
#' @param parser One of "auto" (use gnparser when available), "gnparser", or
#'   "internal".
#' @return A data.frame with columns \code{genus}, \code{species}, \code{rank1},
#'   \code{infra1}, \code{rank2}, \code{infra2}, \code{authorship},
#'   \code{unmatched} and \code{parser}.
#' @keywords internal
#' @noRd
tnrs_parse <- function(x, parser = c("auto", "gnparser", "internal"),
                       codes = "botanical") {
  parser <- match.arg(parser)

  if (parser == "gnparser" && !tnrs_gnparser_available()) {
    stop(
      "parser = \"gnparser\" requires the rgnparser package and the gnparser ",
      "binary. Install with install.packages(\"rgnparser\") then ",
      "rgnparser::install_gnparser(), or use parser = \"internal\"."
    )
  }

  if (parser == "auto") {
    parser <- if (tnrs_gnparser_available()) "gnparser" else "internal"
  }

  if (parser == "gnparser") {
    return(tnrs_parse_gnparser(x, codes))
  }

  tnrs_parse_internal(x, codes)
}

#' Is GNparser usable in this session?
#' @keywords internal
#' @noRd
tnrs_gnparser_available <- function() {
  if (!requireNamespace("rgnparser", quietly = TRUE)) {
    return(FALSE)
  }

  # The R package is only half of it: the gnparser binary is installed
  # separately by rgnparser::install_gnparser(), and gn_version() is the
  # exported call that fails when it is missing.
  version <- tryCatch(rgnparser::gn_version(), error = function(e) NULL)

  !is.null(version) && length(version) > 0 && any(nzchar(as.character(version)))
}

#' Parse using the internal regular-expression parser
#'
#' Handles conventionally formed names: an initial capitalised genus, a lower
#' case specific epithet, up to two infraspecific epithets each optionally
#' preceded by a rank indicator, and an authority making up the remainder.
#' Where the whole name is upper case it is case-normalized first, which is what
#' GNparser does.
#' @keywords internal
#' @noRd
tnrs_parse_internal <- function(x, codes = "botanical") {
  x <- as.character(x)
  n <- length(x)

  out <- data.frame(
    genus = character(n), species = character(n),
    rank1 = character(n), infra1 = character(n),
    rank2 = character(n), infra2 = character(n),
    authorship = character(n), unmatched = character(n),
    parser = rep("internal", n),
    stringsAsFactors = FALSE
  )
  if (n == 0) {
    return(out)
  }

  txt <- ifelse(is.na(x), "", tnrs_reduce_spaces(x))

  for (i in seq_len(n)) {
    if (!nzchar(txt[i])) {
      next
    }

    tokens <- strsplit(txt[i], " ", fixed = TRUE)[[1]]
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) {
      next
    }

    # Keep the submitted forms so that the authority can be reported as given
    raw_tokens <- tokens

    # Capitalisation is what separates an authority from an epithet, so a name
    # written wholly in one case carries no such signal.  Those names are
    # case-normalized, and the ambiguous rules below are suppressed.
    caseless <- !grepl("[a-z]", txt[i]) || !grepl("[A-Z]", txt[i])
    if (caseless) {
      tokens <- tolower(tokens)
    }

    # A family name is not a genus.  Pre-processing strips only one leading
    # family, so a repeated one ("Vochysiaceae Vochysiaceae Qualea grandiflora")
    # would otherwise be taken as the genus.
    while (length(tokens) > 1 && tnrs_is_family_token(tokens[1], codes)) {
      tokens <- tokens[-1]
      raw_tokens <- raw_tokens[-1]
    }

    # Genus: capitalised first token
    out$genus[i] <- paste0(
      tnrs_toupper_ascii(substr(tokens[1], 1, 1)),
      tolower(substring(tokens[1], 2))
    )
    pos <- 2L

    is_epithet <- function(tok) {
      grepl("^[a-z][a-z-]*$", tok) && nchar(tok) > 1 &&
        !nzchar(tnrs_standardize_rank(tok))
    }

    # A standalone hybrid marker is not an epithet and not an author; skip it so
    # that "Platanus x acerifolia" yields the epithet acerifolia.  A marker fused
    # to the epithet ("xacerifolia") is left alone, which is what both the API
    # and GNparser do.
    if (pos <= length(tokens) && tnrs_is_hybrid_marker(tokens[pos]) &&
      pos + 1L <= length(tokens) && is_epithet(tokens[pos + 1L])) {
      pos <- pos + 1L
    }

    # Specific epithet
    if (pos <= length(tokens) && is_epithet(tokens[pos])) {
      out$species[i] <- tokens[pos]
      pos <- pos + 1L
    }

    # Up to two infraspecific epithets, each optionally preceded by a rank.
    # A name may legitimately carry an authority before the infraspecific part,
    # as in "Talisia clathrata Radlk subsp. clathrata", so when a rank indicator
    # appears further along the token stream the intervening words are taken as
    # the authority of the species.
    slot <- 1L
    author_pos <- integer(0)

    while (pos <= length(tokens) && slot <= 2L) {
      rank <- tnrs_standardize_rank(tokens[pos])

      if (!nzchar(rank) && nzchar(out$species[i])) {
        ahead <- which(nzchar(tnrs_standardize_rank(tokens)) &
          seq_along(tokens) > pos)
        ahead <- ahead[ahead < length(tokens)]
        ahead <- ahead[vapply(ahead, function(k) is_epithet(tokens[k + 1L]), logical(1))]

        if (length(ahead) > 0 && !is_epithet(tokens[pos])) {
          author_pos <- c(author_pos, pos:(ahead[1] - 1L))
          pos <- ahead[1]
          rank <- tnrs_standardize_rank(tokens[pos])
        }
      }

      if (nzchar(rank) && pos + 1L <= length(tokens) && is_epithet(tokens[pos + 1L])) {
        out[[paste0("rank", slot)]][i] <- rank
        out[[paste0("infra", slot)]][i] <- tokens[pos + 1L]
        pos <- pos + 2L
      } else if (!caseless && nzchar(out$species[i]) && is_epithet(tokens[pos])) {
        # A bare trailing epithet becomes an infraspecific name with no rank.
        # This relies on capitalisation to tell an epithet from an authority, so
        # it is suppressed for names written wholly in one case: in
        # "ARGEMONE INTERMEDIA SWEET", Sweet is the naming authority, and the
        # API's own answer for the correctly cased name confirms it.
        out[[paste0("infra", slot)]][i] <- tokens[pos]
        pos <- pos + 1L
      } else {
        break
      }
      slot <- slot + 1L
    }

    # Whatever is left is the authority
    if (pos <= length(tokens)) {
      author_pos <- c(author_pos, pos:length(tokens))
    }

    if (length(author_pos) > 0) {
      # Reported as submitted, rather than case-normalized
      author_tokens <- raw_tokens[author_pos]
      # Cultivar epithets are quoted; the server keeps the text but not the
      # quotes, e.g. "Farw. 'Spring Dew'" becomes "Farw. Spring Dew"
      out$authorship[i] <- tnrs_reduce_spaces(
        gsub("'", "", paste(author_tokens, collapse = " "), fixed = TRUE)
      )
    }
  }

  out
}

#' Parse using GNparser via the rgnparser package
#'
#' GNparser establishes where the name ends and the authority begins, which is
#' the part the internal parser is weakest at; the resulting canonical name is
#' then split into its components by the same logic as the internal parser.
#' @keywords internal
#' @noRd
tnrs_parse_gnparser <- function(x, codes = "botanical") {
  x <- as.character(x)
  n <- length(x)
  if (n == 0) {
    return(tnrs_parse_internal(x))
  }

  txt <- ifelse(is.na(x), "", x)

  # A family name is not part of the name.  Pre-processing removes one leading
  # family, but a repeated one would otherwise be parsed as the name itself:
  # GNparser reads "Vochysiaceae Qualea grandiflora" as the genus Vochysiaceae.
  for (i in seq_len(n)) {
    tokens <- strsplit(txt[i], " ", fixed = TRUE)[[1]]
    tokens <- tokens[nzchar(tokens)]
    while (length(tokens) > 1 && tnrs_is_family_token(tokens[1], codes)) {
      tokens <- tokens[-1]
    }
    txt[i] <- paste(tokens, collapse = " ")
  }

  # gn_parse_tidy reads its results with readr, which is chatty
  parsed <- suppressMessages(rgnparser::gn_parse_tidy(txt))

  canonical <- as.character(parsed$canonicalfull)
  canonical[is.na(canonical)] <- ""
  authorship <- as.character(parsed$authorship)
  authorship[is.na(authorship)] <- ""

  out <- tnrs_parse_internal(canonical)
  out$authorship <- authorship

  # GNparser requires a capitalised genus and returns nothing for names that are
  # wholly upper or lower case.  The TNRS server's own parser is more forgiving,
  # so fall back to the internal parser rather than dropping the name.
  empty <- !nzchar(canonical)
  if (any(empty)) {
    out[empty, ] <- tnrs_parse_internal(txt[empty])
  }

  out$parser <- ifelse(empty, "internal", "gnparser")
  out
}
