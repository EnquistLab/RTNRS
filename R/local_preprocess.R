#' Regular-expression alternation matching a family name
#'
#' Internal.  Single definition of what counts as a family name, used both when
#' stripping a family prefix in \code{tnrs_preprocess()} and when refusing to
#' treat a family as a genus in the parser.  Kept in one place so the two cannot
#' drift apart.
#'
#' Currently botanical: anything ending in -aceae, plus the classical
#' alternatives that upstream accepts.  A non-botanical backbone would need its
#' own endings here (-idae and -inae under the zoological code), which is why
#' this is a function rather than a literal.
#'
#' @return A single string: the body of an alternation, with no surrounding
#'   group and no anchors, so that callers can wrap it as they need.
#' @keywords internal
#' @noRd
tnrs_family_pattern <- function() {
  paste0(
    "[[:alpha:]]+aceae|Cruciferae|Guttiferae|Umbelliferae|Compositae|",
    "Leguminosae|Palmae|Labiatae|Gramineae|Mimosoideae|Papilionoideae|",
    "Caesalpinioideae"
  )
}

#' Pre-process submitted names before parsing
#'
#' Internal.  R port of the preamble of \code{Taxamatch::process()}, which runs
#' before the name reaches the parser.  It pulls off the leading junk, the
#' uncertainty annotations, the indeterminate markers and any family prefix,
#' leaving a bare name for the parser to split.
#'
#' @param x Character vector of submitted names.
#' @return A data.frame with one row per input and the columns
#'   \code{start_string} (leading non-alphabetic characters),
#'   \code{annotations} (cf./aff./? markers),
#'   \code{family} (title cased family name, or ""),
#'   \code{family_unmatched} (junk attached to the family token),
#'   \code{preprocessed} (text before the indeterminate markers were removed),
#'   and \code{cleaned} (what should be handed to the parser).
#' @keywords internal
#' @noRd
tnrs_preprocess <- function(x) {
  x <- as.character(x)
  n <- length(x)

  out <- data.frame(
    start_string = character(n), annotations = character(n),
    family = character(n), family_unmatched = character(n),
    preprocessed = character(n), cleaned = character(n),
    stringsAsFactors = FALSE
  )
  if (n == 0) {
    return(out)
  }

  txt <- ifelse(is.na(x), "", x)

  # "+" is accepted as a separator; tabs likewise
  txt <- gsub("+", " ", txt, fixed = TRUE)
  txt <- gsub("\t", " ", txt, fixed = TRUE)
  txt <- tnrs_reduce_spaces(txt)

  # Leading non-alphabetic characters are set aside rather than discarded
  start <- regmatches(txt, regexpr("^[^[:alpha:]]+", txt))
  has_start <- grepl("^[^[:alpha:]]+", txt)
  out$start_string[has_start] <- start
  txt <- sub("^[^[:alpha:]]+", "", txt)

  # Uncertainty annotations.  Upstream captures the first match then removes
  # every occurrence of that exact text.
  anno_re <- "(?:(?:\\s|^)(?:-?cf\\.?|vel\\.? sp\\.? aff\\.?|-?aff\\.?)(?:\\s|$))|(?:\\?+)"
  anno_hit <- regexpr(anno_re, txt, perl = TRUE, ignore.case = TRUE)
  has_anno <- anno_hit > 0
  if (any(has_anno)) {
    matched <- regmatches(txt, anno_hit)
    out$annotations[has_anno] <- trimws(matched)
    for (i in which(has_anno)) {
      txt[i] <- trimws(gsub(matched[which(which(has_anno) == i)], " ", txt[i], fixed = TRUE))
    }
  }

  txt <- gsub(" -", "-", txt, fixed = TRUE)
  txt <- gsub("- ", "-", txt, fixed = TRUE)

  out$preprocessed <- txt

  # Indeterminate markers
  indet_re <- paste0(
    "(?<=\\s|^)(?:\\S*[^[:alpha:][:space:]])?",
    "(indeterminad[ao]|undetermined|unknown|indet\\.?|sp\\.?\\s+nov\\.?|sp\\.?)",
    "(?:[^[:alpha:][:space:]]\\S*)?(?=\\s|$)"
  )
  txt <- gsub(indet_re, " ", txt, perl = TRUE, ignore.case = TRUE)
  txt <- tnrs_reduce_spaces(txt)

  # Leading family name.  Accepts anything ending in -aceae plus the classical
  # alternatives, and captures any junk stuck to the end of the token.
  # Group 1 is the whole token, group 2 the family name and group 3 any junk
  # stuck to it.  The literal words "fam" and "family" are matched here too,
  # then nulled out below; upstream does the same.
  fam_re <- paste0(
    "^((", tnrs_family_pattern(), "|fam(?:ily)?)",
    "((?:[^[:alpha:][:space:]]\\S*)?))(?=\\s+|$)"
  )

  fam <- regmatches(txt, regexec(fam_re, txt, perl = TRUE, ignore.case = TRUE))

  for (i in seq_len(n)) {
    groups <- fam[[i]]
    if (length(groups) == 0) {
      next
    }

    family <- groups[3]
    family_junk <- groups[4]

    # Title case, as upstream does
    family <- paste0(
      tnrs_toupper_ascii(substr(family, 1, 1)),
      tolower(substring(family, 2))
    )

    # The literal words "fam" and "family" are not families
    if (grepl("^fam(ily)?$", family, ignore.case = TRUE)) {
      family_junk <- paste0(family, family_junk)
      family <- ""
    }

    out$family[i] <- family
    out$family_unmatched[i] <- family_junk
    txt[i] <- trimws(sub(groups[1], "", txt[i], fixed = TRUE))
  }

  out$cleaned <- tnrs_reduce_spaces(txt)
  out
}
