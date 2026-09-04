#' Triage names by comparing two resolutions of them
#'
#' Two independent readings of the same names, from two sources, two
#' services or two settings, agree on most and disagree on a few, and the
#' pattern of disagreement says which names need a person. This function
#' sorts each name into one of seven tiers, from "both agree at species"
#' down to "both placed it and they contradict each other", and suggests
#' which reading to take where one is clearly the fuller.
#'
#' Measured on 16,600 raw GBIF names for mosquitoes and ticks, resolved by
#' \code{TNRS_local()} against the Catalogue of Life on one side and by
#' GBIF's backbone on the other, the tiers came out as: both agree at
#' species, 76\% of records; agree at genus or family, 15\%; different
#' strings for one taxon, 1\%; vague strings, 3\%; one side deeper than
#' the other, 5\%; and a genuine contradiction, well under 1\%. The last
#' tier is the review queue.
#'
#' @param a,b Two resolutions of the same names, one row per name in the
#'   same order, as returned by \code{TNRS_local()} or \code{TNRS()} with
#'   \code{matches = "best"}, or any data.frame with the columns
#'   \code{Name_submitted}, \code{Name_matched}, \code{Name_matched_rank}
#'   and \code{Accepted_name}. A reading from elsewhere, GBIF's backbone
#'   say, is passed by putting it in those columns. Where both carry
#'   \code{Name_submitted} the names are checked to be the same. The
#'   \code{Author_score} and \code{Warnings} columns of \code{a} are used
#'   when present, so \code{a} is the side to put a \code{TNRS_local()}
#'   result on.
#' @return A data.frame with one row per name: \code{Name_submitted},
#'   \code{Tier} (1 to 7), \code{Tier_label}, \code{Detail} (the rule that
#'   placed it there), the matched name, rank and accepted name from each
#'   side as \code{Name_a}, \code{Rank_a}, \code{Accepted_a}, \code{Name_b},
#'   \code{Rank_b}, \code{Accepted_b}, and \code{Suggested_name},
#'   \code{Suggested_rank} and \code{Suggested_from} ("both", "a", "b" or
#'   "") giving the reading to take where one can be recommended.
#' @section The tiers:
#' \describe{
#'   \item{1, same species, same accepted name}{Accept.}
#'   \item{2, same species, different accepted name}{The match is not in
#'     doubt; the two backbones disagree on synonymy. Choose a backbone
#'     once rather than reviewing each name.}
#'   \item{3, agree above species}{Both read the name to the same genus
#'     or family. Accept at that rank.}
#'   \item{4, different strings, same taxon}{Two synonyms of one accepted
#'     name, or a spelling variant of the epithet that the author
#'     confirms. Accept.}
#'   \item{5, unresolvable string}{A bare genus with sp., a slash or
#'     hybrid formula, cf. or aff., a barcode identifier, or a string
#'     neither side could read. Accept at the genus where one is given,
#'     otherwise drop; nothing to review.}
#'   \item{6, one side deeper}{One side found nothing or stopped at the
#'     genus; the other went further within the same genus. Take the
#'     deeper reading, keeping a note that the two differed.}
#'   \item{7, contradiction}{Both placed the name and disagree: a
#'     different genus, a different species in the same genus, or a fuzzy
#'     match whose author contradicts it (the \code{[Author]} warning of
#'     \code{TNRS_local()}). Review.}
#' }
#' @section Using it with a GBIF download:
#' GBIF interprets every occurrence record against its backbone, and the
#' download carries both the string the publisher gave and GBIF's reading
#' of it. That makes a download a ready-made second opinion. The steps:
#' \enumerate{
#'   \item Request the download with \code{rgbif::occ_download(pred("taxonKey",
#'     key), format = "SIMPLE_CSV")} and read it back. The columns needed are
#'     \code{verbatimScientificName}, \code{verbatimScientificNameAuthorship},
#'     \code{taxonKey}, \code{scientificName} and \code{taxonRank}.
#'   \item Reduce to distinct strings. Millions of records are usually a few
#'     thousand distinct names, and one record's reading stands for all
#'     that share its string. Where records with the same string were read
#'     differently, take the majority.
#'   \item GBIF's \code{scientificName} carries the author and the download
#'     has no accepted name; look each distinct \code{taxonKey} up with
#'     \code{rgbif::name_usage(key)}, which gives \code{canonicalName},
#'     \code{rank}, \code{taxonomicStatus} and the accepted name. Put those
#'     in a data.frame with the columns \code{Name_matched},
#'     \code{Name_matched_rank} and \code{Accepted_name}; a record GBIF could
#'     not place below the group is \code{"[No match found]"}.
#'   \item Resolve the strings with \code{TNRS_local()}, with \code{within}
#'     set to the group downloaded and the authorship field appended to
#'     any string that lacks an author, so the \code{[Author]} warning has
#'     something to work with.
#'   \item Call \code{TNRS_triage(local, gbif)} and join the tier back to
#'     the records by the raw string.
#' }
#' The dataset \code{\link{gbif_triage_sample}} is a few hundred rows of
#' exactly this, already reduced and looked up, for mosquitoes and ticks;
#' the example below runs on it.
#' @note Names are compared after dropping authorship, subgenus in
#'   parentheses, rank connectors and case, so \code{Aedes (Stegomyia)
#'   aegypti} and \code{Aedes aegypti} agree. An epithet within two edits
#'   of the other side's is treated as a spelling variant only when the
#'   author on side \code{a} scores at least 0.5; without an author it is a
#'   contradiction, since \code{Ixodes barkeri} and \code{Ixodes bakeri}
#'   are also two edits apart and are different ticks.
#' @seealso \code{\link{gbif_triage_sample}} for the worked input;
#'   \code{\link{TNRS_local}} for \code{within} and the \code{[Author]}
#'   warning.
#' @export
#' @examples \dontrun{
#' # Ticks from GBIF against the Catalogue of Life.  Needs the "col" source:
#' # TNRS_local_build("col") once, about 500 MB.
#' ticks <- gbif_triage_sample[gbif_triage_sample$group == "Ixodida", ]
#'
#' # The raw string, with GBIF's separate author field appended where the
#' # string itself does not already carry it
#' author <- ticks$verbatimScientificNameAuthorship
#' already <- mapply(function(a, s) nzchar(a) && grepl(a, s, fixed = TRUE),
#'                   author, ticks$verbatimScientificName)
#' submitted <- ifelse(nzchar(author) & !already,
#'   paste(ticks$verbatimScientificName, author), ticks$verbatimScientificName
#' )
#' local <- TNRS_local(submitted, sources = "col", within = "Ixodida")
#'
#' # GBIF's reading of the same strings, in the shape TNRS_triage() reads
#' gbif <- data.frame(
#'   Name_submitted = local$Name_submitted,
#'   Name_matched = ifelse(nzchar(ticks$gbif_name), ticks$gbif_name, "[No match found]"),
#'   Name_matched_rank = ticks$gbif_rank,
#'   Accepted_name = ticks$gbif_accepted_name
#' )
#'
#' tiers <- TNRS_triage(local, gbif)
#' table(tiers$Tier)
#' # The review queue, largest first
#' queue <- tiers[tiers$Tier == 7, ]
#' queue[order(-ticks$n_records[tiers$Tier == 7]), c("Name_submitted", "Detail", "Name_a", "Name_b")]
#'
#' # Or compare two sources with each other, no GBIF involved
#' both <- TNRS_triage(
#'   TNRS_local(names, sources = "wcvp"),
#'   TNRS_local(names, sources = "wfo")
#' )
#' }
TNRS_triage <- function(a, b) {
  a <- tnrs_triage_side(a, "a")
  b <- tnrs_triage_side(b, "b")

  if (nrow(a) != nrow(b)) {
    stop("a and b must have one row per name, in the same order; they have ",
      nrow(a), " and ", nrow(b), " rows", call. = FALSE)
  }
  if (nzchar(a$submitted[1]) && nzchar(b$submitted[1]) &&
    !identical(tolower(trimws(a$submitted)), tolower(trimws(b$submitted)))) {
    off <- which(tolower(trimws(a$submitted)) != tolower(trimws(b$submitted)))[1]
    stop("a and b resolve different names: row ", off, " is '", a$submitted[off],
      "' in a and '", b$submitted[off], "' in b", call. = FALSE)
  }
  submitted <- if (nzchar(a$submitted[1])) a$submitted else b$submitted

  n <- nrow(a)
  same_name <- a$canon == b$canon & nzchar(a$canon)
  same_accepted <- a$accepted_canon == b$accepted_canon & nzchar(a$accepted_canon)
  same_genus <- a$genus == b$genus & nzchar(a$genus)
  both_placed <- a$placed & b$placed
  both_species <- a$species_level & b$species_level

  # A spelling variant: same genus, both at species level, epithets within
  # two edits, and the author on side a vouches for a's match
  close_epithet <- rep(FALSE, n)
  idx <- which(same_genus & both_species & !same_name)
  if (length(idx) > 0) {
    close_epithet[idx] <- mapply(
      function(x, y) utils::adist(x, y)[1, 1] <= 2, a$canon[idx], b$canon[idx]
    )
  }
  author_vouches <- !is.na(a$author_score) & a$author_score >= 0.5
  author_contradicts <- a$author_flag

  vague <- grepl(
    paste0(
      "\\bsp\\.?$|\\bspp\\.?$|\\bspec\\.?$|\\bsp\\.\\s|/| x |\\bx$|×|",
      "\\bcf\\.|\\bnr\\.|\\baff\\.|\\bcomplex\\b|\\bgroup\\b|\\?|",
      "^BOLD:|\\bindet|\\bundet|\\bunident|\\bunknown|\\bunclassified"
    ),
    submitted, ignore.case = TRUE
  )

  tier <- rep(0L, n)
  detail <- rep("", n)
  place <- function(rows, t, why) {
    rows <- rows & tier == 0L
    tier[rows] <<- t
    detail[rows] <<- why
  }

  place(same_name & both_species & same_accepted, 1L, "same species, same accepted name")
  place(same_name & both_species, 2L, "same species, accepted names differ")
  place(same_name, 3L, "agree above species")
  place(same_accepted & both_placed, 4L, "different synonyms, same accepted name")
  place(close_epithet & author_vouches, 4L, "spelling variant, author confirms")
  place(!a$placed & !b$placed, 5L, "neither side could read it")
  place(vague, 5L, "vague or coded string")
  place(a$placed & !b$placed, 6L, "only a placed it")
  place(!a$placed & b$placed, 6L, "only b placed it")
  place(same_genus & a$depth > b$depth, 6L, "a went deeper in the same genus")
  place(same_genus & b$depth > a$depth, 6L, "b went deeper in the same genus")
  place(author_contradicts, 7L, "fuzzy match, author contradicts it")
  place(same_genus, 7L, "same genus, different species")
  place(rep(TRUE, n), 7L, "different genus")

  labels <- c(
    "same species, same accepted name", "same species, different accepted name",
    "agree above species", "different strings, same taxon",
    "unresolvable string", "one side deeper", "contradiction"
  )

  # What to take: the agreed reading, the deeper one, or the genus for a
  # string that goes no further
  from <- rep("", n)
  from[tier %in% c(1L, 2L, 3L)] <- "both"
  from[tier == 4L] <- "a"
  from[detail %in% c("only a placed it", "a went deeper in the same genus")] <- "a"
  from[detail %in% c("only b placed it", "b went deeper in the same genus")] <- "b"
  from[tier == 5L & a$placed] <- "a"
  from[tier == 5L & !a$placed & b$placed] <- "b"
  suggested_name <- ifelse(from %in% c("both", "a"), a$name, ifelse(from == "b", b$name, ""))
  suggested_rank <- ifelse(from %in% c("both", "a"), a$rank, ifelse(from == "b", b$rank, ""))
  # A vague string is taken no further than its genus
  genus_only <- tier == 5L & nzchar(suggested_name)
  suggested_name[genus_only] <- sub(" .*$", "", suggested_name[genus_only])
  suggested_rank[genus_only] <- "genus"

  data.frame(
    Name_submitted = submitted,
    Tier = tier,
    Tier_label = labels[tier],
    Detail = detail,
    Name_a = a$name, Rank_a = a$rank, Accepted_a = a$accepted,
    Name_b = b$name, Rank_b = b$rank, Accepted_b = b$accepted,
    Suggested_name = suggested_name,
    Suggested_rank = suggested_rank,
    Suggested_from = from,
    stringsAsFactors = FALSE
  )
}

#' Reduce one side of a triage to the fields the rules use
#' @keywords internal
#' @noRd
tnrs_triage_side <- function(x, which) {
  if (!is.data.frame(x)) {
    stop(which, " must be a data.frame of results", call. = FALSE)
  }
  needed <- c("Name_matched", "Name_matched_rank", "Accepted_name")
  missing <- setdiff(needed, names(x))
  if (length(missing) > 0) {
    stop(which, " lacks the column(s) ", paste(missing, collapse = ", "), call. = FALSE)
  }
  col <- function(name) {
    v <- if (name %in% names(x)) as.character(x[[name]]) else rep("", nrow(x))
    v[is.na(v)] <- ""
    v
  }
  name <- col("Name_matched")
  name[name == "[No match found]"] <- ""
  rank <- tolower(col("Name_matched_rank"))
  accepted <- col("Accepted_name")

  canon <- tnrs_triage_canon(name)
  # A bare subgenus is written "Culex (Melanoconion)" by one source and
  # "Melanoconion" by another that ranks it as a genus.  Compared on the
  # subgenus name, so the two readings of the same string can agree.
  subgenus <- rank == "subgenus" & grepl("\\(", name)
  canon[subgenus] <- tolower(trimws(sub("^.*\\(([^)]*)\\).*$", "\\1", name[subgenus])))
  depth <- tnrs_triage_depth(rank, canon)

  author_score <- if ("Author_score" %in% names(x)) suppressWarnings(as.numeric(x$Author_score)) else rep(NA_real_, nrow(x))
  warnings <- if ("Warnings" %in% names(x)) suppressWarnings(as.integer(x$Warnings)) else rep(NA_integer_, nrow(x))
  author_flag <- !is.na(warnings) & bitwAnd(warnings, tnrs_warning_flags()[["Author"]]) > 0L

  data.frame(
    submitted = col("Name_submitted"),
    name = name, rank = rank, accepted = accepted,
    canon = canon, accepted_canon = tnrs_triage_canon(accepted),
    genus = sub(" .*$", "", canon),
    depth = depth,
    placed = nzchar(name) & depth >= 1L,
    species_level = depth >= 2L,
    author_score = author_score,
    author_flag = author_flag,
    stringsAsFactors = FALSE
  )
}

#' A name reduced to the form two sides can be compared on
#'
#' Internal.  Authorship is not part of \code{Name_matched}, so what is
#' removed is the subgenus in parentheses, which one source writes and
#' another does not, rank connectors, the hybrid sign, case and spacing.
#' @keywords internal
#' @noRd
tnrs_triage_canon <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  x <- gsub("\\b(subsp|ssp|var|subvar|f|fo|forma|subf|nothosubsp|nothovar)\\.?\\s+", "", x)
  x <- gsub("×", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  tolower(x)
}

#' How far down a reading reached: 0 above genus, 1 genus, 2 species, 3 below
#'
#' Internal.  From the rank where it is given, otherwise from the shape of
#' the name, so a reading from a source that reports no rank still sorts.
#' @keywords internal
#' @noRd
tnrs_triage_depth <- function(rank, canon) {
  depth <- rep(NA_integer_, length(rank))
  depth[rank %in% c("genus", "subgenus", "section", "series")] <- 1L
  depth[rank %in% c("species")] <- 2L
  depth[rank %in% c("subspecies", "variety", "subvariety", "form", "subform", "infraspecies")] <- 3L
  depth[rank %in% c(
    "family", "subfamily", "tribe", "subtribe", "superfamily", "order",
    "suborder", "class", "subclass", "phylum", "kingdom", "unranked"
  )] <- 0L
  words <- lengths(strsplit(canon, " ", fixed = TRUE))
  guess <- ifelse(words == 0L, 0L, ifelse(words == 1L, 1L, ifelse(words == 2L, 2L, 3L)))
  # A one-word name of unknown rank may be a family as easily as a genus;
  # the -idae / -aceae ending decides
  guess[words == 1L & grepl("(idae|aceae|ales|formes)$", canon)] <- 0L
  depth[is.na(depth)] <- guess[is.na(depth)]
  depth[!nzchar(canon)] <- 0L
  depth
}
