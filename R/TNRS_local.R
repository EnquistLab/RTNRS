#' Resolve plant taxonomic names without an internet connection
#'
#' Resolves names against a locally cached copy of the taxonomic sources, using
#' the same algorithm as the web service.  Run \code{TNRS_local_build()} once to
#' download and prepare the data; afterwards this function needs no internet
#' connection and no round trips to the server.
#'
#' @param taxonomic_names Data.frame containing two columns: 1) Row number,
#'   2) Taxonomic names to be resolved. Alternatively, a character vector of
#'   names can be supplied.
#' @param sources Character. Taxonomic sources to use, in order of preference.
#'   Defaults to "wfo" alone. Supplying more than one blends them, which is what
#'   the web service does, but the sources do not always agree; see the note
#'   below and the \code{Source_conflict} column.
#' @param matches Character. Should all matches be returned ("all") or only the
#'   best match ("best", the default)?
#' @param accuracy Numeric. If specified, matches scoring below this are
#'   discarded. Note that, unlike the web service, this is applied to the
#'   overall score alone; see the note below.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param quiet Suppress progress messages?
#' @return Dataframe of results, with the same core columns as \code{TNRS()},
#'   plus \code{Source} naming the source the match came from and
#'   \code{Source_conflict} flagging names the requested sources disagreed about.
#' @note The default differs from \code{TNRS()}. The web service consults
#'   \code{c("wcvp", "wfo")} together, which means a single answer can be
#'   assembled from two authorities that disagree. This function consults "wfo"
#'   alone by default, so the result comes from one authority and is
#'   straightforward to cite. Pass \code{sources = c("wcvp", "wfo")} to
#'   reproduce the web service's behaviour.
#' @note When more than one source is requested, \code{Source_conflict} is TRUE
#'   for any name where the sources led to different accepted names. Those are
#'   the names worth looking at by hand; \code{matches = "all"} shows what each
#'   source said.
#' @note Results are not guaranteed to be identical to \code{TNRS()}. The local
#'   backbone is downloaded directly from the publishers and is often newer than
#'   the copy the web service is running, so a name may legitimately resolve
#'   differently. Use \code{TNRS_local_status()} to report the versions you used.
#' @note The \code{accuracy} argument behaves differently here than in
#'   \code{TNRS()}. The web service discards a match only when \emph{every}
#'   component score falls below the threshold, so a low-scoring match can
#'   survive a high threshold. This function applies the threshold to the
#'   overall score, which is what the documentation of the web service describes.
#' @seealso \code{\link{TNRS_local_build}}, \code{\link{TNRS_local_status}}
#' @export
#' @examples \dontrun{
#' # One-off setup
#' TNRS_local_build()
#'
#' results <- TNRS_local(c("Quercuss alba", "Xantium strumarium"))
#'
#' # Consult both sources and see where they disagree
#' both <- TNRS_local(c("Quercuss alba", "Xantium strumarium"),
#'                    sources = c("wcvp", "wfo"))
#' both[both$Source_conflict, ]
#' }
TNRS_local <- function(taxonomic_names,
                       sources = "wfo",
                       matches = c("best", "all"),
                       accuracy = NULL,
                       dir = tnrs_cache_dir(),
                       quiet = FALSE) {
  matches <- match.arg(matches)

  if (!inherits(x = accuracy, what = c("NULL", "numeric"))) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }

  registry <- tnrs_source_registry()
  if (!all(sources %in% names(registry))) {
    message(
      "Invalid source(s) specified. Current options are: ",
      paste(names(registry), collapse = ", ")
    )
    return(invisible(NULL))
  }

  missing <- sources[!vapply(
    sources, function(s) file.exists(tnrs_names_path(s, dir)), logical(1)
  )]
  if (length(missing) > 0) {
    message(
      "No local copy of: ", paste(missing, collapse = ", "),
      ".\nRun TNRS_local_build() once to download and prepare the data."
    )
    return(invisible(NULL))
  }

  if (inherits(x = taxonomic_names, what = "character")) {
    taxonomic_names <- data.frame(
      ID = seq_along(taxonomic_names), taxon = taxonomic_names,
      stringsAsFactors = FALSE
    )
  }
  ids <- as.character(taxonomic_names[[1]])
  submitted <- as.character(taxonomic_names[[2]])

  backbone <- tnrs_backbone(sources, dir = dir, quiet = quiet)

  # Preprocess and parse the whole batch at once; the phonetic keys in
  # particular are far cheaper computed in bulk than one name at a time
  pre <- tnrs_preprocess(submitted)
  parsed <- tnrs_parse(pre$cleaned)
  parsed$family <- pre$family
  parsed$annotations <- pre$annotations
  # Kept for the Unmatched_terms calculation, which works by subtracting the
  # matched components from the pre-processed text
  parsed$preprocessed <- pre$preprocessed
  parsed$start_string <- pre$start_string

  # Phonetic keys cost far more computed one name at a time than in bulk, so
  # they are worked out for the whole batch once and handed to the matcher
  parsed$genus_key <- tnrs_near_match(parsed$genus, "genus_only")
  parsed$species_key <- tnrs_near_match(parsed$species, "epithet_only")

  # The exact-match key for each query, at the deepest rank it offered
  wanted <- ifelse(
    nzchar(parsed$species),
    ifelse(
      nzchar(parsed$infra1),
      tnrs_reduce_spaces(paste(parsed$genus, parsed$species, parsed$rank1, parsed$infra1)),
      paste(parsed$genus, parsed$species)
    ),
    parsed$genus
  )
  wanted <- tnrs_toupper_ascii(wanted)

  exact_hits <- lapply(sources, function(s) {
    tnrs_lookup_each(backbone[[s]]$index$rows_by_name, wanted)
  })
  names(exact_hits) <- sources

  results <- vector("list", length(submitted))

  for (i in seq_along(submitted)) {
    query <- parsed[i, , drop = FALSE]

    per_source <- lapply(seq_along(sources), function(s) {
      candidates <- tnrs_match_one(
        query, backbone[[sources[s]]],
        exact = exact_hits[[sources[s]]][[i]]
      )
      if (nrow(candidates) == 0) {
        return(NULL)
      }
      scored <- tnrs_score_candidates(
        candidates, query, backbone[[sources[s]]]$names, source_order = s
      )
      scored$source <- sources[s]
      scored
    })

    per_source <- per_source[!vapply(per_source, is.null, logical(1))]

    results[[i]] <- tnrs_assemble_row(
      id = ids[i], submitted = submitted[i], query = query,
      candidates = per_source, backbone = backbone,
      matches = matches, accuracy = accuracy
    )
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

#' Warning flags, as a bit field
#'
#' Internal.  From \code{TnrsAggregator::$flag_def}.
#' @keywords internal
#' @noRd
tnrs_warning_flags <- function() {
  c(Partial = 1L, Ambiguous = 2L, HigherTaxa = 4L, Overall = 8L)
}

#' Render a warning bit field in words
#' @keywords internal
#' @noRd
tnrs_warnings_english <- function(bits) {
  flags <- tnrs_warning_flags()
  vapply(bits, function(b) {
    if (is.na(b) || b == 0L) {
      return("")
    }
    set <- names(flags)[bitwAnd(b, flags) > 0L]
    paste0("[", paste(set, collapse = "][", recycle0 = FALSE), "]")
  }, character(1), USE.NAMES = FALSE)
}

#' Remove the first occurrence of a literal string, ignoring case
#'
#' Internal.  Upstream uses str_ireplace_first here, and the case-insensitivity
#' matters: a family submitted in capitals must still be recognised as the
#' family that was matched.
#' @keywords internal
#' @noRd
tnrs_remove_first <- function(text, part) {
  # Located by a case-folded fixed search rather than a regular expression: the
  # part is literal text, and authorities are full of brackets, full stops and
  # apostrophes that would otherwise need escaping.
  at <- regexpr(tolower(part), tolower(text), fixed = TRUE)
  if (at < 0) {
    return(text)
  }

  paste0(
    substr(text, 1L, at - 1L),
    substr(text, at + attr(at, "match.length"), nchar(text))
  )
}

#' Terms of the submitted name that no matched component accounts for
#'
#' Internal.  R port of the \code{Unmatched_terms} logic in
#' \code{TnrsAggregator}: start from the pre-processed text and subtract, in
#' turn, the family, the matched name parts, the rank indicators and the
#' authority.  Whatever remains was not accounted for.
#'
#' @param preprocessed The pre-processed submitted text.
#' @param query The parsed query.
#' @param matched_rank Rank the name matched at, or "" if nothing matched.
#' @param family_matched Was the family matched?
#' @param start_string Leading non-alphabetic characters set aside earlier.
#' @return A single string, possibly empty.
#' @keywords internal
#' @noRd
tnrs_unmatched_terms <- function(preprocessed, query, matched_rank,
                                 family_matched = FALSE, start_string = "") {
  # Nothing matched below genus: the whole name is unaccounted for
  if (!nzchar(matched_rank) || identical(matched_rank, "family")) {
    remainder <- preprocessed
  } else {
    remainder <- preprocessed

    # Everything down to the rank actually matched is accounted for; anything
    # below it is not, and stays in the remainder
    depth <- match(matched_rank, c("genus", "species", "infra1"), nomatch = 1L)

    parts <- c(
      if (family_matched) query$family,
      query$genus,
      if (depth >= 2) query$species,
      if (depth >= 3) c(query$rank1, query$infra1),
      query$authorship
    )
    parts <- parts[!is.na(parts) & nzchar(parts)]

    for (part in parts) {
      remainder <- tnrs_remove_first(remainder, part)
    }
  }

  remainder <- tnrs_reduce_spaces(remainder)
  trimws(paste0(start_string, remainder))
}

#' Assemble the output rows for one submitted name
#' @keywords internal
#' @noRd
tnrs_assemble_row <- function(id, submitted, query, candidates, backbone,
                              matches, accuracy) {
  flags <- tnrs_warning_flags()

  blank <- data.frame(
    ID = id,
    Name_submitted = submitted,
    Overall_score = NA_real_,
    Name_matched_id = "",
    Name_matched = "[No match found]",
    Name_score = NA_real_,
    Name_matched_rank = "",
    Author_submitted = query$authorship,
    Author_matched = "",
    Author_score = NA_real_,
    Canonical_author = "",
    Name_matched_accepted_family = "",
    Genus_submitted = query$genus,
    Genus_matched = "",
    Genus_score = NA_real_,
    Specific_epithet_submitted = query$species,
    Specific_epithet_matched = "",
    Specific_epithet_score = NA_real_,
    Family_submitted = query$family,
    Family_matched = "",
    Family_score = NA_real_,
    Infraspecific_rank = query$rank1,
    Infraspecific_epithet_matched = "",
    Infraspecific_epithet_score = NA_real_,
    Infraspecific_rank_2 = query$rank2,
    Infraspecific_epithet_2_matched = "",
    Infraspecific_epithet_2_score = NA_real_,
    Annotations = query$annotations,
    Unmatched_terms = "",
    Name_matched_url = "",
    Name_matched_lsid = "",
    Phonetic = "",
    Taxonomic_status = "",
    Accepted_name = "",
    Accepted_species = "",
    Accepted_name_author = "",
    Accepted_name_id = "",
    Accepted_name_rank = "",
    Accepted_name_url = "",
    Accepted_name_lsid = "",
    Accepted_family = "",
    Overall_score_order = NA_integer_,
    Highertaxa_score_order = NA_integer_,
    Source = "",
    Warnings = 0L,
    WarningsEng = "",
    Source_conflict = FALSE,
    stringsAsFactors = FALSE
  )

  if (length(candidates) == 0) {
    blank$Unmatched_terms <- tnrs_unmatched_terms(
      query$preprocessed, query, "", FALSE, query$start_string
    )
    return(blank)
  }

  # Whether the requested sources led to different accepted names for this
  # submitted name.  Judged on the best candidate from each source, before they
  # are pooled and the winner chosen, since pooling hides the disagreement.
  conflict <- FALSE
  if (length(candidates) > 1) {
    per_source_accepted <- vapply(candidates, function(x) {
      best <- x[1, ]
      source_names <- backbone[[best$source]]$names
      accepted <- source_names$accepted_name_id[best$row]
      if (is.na(accepted)) "" else source_names$scientific_name[accepted]
    }, character(1))

    informative <- per_source_accepted[nzchar(per_source_accepted)]
    conflict <- length(unique(informative)) > 1
  }

  pooled <- do.call(rbind, candidates)

  # Upstream ranks the candidates twice, under two schemes, and warns where the
  # two disagree about which match is best
  overall_order <- tnrs_rank_order(pooled, "overall")
  highertaxa_order <- tnrs_rank_order(pooled, "highertaxa")

  pooled$Overall_score_order <- order(overall_order)
  pooled$Highertaxa_score_order <- order(highertaxa_order)

  pooled <- pooled[overall_order, , drop = FALSE]

  # Ambiguous: two *different* names the ordering could not separate.  Upstream
  # only reaches its ambiguity marker after every comparison including
  # Source_order has tied, so rows that are merely several records of the same
  # name do not count - which is most of them.
  sort_keys <- paste(
    pooled$rank_index, tnrs_total_edit_distance(pooled),
    pooled$name_score, pooled$overall_score, pooled$acceptance,
    pooled$name_matched, pooled$source_order
  )
  tied <- sort_keys %in% sort_keys[duplicated(sort_keys)]
  ambiguous <- tied & ave(
    pooled$row, sort_keys,
    FUN = function(rows) rep(length(unique(rows)) > 1L, length(rows))
  ) == 1L

  if (!is.null(accuracy)) {
    keep <- !is.na(pooled$overall_score) & pooled$overall_score >= accuracy
    ambiguous <- ambiguous[keep]
    pooled <- pooled[keep, , drop = FALSE]
    if (nrow(pooled) == 0) {
      blank$Unmatched_terms <- tnrs_unmatched_terms(
        query$preprocessed, query, "", FALSE, query$start_string
      )
      return(blank)
    }
  }

  if (matches == "best") {
    ambiguous <- ambiguous[1]
    pooled <- pooled[1, , drop = FALSE]
  }

  out <- blank[rep(1, nrow(pooled)), , drop = FALSE]

  for (i in seq_len(nrow(pooled))) {
    candidate <- pooled[i, ]
    names <- backbone[[candidate$source]]$names
    row <- candidate$row

    matched_rank <- candidate$matched_rank
    # "The submitted family was matched", not "the matched name has a family":
    # a name matched into a different family leaves the submitted one unmatched
    family_matched <- nzchar(query$family) &&
      identical(
        tolower(query$family),
        tolower(as.character(names$family[row]))
      )

    # Upstream flags a match as Partial when it did not reach the rank the name
    # was parsed to, and blanks the author fields when it does
    parsed_depth <- if (nzchar(query$infra1)) {
      3L
    } else if (nzchar(query$species)) 2L else 1L
    matched_depth <- match(matched_rank, c("genus", "species", "infra1"),
      nomatch = 0L
    )
    partial <- matched_depth < parsed_depth

    warnings <- 0L
    if (partial) warnings <- bitwOr(warnings, flags[["Partial"]])
    if (isTRUE(ambiguous[i])) warnings <- bitwOr(warnings, flags[["Ambiguous"]])
    if (candidate$Highertaxa_score_order > candidate$Overall_score_order) {
      warnings <- bitwOr(warnings, flags[["HigherTaxa"]])
    } else if (candidate$Highertaxa_score_order < candidate$Overall_score_order) {
      warnings <- bitwOr(warnings, flags[["Overall"]])
    }

    out$Overall_score[i] <- candidate$overall_score
    out$Name_matched_id[i] <- names$source_name_id[row]
    out$Name_matched[i] <- names$scientific_name[row]
    out$Name_score[i] <- candidate$name_score
    out$Name_matched_rank[i] <- names$name_rank[row]
    out$Canonical_author[i] <- names$authorship[row]
    out$Genus_matched[i] <- names$genus[row]
    out$Genus_score[i] <- candidate$genus_score
    out$Specific_epithet_matched[i] <- names$specific_epithet[row]
    out$Specific_epithet_score[i] <- candidate$species_score
    out$Family_matched[i] <- names$family[row]
    out$Family_score[i] <- candidate$family_score
    out$Infraspecific_epithet_matched[i] <- names$infraspecific_epithet[row]
    out$Infraspecific_epithet_score[i] <- candidate$infra1_score
    out$Name_matched_url[i] <- names$url[row]
    out$Phonetic[i] <- if (isTRUE(candidate$phonetic)) "Y" else ""
    out$Taxonomic_status[i] <- names$taxonomic_status[row]
    out$Overall_score_order[i] <- candidate$Overall_score_order
    out$Highertaxa_score_order[i] <- candidate$Highertaxa_score_order
    out$Source[i] <- candidate$source
    out$Source_conflict[i] <- conflict
    out$Warnings[i] <- warnings
    out$WarningsEng[i] <- tnrs_warnings_english(warnings)

    # The author fields are reported only for a match that reached the parsed
    # rank; upstream blanks them otherwise
    if (!partial) {
      out$Author_matched[i] <- names$authorship[row]
      out$Author_score[i] <- candidate$author_score
    }

    out$Unmatched_terms[i] <- tnrs_unmatched_terms(
      query$preprocessed, query, matched_rank, family_matched,
      query$start_string
    )

    accepted <- names$accepted_name_id[row]
    if (!is.na(accepted)) {
      out$Accepted_name[i] <- names$scientific_name[accepted]
      out$Accepted_name_author[i] <- names$authorship[accepted]
      out$Accepted_name_id[i] <- names$source_name_id[accepted]
      out$Accepted_name_rank[i] <- names$name_rank[accepted]
      out$Accepted_name_url[i] <- names$url[accepted]
      out$Accepted_family[i] <- names$family[accepted]
      out$Name_matched_accepted_family[i] <- names$family[accepted]
      if (nzchar(names$specific_epithet[accepted])) {
        out$Accepted_species[i] <- paste(
          names$genus[accepted], names$specific_epithet[accepted]
        )
      }
    }
  }

  out
}
