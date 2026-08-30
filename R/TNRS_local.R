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
#' @param batch_size Number of names to match at a time. Batching releases each
#'   batch's working memory as it goes, and is what allows progress to be
#'   reported on a long job. The saving is modest, because the loaded reference
#'   data dominates memory and the result is assembled in full whatever the
#'   batch size: measured over 100,000 names, batching saved about 190 MB of a
#'   2 GB peak. The default is a reasonable balance; much smaller batches report
#'   progress more often but run slower.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param quiet Suppress progress messages? A progress bar is shown for jobs
#'   large enough to need more than one batch.
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
                       batch_size = 10000,
                       dir = tnrs_cache_dir(),
                       quiet = FALSE) {
  matches <- match.arg(matches)

  # Checked before coercing, so that a non-numeric argument reports the problem
  # rather than emitting a coercion warning on the way to the error
  if (!is.numeric(batch_size) || length(batch_size) != 1L ||
    is.na(batch_size) || batch_size < 1) {
    stop("batch_size should be a single positive number of names", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)

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

  # The reassembled key above stops at the first infraspecific epithet and uses
  # standardized rank indicators.  The submitted name itself, with the authority
  # removed, is tried as well: it carries any second infraspecific epithet, and
  # it uses whatever rank spelling the source does - WFO writes "f." where we
  # standardize to "fo.", so forma names would otherwise never hit this path.
  wanted_full <- tnrs_toupper_ascii(tnrs_reduce_spaces(
    mapply(
      function(text, author) {
        if (nzchar(author)) tnrs_remove_first(text, author) else text
      },
      pre$cleaned, parsed$authorship, USE.NAMES = FALSE
    )
  ))
  # Only worth trying where it differs from the reassembled key
  wanted_full[wanted_full == wanted] <- ""

  # Names are matched in batches, which releases each batch's working set as it
  # goes and lets a long job report progress.  Measured over 100,000 names this
  # saves about 190 MB of a 2 GB peak - real but modest, since the loaded
  # backbone dominates and the result is assembled in full regardless.  At
  # 20,000 names the effect is not visible at all.
  #
  # Everything before this point is done once for the whole set.  Parsing in
  # particular must not be repeated per batch: GNparser costs a couple of
  # seconds to start up however few names it is handed, so parsing in batches of
  # a thousand would add that cost to every one of them.
  batches <- split(
    seq_along(submitted),
    ceiling(seq_along(submitted) / batch_size)
  )

  show_progress <- !quiet && length(batches) > 1
  if (show_progress) {
    progress <- utils::txtProgressBar(
      min = 0, max = length(batches), style = 3, char = "="
    )
    on.exit(close(progress), add = TRUE)
  }

  outputs <- vector("list", length(batches))

  for (b in seq_along(batches)) {
    in_batch <- batches[[b]]

    exact_hits <- lapply(sources, function(s) {
      tnrs_lookup_each(backbone[[s]]$index$rows_by_name, wanted[in_batch])
    })
    names(exact_hits) <- sources

    exact_full_hits <- lapply(sources, function(s) {
      tnrs_lookup_each(backbone[[s]]$index$rows_by_name, wanted_full[in_batch])
    })
    names(exact_full_hits) <- sources

    # Selection happens per name, but the output is assembled once per batch.
    # Building a wide data.frame per name and rbind-ing them was about a quarter
    # of the total run time.
    selected <- vector("list", length(in_batch))

    for (j in seq_along(in_batch)) {
      i <- in_batch[j]
      query <- parsed[i, , drop = FALSE]

      per_source <- lapply(seq_along(sources), function(s) {
        candidates <- tnrs_match_one(
          query, backbone[[sources[s]]],
          exact = exact_hits[[sources[s]]][[j]],
          exact_full = exact_full_hits[[sources[s]]][[j]]
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

      selected[[j]] <- tnrs_select_candidates(
        query = query, candidates = per_source, backbone = backbone,
        matches = matches, accuracy = accuracy, query_index = i
      )
    }

    outputs[[b]] <- tnrs_build_output(selected, ids, submitted, parsed, backbone)

    if (show_progress) utils::setTxtProgressBar(progress, b)
  }

  out <- if (length(outputs) == 1L) outputs[[1]] else do.call(rbind, outputs)
  rownames(out) <- NULL
  out
}

#' Choose the rows to report for one submitted name
#'
#' Internal.  Returns plain vectors rather than a data.frame: the caller
#' concatenates them across all names and builds the output once.
#'
#' @param query One row of the parsed query.
#' @param candidates List of scored candidate frames, one per source.
#' @param backbone The loaded backbone.
#' @param matches "best" or "all".
#' @param accuracy Optional score threshold.
#' @param query_index Position of this name in the submitted batch.
#' @return A list of equal-length vectors, one entry per output row.
#' @importFrom stats ave
#' @keywords internal
#' @noRd
tnrs_select_candidates <- function(query, candidates, backbone, matches,
                                   accuracy, query_index) {
  none <- list(
    query_index = query_index, row = NA_integer_, source = "",
    matched_rank = "", overall_score = NA_real_, name_score = NA_real_,
    author_score = NA_real_, genus_score = NA_real_, species_score = NA_real_,
    infra1_score = NA_real_, family_score = NA_real_, phonetic = FALSE,
    ambiguous = FALSE, conflict = FALSE,
    overall_order = NA_integer_, highertaxa_order = NA_integer_
  )

  if (length(candidates) == 0) {
    return(none)
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

  # rbind on a one-element list still copies and rebuilds row names, which is
  # measurable when it runs once per submitted name
  pooled <- if (length(candidates) == 1L) candidates[[1]] else do.call(rbind, candidates)

  # Upstream ranks the candidates twice, under two schemes, and warns where the
  # two disagree about which match is best
  overall_order <- tnrs_rank_order(pooled, "overall")
  pooled$Overall_score_order <- order(overall_order)
  pooled$Highertaxa_score_order <- order(tnrs_rank_order(pooled, "highertaxa"))
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
      return(none)
    }
  }

  if (matches == "best") {
    ambiguous <- ambiguous[1]
    pooled <- pooled[1, , drop = FALSE]
  }

  list(
    query_index = rep(query_index, nrow(pooled)),
    row = pooled$row,
    source = pooled$source,
    matched_rank = pooled$matched_rank,
    overall_score = pooled$overall_score,
    name_score = pooled$name_score,
    author_score = pooled$author_score,
    genus_score = pooled$genus_score,
    species_score = pooled$species_score,
    infra1_score = pooled$infra1_score,
    family_score = pooled$family_score,
    phonetic = pooled$phonetic,
    ambiguous = ambiguous,
    conflict = rep(conflict, nrow(pooled)),
    overall_order = pooled$Overall_score_order,
    highertaxa_order = pooled$Highertaxa_score_order
  )
}

#' Build the output table from the selected candidates
#'
#' Internal.  Every column is built once, across all rows, rather than a
#' data.frame being constructed per submitted name.
#'
#' @param selected List of \code{tnrs_select_candidates()} results.
#' @param ids,submitted The submitted identifiers and names.
#' @param parsed The parsed queries.
#' @param backbone The loaded backbone.
#' @return The output data.frame.
#' @keywords internal
#' @noRd
tnrs_build_output <- function(selected, ids, submitted, parsed, backbone) {
  pull <- function(field) unlist(lapply(selected, function(x) x[[field]]), use.names = FALSE)

  query_index <- pull("query_index")
  row <- pull("row")
  source <- pull("source")
  n <- length(row)

  used_sources <- unique(source[nzchar(source)])

  # Look each column up once per source rather than once per row
  from_names <- function(column) {
    out <- rep("", n)
    for (s in used_sources) {
      take <- which(source == s & !is.na(row))
      if (length(take) > 0) {
        out[take] <- as.character(backbone[[s]]$names[[column]][row[take]])
      }
    }
    out
  }

  accepted <- rep(NA_integer_, n)
  for (s in used_sources) {
    take <- which(source == s & !is.na(row))
    accepted[take] <- backbone[[s]]$names$accepted_name_id[row[take]]
  }

  from_accepted <- function(column) {
    out <- rep("", n)
    for (s in used_sources) {
      take <- which(source == s & !is.na(accepted))
      if (length(take) > 0) {
        out[take] <- as.character(backbone[[s]]$names[[column]][accepted[take]])
      }
    }
    out
  }

  matched_rank <- pull("matched_rank")
  matched <- !is.na(row)

  # A match that did not reach the rank the name was parsed to is partial, and
  # upstream blanks the author fields when it is
  depth_of_query <- ifelse(
    nzchar(parsed$infra2), 4L,
    ifelse(nzchar(parsed$infra1), 3L, ifelse(nzchar(parsed$species), 2L, 1L))
  )[query_index]
  depth_matched <- match(
    matched_rank, c("genus", "species", "infra1", "infra2"),
    nomatch = 0L
  )
  partial <- matched & depth_matched < depth_of_query

  flags <- tnrs_warning_flags()
  overall_order <- pull("overall_order")
  highertaxa_order <- pull("highertaxa_order")

  warnings <- flags[["Partial"]] * partial +
    flags[["Ambiguous"]] * (pull("ambiguous") %in% TRUE) +
    flags[["HigherTaxa"]] *
      (!is.na(overall_order) & highertaxa_order > overall_order) +
    flags[["Overall"]] *
      (!is.na(overall_order) & highertaxa_order < overall_order)
  warnings <- as.integer(warnings)

  family_matched_name <- from_names("family")
  submitted_family <- parsed$family[query_index]
  # "The submitted family was matched", not "the matched name has a family": a
  # name matched into a different family leaves the submitted one unmatched
  family_matched <- nzchar(submitted_family) &
    tolower(submitted_family) == tolower(family_matched_name)

  unmatched <- vapply(seq_len(n), function(k) {
    tnrs_unmatched_terms(
      parsed$preprocessed[query_index[k]], parsed[query_index[k], ],
      if (matched[k]) matched_rank[k] else "",
      family_matched[k], parsed$start_string[query_index[k]]
    )
  }, character(1))

  author_score <- pull("author_score")
  author_score[partial] <- NA_real_

  accepted_genus <- from_accepted("genus")
  accepted_epithet <- from_accepted("specific_epithet")

  data.frame(
    ID = ids[query_index],
    Name_submitted = submitted[query_index],
    Overall_score = pull("overall_score"),
    Name_matched_id = from_names("source_name_id"),
    Name_matched = ifelse(matched, from_names("scientific_name"), "[No match found]"),
    Name_score = pull("name_score"),
    Name_matched_rank = from_names("name_rank"),
    Author_submitted = parsed$authorship[query_index],
    Author_matched = ifelse(partial, "", from_names("authorship")),
    Author_score = author_score,
    Canonical_author = from_names("authorship"),
    Name_matched_accepted_family = from_accepted("family"),
    Genus_submitted = parsed$genus[query_index],
    Genus_matched = from_names("genus"),
    Genus_score = pull("genus_score"),
    Specific_epithet_submitted = parsed$species[query_index],
    Specific_epithet_matched = from_names("specific_epithet"),
    Specific_epithet_score = pull("species_score"),
    Family_submitted = parsed$family[query_index],
    Family_matched = family_matched_name,
    Family_score = pull("family_score"),
    Infraspecific_rank = parsed$rank1[query_index],
    Infraspecific_epithet_matched = from_names("infraspecific_epithet"),
    Infraspecific_epithet_score = pull("infra1_score"),
    Infraspecific_rank_2 = parsed$rank2[query_index],
    Infraspecific_epithet_2_matched = rep("", n),
    Infraspecific_epithet_2_score = rep(NA_real_, n),
    Annotations = parsed$annotations[query_index],
    Unmatched_terms = unmatched,
    Name_matched_url = from_names("url"),
    Name_matched_lsid = rep("", n),
    Phonetic = ifelse(pull("phonetic") %in% TRUE, "Y", ""),
    Taxonomic_status = from_names("taxonomic_status"),
    Accepted_name = from_accepted("scientific_name"),
    Accepted_species = ifelse(
      nzchar(accepted_epithet), paste(accepted_genus, accepted_epithet), ""
    ),
    Accepted_name_author = from_accepted("authorship"),
    Accepted_name_id = from_accepted("source_name_id"),
    Accepted_name_rank = from_accepted("name_rank"),
    Accepted_name_url = from_accepted("url"),
    Accepted_name_lsid = rep("", n),
    Accepted_family = from_accepted("family"),
    Overall_score_order = overall_order,
    Highertaxa_score_order = highertaxa_order,
    Source = source,
    Warnings = warnings,
    WarningsEng = tnrs_warnings_english(warnings),
    Source_conflict = pull("conflict") %in% TRUE,
    stringsAsFactors = FALSE
  )
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
