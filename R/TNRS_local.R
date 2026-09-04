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
#'   below and the \code{Source_conflict} column. "wcvp" and "wfo" cover
#'   plants; "mdd" is the Mammal Diversity Database, "phylacine" the
#'   PHYLACINE 1.2.1 mammal list with the names its species carry in IUCN
#'   2016-3, EltonTraits and earlier PHYLACINE versions (a crosswalk to
#'   PHYLACINE's own names, for joining to its traits and phylogenies, rather
#'   than a synonymy of mammals), and "col" the Catalogue of
#'   Life, which covers all life. A checklist registered with
#'   \code{TNRS_local_add_source()} can be named here like any other source.
#'   \code{TNRS_local_status()} lists what is available and what is built.
#' @param matches Character. Should all matches be returned ("all") or only the
#'   best match ("best", the default)?
#' @param accuracy Numeric between 0 and 1, or NULL. Score threshold below
#'   which a match is discarded and the name reported as unmatched. The
#'   default, 0.53, is the web service's default, and the rule is the web
#'   service's too: a match is dropped only when its overall score and every
#'   component score (family, genus, specific and infraspecific epithets) all
#'   fall below the threshold, so a partial match whose genus is right
#'   survives; see the note below. NULL, or 0, keeps every match.
#' @param batch_size Number of names to match at a time. Batching releases each
#'   batch's working memory as it goes, and is what allows progress to be
#'   reported on a long job. The saving is modest, because the loaded reference
#'   data dominates memory and the result is assembled in full whatever the
#'   batch size: measured over 100,000 names, batching saved about 190 MB of a
#'   2 GB peak. The default is a reasonable balance; much smaller batches report
#'   progress more often but run slower.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param nomenclature Nomenclatural code the submitted names follow, either
#'   "botanical", "zoological" or "mixed". Defaults to NULL, which takes it from
#'   the sources requested, so an animal backbone reads animal names without
#'   your having to say so. Set it to override that. It decides how a family
#'   prefix is recognised: botanical families end in -aceae, zoological ones in
#'   -idae, so "Felidae Panthera leo" needs the zoological setting for the
#'   family to be stripped rather than read as the genus.
#' @param within Character. One or more taxon names at family rank or above
#'   (family, order, class, phylum or kingdom) to confine the search to, for
#'   instance \code{"Culicidae"} or \code{c("Ixodida", "Mesostigmata")}.
#'   Defaults to NULL, the whole source. Names are never matched outside the
#'   scope, so a misspelt or missing tick name cannot come back as the bird or
#'   fungus it most resembles, which is what happens against an all-life
#'   source like "col" otherwise; see the note below. Every source can be
#'   confined to a family; ranks above that need the source to record them,
#'   which "col" and "mdd" do and the plant sources do not. A genus is not
#'   accepted, because a genus name is not unique across the tree of life.
#'   A source built by an earlier version of this package has to be rebuilt
#'   before it can be searched this way.
#' @param build_missing Should a requested source that has not been built yet be
#'   downloaded and built now? Defaults to \code{interactive()}, which reports
#'   the size and asks first. In a script it is therefore FALSE and nothing is
#'   downloaded silently: the function reports what is missing and the call that
#'   would fix it. Set it to TRUE to allow an unattended build.
#' @param quiet Suppress progress messages? A progress bar is shown for jobs
#'   large enough to need more than one batch.
#' @return Dataframe of results, with the same core columns as \code{TNRS()},
#'   plus \code{Source} naming the source the match came from and
#'   \code{Source_conflict} flagging names the requested sources disagreed about.
#'   \code{Warnings} carries the web service's four flags and one more of its
#'   own, \code{[Author]} (bit 16): the name matched only approximately and
#'   the authority it was submitted with contradicts the authority of the
#'   match. That combination usually means a name the source lacks has been
#'   matched to its nearest neighbour, and is worth a look before the
#'   accepted name is trusted. An author disagreement on an exact name match
#'   is not flagged, since sources often cite a later combination. Before
#'   authors are compared, an abbreviated surname on either side ("Theob.",
#'   "Edw.", "C.A.Mey.") is expanded against the other where it is a prefix
#'   of exactly one surname there, so an abbreviation alone does not count as
#'   a disagreement.
#' @note \strong{This is a new implementation and should be treated as beta.}
#'   It is a port of the published TNRS algorithm. Measured against the web
#'   service over the 100 name benchmark in \code{tnrs_testfile}, it returns the
#'   same matched name for all 100, two of them differing only in whether the
#'   hybrid marker is written out, and the same accepted name for 88. Most of
#'   the 12 accepted-name differences are explained rather than wrong: in
#'   several the accepted name the service gives is now a synonym in the
#'   current sources, or the service gave none at all, so the local answer
#'   reflects a treatment that changed after the service's backbone was built
#'   in January 2024; in others WFO and WCVP disagree and the two follow
#'   different sources, which \code{Source_conflict} marks. Beyond that
#'   benchmark it has not been tested broadly across taxonomic groups,
#'   sources, or the range of messy input real datasets contain, and it has
#'   not been independently reviewed. For work where the answer matters, check
#'   a sample against \code{TNRS()} and please report what disagrees.
#' @note The default differs from \code{TNRS()}. The web service consults
#'   \code{c("wcvp", "wfo")} together, which means a single answer can be
#'   assembled from two authorities that disagree. This function consults "wfo"
#'   alone by default, so the result comes from one authority and is
#'   straightforward to cite. Pass \code{sources = c("wcvp", "wfo")} to
#'   reproduce the web service's behaviour.
#' @note \code{TNRS_local_build()} builds "wfo" alone by default, so asking for
#'   "wcvp" as well needs it built first. Rather than download it behind your
#'   back, this function reports what is missing and the call that would build
#'   it; see \code{build_missing}.
#' @note When more than one source is requested, \code{Source_conflict} is TRUE
#'   for any name where the sources led to different accepted names. Those are
#'   the names worth looking at by hand; \code{matches = "all"} shows what each
#'   source said.
#' @note Results are not guaranteed to be identical to \code{TNRS()}. The local
#'   backbone is downloaded directly from the publishers and is often newer than
#'   the copy the web service is running, so a name may legitimately resolve
#'   differently. Use \code{TNRS_local_status()} to report the versions you used.
#' @note A submitted family prefix does not confine the search. As in the web
#'   service, the family is matched separately and only competes with what
#'   was found below it, so \code{"Ixodidae Ixodes inopinatus"} against "col"
#'   still reaches a bird. \code{within} is what narrows the search; a
#'   family prefix only scores it. A prefix above family rank is different:
#'   a name opening with an order, class, phylum or kingdom the source knows,
#'   "Carnivora Vulpes vulpes" or "Plantae Oenanthe", has it taken off and
#'   the search for that one name confined to it, exactly as \code{within}
#'   would. The web service reads such a prefix as the genus.
#' @note \code{accuracy} means the same here as in \code{TNRS()}, and the
#'   default is the same 0.53. The rule is more permissive than it looks:
#'   the web service discards a match only when \emph{every} score is below
#'   the threshold, so a genus-only match, whose genus score is 1, is always
#'   kept, and a fuzzy match to a wrong genus survives if the genus score
#'   alone clears the bar. What the threshold removes is the tail of matches
#'   that are poor in every part. To cut a fuzzy match to the wrong group,
#'   use \code{within}; to keep every match, pass \code{accuracy = NULL}.
#' @seealso \code{\link{TNRS_local_build}}, \code{\link{TNRS_local_status}},
#'   \code{\link{TNRS_local_add_source}} to resolve against a checklist of your
#'   own.
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
#'
#' # Tick names against the Catalogue of Life, without straying outside ticks
#' TNRS_local(c("Ixodes inopinatus", "Boophilus"),
#'            sources = "col", within = "Ixodida")
#' }
TNRS_local <- function(taxonomic_names,
                       sources = "wfo",
                       matches = c("best", "all"),
                       accuracy = 0.53,
                       batch_size = 10000,
                       dir = tnrs_cache_dir(),
                       nomenclature = NULL,
                       within = NULL,
                       build_missing = interactive(),
                       quiet = FALSE) {
  matches <- match.arg(matches)

  if (!is.null(within)) {
    if (!is.character(within) || length(within) == 0 ||
      any(is.na(within)) || any(!nzchar(trimws(within)))) {
      stop("within should be one or more taxon names, or NULL", call. = FALSE)
    }
    within <- trimws(within)
  }

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
  if (!is.null(accuracy)) {
    if (length(accuracy) != 1L || is.na(accuracy) || accuracy < 0 || accuracy > 1) {
      stop("accuracy should be either numeric between 0 and 1, or NULL")
    }
    # The web service treats 0 as "return everything", and so does this
    if (accuracy == 0) {
      accuracy <- NULL
    }
  }

  registry <- tnrs_source_registry(dir)
  if (!all(sources %in% names(registry))) {
    message(
      "Invalid source(s) specified. Current options are: ",
      paste(names(registry), collapse = ", ")
    )
    return(invisible(NULL))
  }

  if (!tnrs_require_sources(sources,
    dir = dir,
    build_missing = build_missing, quiet = quiet
  )) {
    return(invisible(NULL))
  }

  # Checked the same way as TNRS(), so the two report the same problems in the
  # same words
  checked <- tnrs_check_names(taxonomic_names)
  ids <- checked$ID
  submitted <- checked$name

  backbone <- tnrs_backbone(sources, dir = dir, quiet = quiet)

  # Which part of each source the search is confined to.  A taxon that no
  # source knows at any rank above genus is a mistake worth stopping on; one
  # that some sources know and others do not just leaves the others empty.
  scopes <- NULL
  if (!is.null(within)) {
    scopes <- lapply(sources, function(s) tnrs_scope_mask(backbone[[s]], within))
    names(scopes) <- sources
    found <- unique(unlist(lapply(scopes, function(x) x$found)))
    unknown <- within[!tnrs_toupper_ascii(within) %in% tnrs_toupper_ascii(found)]
    if (length(unknown) > 0) {
      message(
        "within: ", paste0('"', unknown, '"', collapse = ", "),
        if (length(unknown) == 1) " is" else " are",
        " not a taxon at family rank or above in ",
        if (length(sources) == 1) "this source" else "any of these sources",
        ". A genus cannot be used as a scope; give its family instead."
      )
      return(invisible(NULL))
    }
    empty <- sources[vapply(scopes, function(x) !any(x$row), logical(1))]
    if (length(empty) > 0 && !quiet) {
      message(
        "within: source(s) ", paste(empty, collapse = ", "),
        " hold nothing in that scope and will not match anything."
      )
    }
  }

  # Which nomenclatural code the names follow decides how a family prefix is
  # recognised, so it is settled before anything is read
  codes <- tnrs_effective_codes(sources, nomenclature, dir = dir)

  # Preprocess and parse the whole batch at once; the phonetic keys in
  # particular are far cheaper computed in bulk than one name at a time
  # A leading order, class, phylum or kingdom that a source knows is taken
  # off the name and confines its search, as within does; a leading family
  # the sources know goes down the family path whatever its ending
  known_higher <- unique(unlist(lapply(backbone, function(b) b$higher)))
  known_families <- unique(unlist(lapply(backbone, function(b) {
    tnrs_toupper_ascii(b$index$family$name)
  })))
  pre <- tnrs_preprocess(submitted, codes = codes, higher = known_higher, families = known_families)
  parsed <- tnrs_parse(pre$cleaned, codes = codes)
  parsed$family <- pre$family
  parsed$higher <- pre$higher
  parsed$annotations <- pre$annotations

  # One mask per source per distinct prefix, built on first use and combined
  # with any within the caller gave
  prefix_scopes <- new.env(parent = emptyenv())
  scope_for <- function(s, higher) {
    if (!nzchar(higher)) {
      return(scopes[[s]])
    }
    key <- paste(s, higher, sep = "|")
    if (is.null(prefix_scopes[[key]])) {
      mask <- tnrs_scope_mask(backbone[[s]], higher)
      if (!is.null(scopes)) {
        mask <- tnrs_scope_intersect(mask, scopes[[s]])
      }
      prefix_scopes[[key]] <- mask
    }
    prefix_scopes[[key]]
  }
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
          exact_full = exact_full_hits[[sources[s]]][[j]],
          scope = scope_for(sources[s], query$higher)
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
    # The web service's rule (tnrs_api.php): a match is reset to "no match"
    # only when the overall score and every component score are all below the
    # threshold.  A component that was not scored counts as below, as it does
    # there, where an empty string compares as zero.  So a genus-only match
    # always survives, and what goes is the match poor in every part.
    clears <- function(score) !is.na(score) & score >= accuracy
    keep <- clears(pooled$overall_score) | clears(pooled$family_score) |
      clears(pooled$genus_score) | clears(pooled$species_score) |
      clears(pooled$infra1_score)
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

  # Author: the name matched only approximately and the authority the name
  # came with contradicts the authority of what it matched.  Not an upstream
  # flag.  Measured on GBIF's mosquito and tick names against the Catalogue
  # of Life, this combination is almost exclusively a name the source lacks
  # matched to its nearest neighbour, "Ixodes barkeri Barker 2019" landing on
  # "Ixodes bakeri Arthur & Clifford, 1961" at 0.8.  The author alone is not
  # evidence: on an exact name match a disagreeing author is usually the
  # source citing a later combination, and the name is right.
  name_score <- pull("name_score")
  author_score_raw <- pull("author_score")
  author_disagrees <- matched & !partial &
    !is.na(name_score) & name_score < 1 &
    nzchar(parsed$authorship[query_index]) & nzchar(from_names("authorship")) &
    !is.na(author_score_raw) & author_score_raw < 0.5

  warnings <- flags[["Partial"]] * partial +
    flags[["Ambiguous"]] * (pull("ambiguous") %in% TRUE) +
    flags[["HigherTaxa"]] *
      (!is.na(overall_order) & highertaxa_order > overall_order) +
    flags[["Overall"]] *
      (!is.na(overall_order) & highertaxa_order < overall_order) +
    flags[["Author"]] * author_disagrees
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
#' Internal.  The first four are \code{TnrsAggregator::$flag_def}; Author is
#' local to this implementation and uses the next free bit, so the four the
#' web service sets keep their values.
#' @keywords internal
#' @noRd
tnrs_warning_flags <- function() {
  c(Partial = 1L, Ambiguous = 2L, HigherTaxa = 4L, Overall = 8L, Author = 16L)
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
      # A higher-taxon prefix confined the search, so it was used
      if (!is.null(query$higher)) query$higher,
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
