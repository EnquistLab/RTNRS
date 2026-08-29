#' Build a candidate frame with the standard columns
#'
#' Internal.  Every stage of the cascade returns the same shape, so that they
#' can be pooled without a column going missing.
#' @keywords internal
#' @noRd
tnrs_candidate_frame <- function(row, matched_rank,
                                 genus_ed = NA_integer_,
                                 species_ed = NA_integer_,
                                 infra1_ed = NA_integer_,
                                 family_ed = NA_integer_,
                                 phonetic = FALSE) {
  data.frame(
    row = row, matched_rank = matched_rank,
    genus_ed = genus_ed, species_ed = species_ed, infra1_ed = infra1_ed,
    family_ed = family_ed, phonetic = phonetic,
    stringsAsFactors = FALSE
  )
}

#' Find candidate name rows for one parsed query against one source
#'
#' Internal.  Runs the cascade: an exact hit on the full name short-circuits
#' everything, otherwise the genus is blocked and scored, then the epithet within
#' the surviving genera.  Returns the rows of the source's name table that
#' matched, with their component edit distances.
#'
#' @param parsed One row of \code{tnrs_parse()} output, plus \code{family}.
#' @param backbone One element of \code{tnrs_backbone()}.
#' @param search_mode "normal" or "extended".
#' @param exact Rows already found by the batched exact lookup, or NULL to look
#'   the name up here.  Batching that lookup matters: it hashes a key vector of
#'   over a million names, and doing it once for the whole query set rather than
#'   once per name is the difference between usable and not.
#' @return A data.frame of candidate rows and their scores, possibly empty.
#' @keywords internal
#' @noRd
tnrs_match_one <- function(parsed, backbone, search_mode = "normal",
                           exact = NULL, exact_full = NULL) {
  index <- backbone$index
  names <- backbone$names

  empty <- tnrs_candidate_frame(
    integer(0), character(0),
    genus_ed = integer(0), species_ed = integer(0), infra1_ed = integer(0),
    family_ed = integer(0), phonetic = logical(0)
  )

  genus <- parsed$genus
  epithet <- parsed$species

  if (!nzchar(genus)) {
    return(tnrs_match_family_only(parsed, backbone, search_mode))
  }

  # 1. Exact hit at the deepest rank the query offered.  Most real queries land
  # here, and it costs one hash lookup.  The name has to be assembled down to
  # the infraspecific epithet: looking up the binomial alone would return a
  # species-rank match for a name that named a variety.
  wanted <- genus
  wanted_rank <- "genus"
  if (nzchar(epithet)) {
    wanted <- paste(wanted, epithet)
    wanted_rank <- "species"
    if (nzchar(parsed$infra1)) {
      wanted <- paste(wanted, parsed$rank1, parsed$infra1)
      wanted <- tnrs_reduce_spaces(wanted)
      wanted_rank <- "infra1"
    }
  }

  # A hit on the whole submitted name is the most specific answer available,
  # and is the only route to a second infraspecific epithet
  if (length(exact_full) > 0) {
    full_rank <- if (nzchar(parsed$infra2)) {
      "infra2"
    } else if (nzchar(parsed$infra1)) {
      "infra1"
    } else if (nzchar(epithet)) "species" else "genus"

    return(tnrs_candidate_frame(
      exact_full, full_rank,
      genus_ed = 0L,
      species_ed = if (nzchar(epithet)) 0L else NA_integer_,
      infra1_ed = if (nzchar(parsed$infra1)) 0L else NA_integer_,
      phonetic = TRUE
    ))
  }

  if (is.null(exact)) {
    exact <- tnrs_lookup(index$rows_by_name, tnrs_toupper_ascii(wanted))
  }

  if (length(exact) > 0) {
    return(tnrs_candidate_frame(
      exact, wanted_rank,
      genus_ed = 0L,
      species_ed = if (nzchar(epithet)) 0L else NA_integer_,
      infra1_ed = if (identical(wanted_rank, "infra1")) 0L else NA_integer_,
      phonetic = TRUE
    ))
  }

  # 2. Block the genus, then judge the survivors
  genus_key <- if (!is.null(parsed$genus_key)) parsed$genus_key else NULL

  genus_candidates <- tnrs_genus_candidates(
    genus, epithet, index$genus,
    species_index = index$species, search_mode = search_mode,
    query_key = genus_key
  )
  if (length(genus_candidates) == 0) {
    return(tnrs_match_family_only(parsed, backbone, search_mode))
  }

  genus_match <- tnrs_match_component(
    genus, index$genus$upper[genus_candidates], "genus",
    search_mode = search_mode,
    query_key = genus_key,
    candidate_key = index$genus$key[genus_candidates]
  )
  matched_genera <- genus_candidates[genus_match$match]
  if (length(matched_genera) == 0) {
    return(tnrs_match_family_only(parsed, backbone, search_mode))
  }

  genus_distance <- genus_match$edit_distance[genus_match$match]
  genus_phonetic <- genus_match$phonetic[genus_match$match]

  # 3. No epithet submitted: the genus itself is the answer
  if (!nzchar(epithet)) {
    rows <- lapply(seq_along(matched_genera), function(i) {
      hits <- tnrs_int_lookup(index$rows_by_genus, matched_genera[i])
      if (length(hits) == 0) {
        return(NULL)
      }
      hits <- hits[names$name_rank[hits] == "genus"]
      if (length(hits) == 0) {
        return(NULL)
      }
      tnrs_candidate_frame(
        hits, "genus",
        genus_ed = genus_distance[i], phonetic = genus_phonetic[i]
      )
    })
    rows <- do.call(rbind, rows)
    if (is.null(rows)) {
      return(tnrs_match_family_only(parsed, backbone, search_mode))
    }
    return(rows)
  }

  # 4. Epithet within the matched genera
  species_candidates <- tnrs_child_candidates(epithet, matched_genera, index$species)
  if (length(species_candidates) == 0) {
    return(tnrs_match_genus_only(
      parsed, backbone, matched_genera, genus_distance, genus_phonetic
    ))
  }

  species_match <- tnrs_match_component(
    epithet, index$species$upper[species_candidates], "epithet",
    search_mode = search_mode,
    query_key = if (!is.null(parsed$species_key)) parsed$species_key else NULL,
    candidate_key = index$species$key[species_candidates]
  )
  accepted <- species_candidates[species_match$match]
  if (length(accepted) == 0) {
    return(tnrs_match_genus_only(
      parsed, backbone, matched_genera, genus_distance, genus_phonetic
    ))
  }

  species_distance <- species_match$edit_distance[species_match$match]
  species_phonetic <- species_match$phonetic[species_match$match]
  parent_genus <- index$species$parent[accepted]
  genus_slot <- match(parent_genus, matched_genera)

  # 5. Infraspecific epithet within the matched species, when one was submitted
  infra <- parsed$infra1
  if (nzchar(infra)) {
    infra_rows <- tnrs_match_infraspecific(
      infra, accepted, index, genus_distance[genus_slot],
      species_distance, genus_phonetic[genus_slot] & species_phonetic
    )
    if (!is.null(infra_rows)) {
      return(infra_rows)
    }
    # Nothing matched below species; fall through to the species-rank answer
  }

  rows <- lapply(seq_along(accepted), function(i) {
    hits <- tnrs_int_lookup(index$rows_by_species, accepted[i])
    if (length(hits) == 0) {
      return(NULL)
    }
    # Prefer the species-rank rows; an infraspecific row would misrepresent a
    # query that carried no infraspecific epithet
    at_rank <- hits[!nzchar(names$infraspecific_epithet[hits])]
    if (length(at_rank) > 0) {
      hits <- at_rank
    }
    tnrs_candidate_frame(
      hits, "species",
      genus_ed = genus_distance[genus_slot[i]],
      species_ed = species_distance[i],
      phonetic = genus_phonetic[genus_slot[i]] && species_phonetic[i]
    )
  })
  rows <- do.call(rbind, rows)

  if (is.null(rows)) empty else rows
}

#' Match an infraspecific epithet within already-matched species
#'
#' Internal.  The last step of the cascade.  Returns NULL when nothing matches,
#' so the caller can fall back to the species-rank answer, which is what
#' upstream does.
#' @keywords internal
#' @noRd
tnrs_match_infraspecific <- function(infra, species_positions, index,
                                     genus_distance, species_distance,
                                     phonetic_so_far) {
  candidates <- tnrs_child_candidates(infra, species_positions, index$infra1)
  if (length(candidates) == 0) {
    return(NULL)
  }

  judged <- tnrs_match_component(
    infra, index$infra1$upper[candidates], "epithet",
    candidate_key = index$infra1$key[candidates]
  )
  accepted <- candidates[judged$match]
  if (length(accepted) == 0) {
    return(NULL)
  }

  distance <- judged$edit_distance[judged$match]
  phonetic <- judged$phonetic[judged$match]
  species_slot <- match(index$infra1$parent[accepted], species_positions)

  rows <- lapply(seq_along(accepted), function(i) {
    hits <- tnrs_int_lookup(index$rows_by_infra1, accepted[i])
    if (length(hits) == 0) {
      return(NULL)
    }
    tnrs_candidate_frame(
      hits, "infra1",
      genus_ed = genus_distance[species_slot[i]],
      species_ed = species_distance[species_slot[i]],
      infra1_ed = distance[i],
      phonetic = phonetic_so_far[species_slot[i]] && phonetic[i]
    )
  })
  rows <- do.call(rbind, rows)

  rows
}

#' Fall back to the genus when the epithet cannot be matched
#' @keywords internal
#' @noRd
tnrs_match_genus_only <- function(parsed, backbone, matched_genera,
                                  genus_distance, genus_phonetic) {
  index <- backbone$index
  names <- backbone$names

  rows <- lapply(seq_along(matched_genera), function(i) {
    hits <- tnrs_int_lookup(index$rows_by_genus, matched_genera[i])
    hits <- hits[names$name_rank[hits] == "genus"]
    if (length(hits) == 0) {
      return(NULL)
    }
    tnrs_candidate_frame(
      hits, "genus",
      genus_ed = genus_distance[i], phonetic = genus_phonetic[i]
    )
  })
  rows <- do.call(rbind, rows)

  if (is.null(rows)) {
    tnrs_match_family_only(parsed, backbone, "normal")
  } else {
    rows
  }
}

#' Fall back to the family when nothing below it matched
#' @keywords internal
#' @noRd
tnrs_match_family_only <- function(parsed, backbone, search_mode = "normal") {
  empty <- tnrs_candidate_frame(
    integer(0), character(0),
    genus_ed = integer(0), species_ed = integer(0), infra1_ed = integer(0),
    family_ed = integer(0), phonetic = logical(0)
  )

  family <- parsed$family
  if (is.null(family) || !nzchar(family)) {
    return(empty)
  }

  index <- backbone$index
  names <- backbone$names

  candidates <- tnrs_family_candidates(family, index$family, search_mode)
  if (length(candidates) == 0) {
    return(empty)
  }

  judged <- tnrs_match_component(
    family, index$family$upper[candidates], "family",
    search_mode = search_mode,
    candidate_key = index$family$key[candidates]
  )
  matched <- candidates[judged$match]
  if (length(matched) == 0) {
    return(empty)
  }

  # A family match points at the family name itself where the source carries
  # one as a name in its own right
  rows <- lapply(seq_along(matched), function(i) {
    hits <- tnrs_lookup(
      index$rows_by_name,
      tnrs_toupper_ascii(index$family$name[matched[i]])
    )
    if (length(hits) == 0) {
      return(NULL)
    }
    tnrs_candidate_frame(
      hits, "family",
      family_ed = judged$edit_distance[judged$match][i],
      phonetic = judged$phonetic[judged$match][i]
    )
  })
  rows <- do.call(rbind, rows)

  if (is.null(rows)) empty else rows
}

#' Score and rank a query's candidate matches
#'
#' Internal.  Applies the component scores, the name and overall scores, and the
#' ordering from \code{TnrsAggregator::cmp()}: deepest matched rank first, then
#' smallest edit distance, then highest name score, then highest overall score,
#' then accepted names ahead of synonyms, then alphabetically, then by the order
#' the sources were requested.
#'
#' @param candidates Output of \code{tnrs_match_one()}.
#' @param parsed The parsed query.
#' @param names The source's name table.
#' @param source_order Position of this source in the user's `sources` argument.
#' @return The candidates with score columns added, ordered best first.
#' @keywords internal
#' @noRd
tnrs_score_candidates <- function(candidates, parsed, names, source_order = 1L) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }

  rows <- candidates$row

  candidates$name_matched <- names$scientific_name[rows]
  candidates$genus_matched <- names$genus[rows]
  candidates$species_matched <- names$specific_epithet[rows]
  candidates$family_matched <- names$family[rows]

  candidates$genus_score <- ifelse(
    is.na(candidates$genus_ed), NA_real_,
    tnrs_ed_score(
      ifelse(is.na(candidates$genus_ed), 0L, candidates$genus_ed),
      candidates$genus_matched, parsed$genus
    )
  )
  candidates$species_score <- ifelse(
    is.na(candidates$species_ed), NA_real_,
    tnrs_ed_score(
      ifelse(is.na(candidates$species_ed), 0L, candidates$species_ed),
      candidates$species_matched, parsed$species
    )
  )
  candidates$infra1_matched <- names$infraspecific_epithet[rows]
  candidates$rank_indicator <- names$rank_indicator[rows]
  candidates$infra1_score <- ifelse(
    is.na(candidates$infra1_ed), NA_real_,
    tnrs_ed_score(
      ifelse(is.na(candidates$infra1_ed), 0L, candidates$infra1_ed),
      candidates$infra1_matched, parsed$infra1
    )
  )
  # A match at the wrong infraspecific rank is accepted but penalised, so that
  # a correctly ranked alternative outranks it.  The row's rank indicator
  # describes its own rank, so a name matched at the second infraspecific level
  # must be judged against the second submitted rank, not the first.
  query_rank <- ifelse(
    candidates$matched_rank == "infra2", parsed$rank2, parsed$rank1
  )

  wrong_rank <- !is.na(candidates$infra1_score) & nzchar(query_rank) &
    nzchar(candidates$rank_indicator) &
    !tnrs_same_rank(query_rank, candidates$rank_indicator)
  candidates$infra1_score[wrong_rank] <-
    candidates$infra1_score[wrong_rank] - tnrs_rank_penalty()

  candidates$family_score <- ifelse(
    is.na(candidates$family_ed), NA_real_,
    tnrs_ed_score(
      ifelse(is.na(candidates$family_ed), 0L, candidates$family_ed),
      candidates$family_matched, parsed$family
    )
  )

  # match_score sums the components that were actually compared; parsed_part is
  # how many components the query offered
  component_sum <- rowSums(
    cbind(
      ifelse(is.na(candidates$genus_score), 0, candidates$genus_score),
      ifelse(is.na(candidates$species_score), 0, candidates$species_score),
      ifelse(is.na(candidates$infra1_score), 0, candidates$infra1_score),
      ifelse(is.na(candidates$family_score), 0, candidates$family_score)
    )
  )
  parsed_part <- max(
    1L,
    sum(nzchar(c(parsed$genus, parsed$species, parsed$infra1))) +
      as.integer(nzchar(parsed$family))
  )

  candidates$name_score <- tnrs_num_to_score(component_sum, parsed_part)

  author_matched <- names$authorship[rows]
  candidates$author_score <- if (nzchar(parsed$authorship)) {
    tnrs_compare_auth(parsed$authorship, author_matched)
  } else {
    rep(NA_real_, nrow(candidates))
  }

  candidates$overall_score <- tnrs_overall_score(
    candidates$name_score, candidates$author_score, 0
  )

  candidates$rank_index <- tnrs_rank_index(candidates$matched_rank)
  candidates$status <- names$taxonomic_status[rows]
  candidates$acceptance <- c(
    "Accepted" = 2L, "Synonym" = 1L, "Illegitimate" = 1L,
    "Invalid" = 1L, "Unchecked" = 0L, "Unplaced" = 0L
  )[candidates$status]
  candidates$acceptance[is.na(candidates$acceptance)] <- 0L
  candidates$source_order <- source_order

  # The upstream ordering, deepest match first
  total_ed <- tnrs_total_edit_distance(candidates)

  candidates[order(
    -candidates$rank_index,
    total_ed,
    -candidates$name_score,
    -candidates$overall_score,
    -candidates$acceptance,
    candidates$name_matched,
    candidates$source_order
  ), ]
}

#' Summed edit distance across the compared components
#' @keywords internal
#' @noRd
tnrs_total_edit_distance <- function(candidates) {
  zero <- function(x) ifelse(is.na(x), 0L, x)
  zero(candidates$genus_ed) + zero(candidates$species_ed) +
    zero(candidates$infra1_ed) + zero(candidates$family_ed)
}

#' Order candidates under one of the two upstream sort schemes
#'
#' Internal.  R port of \code{TnrsAggregator::cmpMatched()}.  The two schemes
#' share a tail; "highertaxa" prefixes it with a walk down the ranks comparing
#' each one's edit distance in turn, so that a match agreeing at the higher taxa
#' sorts first.  Upstream runs both and warns when they disagree, which is what
#' the HigherTaxa and Overall warning flags mean.
#'
#' @param candidates A scored candidate frame.
#' @param scheme "overall" or "highertaxa".
#' @return Integer ordering vector.
#' @keywords internal
#' @noRd
tnrs_rank_order <- function(candidates, scheme = c("overall", "highertaxa")) {
  scheme <- match.arg(scheme)
  zero <- function(x) ifelse(is.na(x), 0L, x)

  keys <- list()
  if (scheme == "highertaxa") {
    keys <- list(
      zero(candidates$family_ed), zero(candidates$genus_ed),
      zero(candidates$species_ed), zero(candidates$infra1_ed)
    )
  }

  keys <- c(keys, list(
    -candidates$name_score,
    -candidates$overall_score,
    -candidates$acceptance,
    candidates$name_matched,
    candidates$source_order
  ))

  do.call(order, keys)
}
