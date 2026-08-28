#' Score a single matched name component
#'
#' Internal.  R port of \code{TnrsAggregator::getEDScore()}.  Produces the
#' per-component scores (\code{Genus_score}, \code{Specific_epithet_score},
#' \code{Family_score} and the two infraspecific scores).
#'
#' @param ed Edit distance between the two strings.
#' @param matched,submitted Character vectors of the compared strings.
#' @return Numeric score on a 0-1 scale; 0 when both strings are empty.
#' @keywords internal
#' @noRd
tnrs_ed_score <- function(ed, matched, submitted) {
  n <- max(length(ed), length(matched), length(submitted))
  ed <- rep_len(ed, n)
  denom <- pmax(rep_len(nchar(matched), n), rep_len(nchar(submitted), n))

  score <- rep(0, n)
  nonzero <- denom > 0
  score[nonzero] <- 1 - ed[nonzero] / denom[nonzero]
  score
}

#' Squash a summed component score onto a 0-1 scale
#'
#' Internal.  R port of \code{TnrsAggregator::num_to_score()}, the arctangent
#' transform that turns the sum of the component scores into \code{Name_score}.
#' Upstream always calls it with \code{s = 2} and \code{t = 1}.
#'
#' @param num Sum of the component scores.
#' @param max Number of name components that were parsed.
#' @param s,t Shape parameters.
#' @return Numeric score on a 0-1 scale.
#' @keywords internal
#' @noRd
tnrs_num_to_score <- function(num, max, s = 2, t = 1) {
  num <- 2 * num - max
  atan((s * num / max)^(2 * t + 1)) / (2 * atan(s^(2 * t + 1))) + 0.5
}

#' Combine the name and author scores into the overall score
#'
#' Internal.  R port of \code{TnrsAggregator::getOverallScore()}.  The author
#' score contributes a fixed weight when it is present; unmatched terms attract a
#' flat penalty.
#'
#' @param name_score Numeric \code{Name_score}.
#' @param author_score Numeric \code{Author_score}, or NA where the name carried
#'   no authority, in which case the name score is used unchanged.
#' @param extra_part Surplus term count from \code{tnrs_extra_part()}.  Note that
#'   this is not simply "are there unmatched terms": unmatched words that the
#'   rank arithmetic accounts for do not attract the penalty.
#' @return Numeric \code{Overall_score}.
#' @keywords internal
#' @noRd
tnrs_overall_score <- function(name_score, author_score = NA_real_, extra_part = 0) {
  # Recycled explicitly: ifelse() returns a result the length of its condition,
  # so a scalar extra_part would otherwise silently collapse a vector of scores.
  n <- max(length(name_score), length(author_score), length(extra_part))
  name_score <- rep_len(name_score, n)
  author_score <- rep_len(author_score, n)
  extra_part <- rep_len(extra_part, n)

  weight <- tnrs_author_weight()
  score <- ifelse(
    is.na(author_score),
    name_score,
    name_score * (1 - weight) + author_score * weight
  )

  score - tnrs_extra_penalty() * (extra_part > 0)
}

#' Count the surplus terms in a name, for the overall-score penalty
#'
#' Internal.  R port of the \code{extra_part} arithmetic in
#' \code{TnrsAggregator}.  A name is penalised only when it carries more words
#' than the ranks it matched and parsed can account for, so a leftover family
#' name or infraspecific rank indicator does \emph{not} trigger the penalty
#' while, say, a duplicated family name does.
#'
#' \deqn{extra = unmatched + matched - parsed - [rank1] - [rank2]}
#'
#' @param unmatched_part Number of space-separated words in the unmatched terms.
#' @param matched_part Rank index of the matched rank, plus one if a family was
#'   matched and one for each matched infraspecific rank indicator.
#' @param parsed_part Rank index of the lowest parsed rank, plus one if a family
#'   was parsed.
#' @param has_rank1,has_rank2 Was an infraspecific rank indicator parsed?
#' @return Integer surplus; the penalty applies when this is greater than zero.
#' @keywords internal
#' @noRd
tnrs_extra_part <- function(unmatched_part, matched_part, parsed_part,
                            has_rank1 = FALSE, has_rank2 = FALSE) {
  n <- max(
    length(unmatched_part), length(matched_part), length(parsed_part),
    length(has_rank1), length(has_rank2)
  )
  unmatched_part <- rep_len(unmatched_part, n)

  extra <- unmatched_part + rep_len(matched_part, n) - rep_len(parsed_part, n) -
    as.integer(rep_len(has_rank1, n)) - as.integer(rep_len(has_rank2, n))

  # Upstream only enters this branch when there are unmatched terms at all
  extra[unmatched_part <= 0] <- 0L
  extra
}

#' Rank ordering used throughout the aggregator
#'
#' Lower is higher in the hierarchy.  Used both for the surplus arithmetic and
#' for ranking candidate matches, where a deeper match wins.
#' @keywords internal
#' @noRd
tnrs_rank_index <- function(rank) {
  idx <- c(family = 0L, genus = 1L, species = 2L, infra1 = 3L, infra2 = 4L)
  unname(idx[rank])
}

# Scoring constants, from class.tnrs_aggregator.php and tnrsapi/params.php.
# Kept as functions so that there is a single definition to change if the
# upstream values ever move.

#' @keywords internal
#' @noRd
tnrs_author_weight <- function() 0.2

#' @keywords internal
#' @noRd
tnrs_extra_penalty <- function() 0.1

#' @keywords internal
#' @noRd
tnrs_rank_penalty <- function() 0.3

#' @keywords internal
#' @noRd
tnrs_default_accuracy <- function() 0.53
