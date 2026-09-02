#' Assemble a zoological author citation from its parts
#'
#' Internal.  The zoological code writes the year as part of the citation, and
#' wraps both in parentheses when the species has since been moved out of the
#' genus it was described in.  The Mammal Diversity Database keeps the three
#' pieces apart, so they are put back together here in the form the matcher
#' compares against.
#'
#' @param author Author string.
#' @param year Year of publication.
#' @param parentheses "1" where the citation is parenthesised.
#' @return One citation per row.
#' @keywords internal
#' @noRd
tnrs_zoological_author <- function(author, year, parentheses) {
  author <- trimws(ifelse(is.na(author), "", author))
  year <- trimws(ifelse(is.na(year), "", year))

  out <- ifelse(
    nzchar(author) & nzchar(year), paste0(author, ", ", year),
    ifelse(nzchar(author), author, year)
  )
  wrap <- !is.na(parentheses) & parentheses == "1" & nzchar(out)
  out[wrap] <- paste0("(", out[wrap], ")")
  out
}

#' Statuses used by the Mammal Diversity Database
#'
#' Internal.  Its synonym file records why each name is not the accepted one,
#' in a vocabulary of its own.  The names that are merely doubtful must not be
#' folded in with the synonyms: a synonym points at an accepted name, whereas a
#' nomen dubium points at nothing, and treating one as the other would have the
#' resolver report an accepted name that its own source declines to give.
#' @keywords internal
#' @noRd
tnrs_mdd_status <- function(validity) {
  validity <- tolower(trimws(as.character(validity)))
  known <- c(
    "species" = "Accepted",
    "hybrid" = "Accepted",
    "synonym" = "Synonym",
    "nomen_dubium" = "Unchecked",
    "species_inquirenda" = "Unchecked",
    "composite" = "Unchecked",
    "unavailable" = "Invalid"
  )
  out <- unname(known[validity])
  out[is.na(out)] <- "Unchecked"
  out
}

#' Read the Mammal Diversity Database into the local name table
#'
#' Internal.  The MDD publishes two files: the accepted species, and every name
#' ever applied to them.  Both are needed, because the first carries the
#' accepted names in their current combination while the second carries the
#' synonyms, and neither alone lets a submitted synonym resolve to an accepted
#' name.
#'
#' The synonym file repeats each accepted species as a row of its own, under
#' the combination the name was published in rather than the one in use.  Where
#' the two differ that row is a synonym worth keeping, and often the obvious one
#' to look up: the lion is filed there as "Felis leo".  Where they agree it is
#' dropped, since the species file already carries the name.
#'
#' @param species_path Path to the species file.
#' @param synonym_path Path to the synonym file.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_mdd <- function(species_path, synonym_path, quiet = FALSE) {
  for (path in c(species_path, synonym_path)) {
    if (!file.exists(path)) {
      stop("MDD file not found: ", path, call. = FALSE)
    }
  }

  if (!quiet) message("Reading MDD species ...")
  species <- utils::read.csv(
    species_path,
    colClasses = "character", na.strings = character(0),
    stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"
  )

  # The MDD writes a binomial with an underscore, being a database key as much
  # as a name; the matcher wants the name
  accepted_name <- gsub("_", " ", species$sciName, fixed = TRUE)
  keep <- nzchar(accepted_name)
  species <- species[keep, , drop = FALSE]
  accepted_name <- accepted_name[keep]

  accepted <- data.frame(
    source_name_id = species$id,
    scientific_name = accepted_name,
    authorship = tnrs_zoological_author(
      species$authoritySpeciesAuthor, species$authoritySpeciesYear,
      species$authorityParentheses
    ),
    name_rank = "species",
    taxonomic_status = "Accepted",
    family = tnrs_title_case(species$family),
    genus = species$genus,
    specific_epithet = species$specificEpithet,
    infraspecific_epithet = "",
    accepted_source_name_id = species$id,
    stringsAsFactors = FALSE
  )

  if (!quiet) message("Reading MDD synonyms ...")
  synonyms <- utils::read.csv(
    synonym_path,
    colClasses = "character", na.strings = character(0),
    stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"
  )

  syn_name <- trimws(synonyms$MDD_normalized_original_combination)
  syn_name[is.na(syn_name)] <- ""
  status <- tnrs_mdd_status(synonyms$MDD_validity)

  # A row marked "species" is the accepted taxon recorded under the combination
  # it was published in.  Where that differs from the name in use it is a
  # synonym, and a common one to be given: the lion is filed here as
  # "Felis leo".  Where it is the same it is dropped, since the species file
  # already carries it.
  current <- gsub("_", " ", ifelse(is.na(synonyms$MDD_species), "",
    synonyms$MDD_species
  ), fixed = TRUE)
  original <- tolower(status) == "accepted"
  status[original] <- ifelse(syn_name[original] == current[original],
    "drop", "Synonym"
  )

  # A name that is not a synonym of anything points at no accepted name, so it
  # is not given one to point at
  accepted_id <- ifelse(status == "Synonym", synonyms$MDD_species_id, "")
  accepted_id[is.na(accepted_id)] <- ""

  # The genus and epithet columns describe the accepted taxon, not the name on
  # the row, so "Rattus latidens" carries genus Abditomys.  Taking them at face
  # value would index every synonym under a genus it is not spelled with, and
  # so make it unfindable.  A zoological name has no rank connector, so its
  # parts are simply its words.
  parts <- strsplit(syn_name, "[[:space:]]+")
  part <- function(i) {
    vapply(parts, function(p) if (length(p) >= i) p[[i]] else "", character(1))
  }
  syn_genus <- part(1L)
  syn_species <- part(2L)
  syn_infra <- part(3L)

  other <- data.frame(
    source_name_id = paste0("syn-", synonyms$MDD_syn_ID),
    scientific_name = syn_name,
    authorship = tnrs_zoological_author(
      synonyms$MDD_author, synonyms$MDD_year,
      synonyms$MDD_authority_parentheses
    ),
    name_rank = ifelse(
      nzchar(syn_infra), "subspecies",
      ifelse(nzchar(syn_species), "species", "genus")
    ),
    taxonomic_status = status,
    family = tnrs_title_case(synonyms$MDD_family),
    genus = syn_genus,
    specific_epithet = syn_species,
    infraspecific_epithet = syn_infra,
    accepted_source_name_id = accepted_id,
    stringsAsFactors = FALSE
  )
  other <- other[nzchar(other$scientific_name) & other$taxonomic_status != "drop", ,
    drop = FALSE
  ]

  combined <- rbind(accepted, other)

  out <- data.frame(
    name_id = seq_len(nrow(combined)),
    source = "mdd",
    source_name_id = combined$source_name_id,
    scientific_name = combined$scientific_name,
    authorship = combined$authorship,
    name_rank = combined$name_rank,
    taxonomic_status = combined$taxonomic_status,
    family = combined$family,
    genus = combined$genus,
    specific_epithet = combined$specific_epithet,
    # The zoological code uses no connector before a subspecific epithet, so
    # there is no indicator to record
    rank_indicator = "",
    infraspecific_epithet = combined$infraspecific_epithet,
    is_hybrid = FALSE,
    url = "",
    accepted_source_name_id = combined$accepted_source_name_id,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }
  out
}

#' Is a value both present and non-empty?
#' @keywords internal
#' @noRd
nzchar2 <- function(x) !is.na(x) & nzchar(x)

#' Title case a family name
#'
#' Internal.  Families are reported capitalised, and a source that writes them
#' otherwise would show through into the output.
#' @keywords internal
#' @noRd
tnrs_title_case <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  lower <- tolower(x)
  substr(lower, 1, 1) <- toupper(substr(lower, 1, 1))
  lower
}
