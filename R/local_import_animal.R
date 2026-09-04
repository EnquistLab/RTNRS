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
    # The species file classifies each species up to its order
    order = if (is.null(species$order)) "" else tnrs_title_case(species$order),
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
    # Taken from the accepted name once the table is linked
    order = "",
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
    # Every name here is a mammal, and the order is the deepest rank above
    # the family the source gives
    kingdom = "Animalia", phylum = "Chordata", class = "Mammalia",
    order = combined$order,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }
  out
}

#' Read the PHYLACINE synonymy table into the local name table
#'
#' Internal.  PHYLACINE 1.2 publishes one row per species it accepts, giving
#' the binomial with its order, family and genus, and beside it the name the
#' same species carries in four other lists: PHYLACINE 1.0 and 1.1,
#' EltonTraits 1.0 and IUCN 2016-3.  Rows whose PHYLACINE fields read
#' \code{000 Species not accepted} are names in those lists that PHYLACINE
#' rejects.  There is no authorship, no rank below species and no status
#' vocabulary; all of that is implied by the row's shape.
#'
#' Every alternative binomial that differs from the accepted one becomes a
#' synonym pointing at it, so a name from any of the four lists resolves to
#' PHYLACINE's; a rejected name becomes Unplaced with no accepted name.  A
#' blank epithet, which the table has once, is dropped rather than imported
#' as a bare genus that would then match every species of it.
#'
#' @param path Path to \code{Synonymy_table_with_unaccepted_species.csv}.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_phylacine <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    stop("PHYLACINE synonymy table not found: ", path, call. = FALSE)
  }
  if (!quiet) message("Reading PHYLACINE synonymy table ...")

  raw <- utils::read.csv(
    path,
    colClasses = "character", na.strings = character(0),
    stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"
  )
  needed <- c(
    "Binomial.1.2", "Order.1.2", "Family.1.2", "Genus.1.2", "Species.1.2",
    "Genus.1.1", "Species.1.1", "Genus.1.0", "Species.1.0",
    "EltonTraits.1.0.Genus", "EltonTraits.1.0.Species",
    "IUCN.2016.3.Genus", "IUCN.2016.3.Species"
  )
  absent <- setdiff(needed, names(raw))
  if (length(absent) > 0) {
    stop(
      "The PHYLACINE synonymy table is missing the column(s) ",
      paste(absent, collapse = ", "), ". The published format may have changed.",
      call. = FALSE
    )
  }

  gone <- "000 Species not accepted"
  blank <- function(v) is.na(v) | !nzchar(trimws(v)) | v == gone
  binomial <- function(genus, epithet) {
    ifelse(blank(genus) | blank(epithet), "", paste(trimws(genus), trimws(epithet)))
  }

  accepted <- raw[raw$Binomial.1.2 != gone, , drop = FALSE]
  current <- gsub("_", " ", accepted$Binomial.1.2, fixed = TRUE)

  # The four other lists' names for each accepted species, kept where they
  # differ from the current one
  others <- lapply(
    list(
      c("Genus.1.1", "Species.1.1"), c("Genus.1.0", "Species.1.0"),
      c("EltonTraits.1.0.Genus", "EltonTraits.1.0.Species"),
      c("IUCN.2016.3.Genus", "IUCN.2016.3.Species")
    ),
    function(cols) {
      name <- binomial(accepted[[cols[1]]], accepted[[cols[2]]])
      keep <- nzchar(name) & name != current
      data.frame(name = name[keep], to = accepted$Binomial.1.2[keep], stringsAsFactors = FALSE)
    }
  )
  synonyms <- unique(do.call(rbind, others))

  # Names those lists carry that PHYLACINE does not accept, taken from
  # whichever list has them
  rejected <- raw[raw$Binomial.1.2 == gone, , drop = FALSE]
  rejected_name <- rep("", nrow(rejected))
  for (cols in list(
    c("EltonTraits.1.0.Genus", "EltonTraits.1.0.Species"),
    c("IUCN.2016.3.Genus", "IUCN.2016.3.Species"),
    c("Genus.1.1", "Species.1.1"), c("Genus.1.0", "Species.1.0")
  )) {
    candidate <- binomial(rejected[[cols[1]]], rejected[[cols[2]]])
    take <- !nzchar(rejected_name) & nzchar(candidate)
    rejected_name[take] <- candidate[take]
  }
  rejected_name <- unique(rejected_name[nzchar(rejected_name)])

  first_word <- function(x) sub("\\s.*$", "", x)
  rest <- function(x) sub("^\\S+\\s+", "", x)

  rows <- rbind(
    data.frame(
      source_name_id = accepted$Binomial.1.2, scientific_name = current,
      taxonomic_status = "Accepted", accepted_source_name_id = accepted$Binomial.1.2,
      family = trimws(accepted$Family.1.2), order = trimws(accepted$Order.1.2),
      genus = trimws(accepted$Genus.1.2), specific_epithet = trimws(accepted$Species.1.2),
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_name_id = paste0("syn-", seq_len(nrow(synonyms))), scientific_name = synonyms$name,
      taxonomic_status = "Synonym", accepted_source_name_id = synonyms$to,
      # Classification comes from the accepted name, filled in after linking
      family = "", order = "",
      genus = first_word(synonyms$name), specific_epithet = rest(synonyms$name),
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_name_id = paste0("rej-", seq_along(rejected_name)), scientific_name = rejected_name,
      taxonomic_status = "Unplaced", accepted_source_name_id = "",
      family = "", order = "",
      genus = first_word(rejected_name), specific_epithet = rest(rejected_name),
      stringsAsFactors = FALSE
    )
  )

  out <- data.frame(
    name_id = seq_len(nrow(rows)),
    source = "phylacine",
    source_name_id = rows$source_name_id,
    scientific_name = rows$scientific_name,
    authorship = "",
    name_rank = "species",
    taxonomic_status = rows$taxonomic_status,
    family = rows$family,
    genus = rows$genus,
    specific_epithet = rows$specific_epithet,
    rank_indicator = "",
    infraspecific_epithet = "",
    is_hybrid = FALSE,
    url = "",
    accepted_source_name_id = rows$accepted_source_name_id,
    kingdom = "Animalia", phylum = "Chordata", class = "Mammalia",
    order = rows$order,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message(
      "  ", format(sum(out$taxonomic_status == "Accepted"), big.mark = ","), " accepted species, ",
      format(sum(out$taxonomic_status == "Synonym"), big.mark = ","), " names from other lists, ",
      sum(out$taxonomic_status == "Unplaced"), " rejected"
    )
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
