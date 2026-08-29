#' Columns kept from a source checklist
#'
#' Internal.  The local name table is a subset of the TNRS core \code{name}
#' table: the fields the matcher, the scorer and the resolver actually use.
#' @keywords internal
#' @noRd
tnrs_name_columns <- function() {
  c(
    "name_id", "source", "source_name_id", "scientific_name", "authorship",
    "name_rank", "taxonomic_status", "family", "genus", "specific_epithet",
    "rank_indicator", "infraspecific_epithet", "is_hybrid", "url",
    "accepted_source_name_id"
  )
}

#' Check a source file's header before reading it
#'
#' Reading with \code{colClasses} fails with an obscure message when a source
#' changes its layout.  Validating the header first turns that into an
#' actionable error, and guarantees the columns are never silently mis-mapped
#' onto the wrong fields.
#'
#' @param path File to check.
#' @param expected Character vector of expected column names, in order.
#' @param sep Field separator.
#' @param source Source name, for the error message.
#' @keywords internal
#' @noRd
tnrs_check_header <- function(path, expected, sep, source) {
  header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (length(header) == 0) {
    stop("Source file for '", source, "' is empty: ", path, call. = FALSE)
  }

  found <- strsplit(header, sep, fixed = TRUE)[[1]]
  found <- gsub('"', "", found, fixed = TRUE)

  if (!identical(found, expected)) {
    stop(
      "Unexpected ", toupper(source), " column layout in ", path,
      ". The published format may have changed. Expected ",
      length(expected), " columns (", paste(expected, collapse = ", "),
      ") but found ", length(found), " (", paste(found, collapse = ", "), ").",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Column layout of the WCVP names file, v15
#' @keywords internal
#' @noRd
tnrs_wcvp_columns <- function() {
  c(
    "plant_name_id", "ipni_id", "taxon_rank", "taxon_status", "family",
    "genus_hybrid", "genus", "species_hybrid", "species",
    "infraspecific_rank", "infraspecies", "parenthetical_author",
    "primary_author", "publication_author", "place_of_publication",
    "volume_and_page", "first_published", "nomenclatural_remarks",
    "geographic_area", "lifeform_description", "climate_description",
    "taxon_name", "taxon_authors", "accepted_plant_name_id",
    "basionym_plant_name_id", "replaced_synonym_author", "homotypic_synonym",
    "parent_plant_name_id", "powo_id", "hybrid_formula", "reviewed"
  )
}

#' Column layout of the WFO classification file, 2025-12
#' @keywords internal
#' @noRd
tnrs_wfo_columns <- function() {
  c(
    "taxonID", "scientificNameID", "localID", "scientificName", "taxonRank",
    "parentNameUsageID", "scientificNameAuthorship", "family", "subfamily",
    "tribe", "subtribe", "genus", "subgenus", "specificEpithet",
    "infraspecificEpithet", "verbatimTaxonRank", "nomenclaturalStatus",
    "namePublishedIn", "taxonomicStatus", "acceptedNameUsageID",
    "originalNameUsageID", "nameAccordingToID", "taxonRemarks", "created",
    "modified", "references", "source", "majorGroup", "tplID"
  )
}

#' Read the World Checklist of Vascular Plants into the local name table
#'
#' Internal.  WCVP ships one pipe-delimited file of names whose columns map
#' almost directly onto the TNRS core schema.  Only the columns the matcher uses
#' are read, which keeps the memory cost down; base R is used rather than a
#' faster reader because this runs once per build and adding a dependency for it
#' is not worth the weight.
#'
#' @param path Path to \code{wcvp_names.csv}.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_wcvp <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    stop("WCVP names file not found: ", path, call. = FALSE)
  }

  # Column positions in the v15 layout.  Read only what is needed; everything
  # else is skipped by the reader rather than loaded and discarded.
  wanted <- c(
    plant_name_id = 1L, taxon_rank = 3L, taxon_status = 4L, family = 5L,
    genus_hybrid = 6L, genus = 7L, species_hybrid = 8L, species = 9L,
    infraspecific_rank = 10L, infraspecies = 11L, taxon_name = 22L,
    taxon_authors = 23L, accepted_plant_name_id = 24L, powo_id = 29L
  )
  n_columns <- 31L

  tnrs_check_header(path, tnrs_wcvp_columns(), "|", "wcvp")

  classes <- rep("NULL", n_columns)
  classes[wanted] <- "character"

  if (!quiet) message("Reading WCVP names ...")

  raw <- utils::read.table(
    path,
    sep = "|", quote = "", header = TRUE, comment.char = "",
    colClasses = classes, na.strings = character(0),
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

  keep <- nzchar(raw$taxon_name)
  raw <- raw[keep, , drop = FALSE]

  out <- data.frame(
    name_id = seq_len(nrow(raw)),
    source = "wcvp",
    source_name_id = raw$plant_name_id,
    scientific_name = raw$taxon_name,
    authorship = raw$taxon_authors,
    name_rank = tolower(raw$taxon_rank),
    taxonomic_status = raw$taxon_status,
    family = raw$family,
    genus = raw$genus,
    specific_epithet = raw$species,
    rank_indicator = raw$infraspecific_rank,
    infraspecific_epithet = raw$infraspecies,
    is_hybrid = nzchar(raw$genus_hybrid) | nzchar(raw$species_hybrid),
    url = ifelse(nzchar(raw$powo_id),
      paste0("https://powo.science.kew.org/taxon/", raw$powo_id), ""
    ),
    accepted_source_name_id = raw$accepted_plant_name_id,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }

  out
}

#' Read the World Flora Online backbone into the local name table
#'
#' Internal.  WFO ships a single tab-delimited Darwin Core classification file.
#' Two things need care: it is nearly a gigabyte uncompressed, so only the needed
#' columns are read; and it contains a handful of invalid UTF-8 byte sequences,
#' which make a re-encoding connection drop rows silently.  The file is therefore
#' read with the encoding declared rather than converted, and invalid sequences
#' are stripped afterwards.
#'
#' @param path Path to \code{classification.csv}.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_wfo <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    stop("WFO classification file not found: ", path, call. = FALSE)
  }

  wanted <- c(
    taxonID = 1L, scientificName = 4L, taxonRank = 5L,
    scientificNameAuthorship = 7L, family = 8L, genus = 12L,
    specificEpithet = 14L, infraspecificEpithet = 15L,
    taxonomicStatus = 19L, acceptedNameUsageID = 20L, references = 26L
  )
  n_columns <- 29L

  tnrs_check_header(path, tnrs_wfo_columns(), "	", "wfo")

  classes <- rep("NULL", n_columns)
  classes[wanted] <- "character"

  if (!quiet) message("Reading WFO classification ...")

  raw <- utils::read.table(
    path,
    sep = "\t", quote = "\"", header = TRUE, comment.char = "",
    colClasses = classes, na.strings = character(0),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  # Drop invalid byte sequences rather than losing the rows that carry them
  for (column in names(raw)) {
    raw[[column]] <- iconv(raw[[column]], "UTF-8", "UTF-8", sub = "")
  }

  keep <- nzchar(raw$scientificName)
  raw <- raw[keep, , drop = FALSE]

  out <- data.frame(
    name_id = seq_len(nrow(raw)),
    source = "wfo",
    source_name_id = raw$taxonID,
    scientific_name = raw$scientificName,
    authorship = raw$scientificNameAuthorship,
    name_rank = tolower(raw$taxonRank),
    taxonomic_status = raw$taxonomicStatus,
    family = raw$family,
    genus = raw$genus,
    specific_epithet = raw$specificEpithet,
    rank_indicator = tnrs_wfo_rank_indicator(raw$taxonRank),
    infraspecific_epithet = raw$infraspecificEpithet,
    # WFO carries no hybrid flag, so it is read off the name itself.
    # U+00D7 is the multiplication sign used as the hybrid marker; built by
    # code point to keep this file ASCII.
    is_hybrid = grepl(intToUtf8(0x00D7), raw$scientificName, fixed = TRUE) |
      grepl("^x | x ", raw$scientificName),
    url = ifelse(nzchar(raw$references), raw$references,
      paste0("https://www.worldfloraonline.org/taxon/", raw$taxonID)
    ),
    accepted_source_name_id = raw$acceptedNameUsageID,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }

  out
}

#' Map WFO rank names onto the standard rank indicators
#'
#' WFO spells ranks out in full.  Most map through the shared rank table; "form"
#' is spelled without its final "a" in WFO and is handled here rather than by
#' loosening the shared table, which is a faithful copy of the upstream one.
#' @keywords internal
#' @noRd
tnrs_wfo_rank_indicator <- function(rank) {
  rank <- tolower(rank)
  out <- tnrs_standardize_rank(rank)
  out[rank == "form"] <- "fo."
  # Ranks at or above species carry no indicator
  out[rank %in% c("species", "genus", "family", "unranked", "")] <- ""
  out
}

#' Resolve within-source accepted-name links
#'
#' Internal.  Sources identify accepted names by their own identifiers; this
#' translates those into positions in the local name table so that resolution is
#' a lookup rather than a join.
#'
#' @param names A name table from one of the importers.
#' @return The same table with an integer \code{accepted_name_id} column added
#'   and the source-specific identifier dropped.
#' @keywords internal
#' @noRd
tnrs_link_accepted <- function(names) {
  accepted <- match(names$accepted_source_name_id, names$source_name_id)

  # A name that is itself accepted points at itself
  is_accepted <- names$taxonomic_status == "Accepted"
  accepted[is_accepted & is.na(accepted)] <- which(is_accepted & is.na(accepted))

  names$accepted_name_id <- accepted
  names$accepted_source_name_id <- NULL
  names
}

#' Unpack an archived source into the cache
#'
#' @param source Source name.
#' @param member File within the archive to extract.
#' @param dir Cache directory.
#' @return Path to the extracted file.
#' @keywords internal
#' @noRd
tnrs_unpack_source <- function(source, member, dir = tnrs_cache_dir()) {
  provenance_file <- tnrs_provenance_path(source, dir)
  if (!file.exists(provenance_file)) {
    stop(
      "Source '", source, "' has not been downloaded. ",
      "Run TNRS_local_build() first.",
      call. = FALSE
    )
  }

  provenance <- readRDS(provenance_file)
  target <- file.path(dir, member)

  if (!file.exists(target)) {
    utils::unzip(provenance$archive, files = member, exdir = dir)
  }
  if (!file.exists(target)) {
    stop("Could not extract '", member, "' from ", provenance$archive, call. = FALSE)
  }

  target
}
