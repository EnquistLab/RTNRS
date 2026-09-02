#' Darwin Core terms the Catalogue of Life core is read from
#' @keywords internal
#' @noRd
tnrs_col_terms <- function() {
  c(
    "taxonID", "scientificName", "scientificNameAuthorship", "taxonRank",
    "taxonomicStatus", "family", "genericName", "genus", "specificEpithet",
    "infraspecificEpithet", "acceptedNameUsageID", "kingdom",
    "nomenclaturalCode"
  )
}

#' Strip the namespace from a Darwin Core column name
#'
#' Internal.  A Darwin Core Archive may write its header as bare terms
#' (\code{taxonID}), as prefixed terms (\code{dwc:taxonID}), or as the full term
#' URI.  All three mean the same column, so the local name is what is matched
#' on.
#' @keywords internal
#' @noRd
tnrs_dwc_term <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub('"', "", x, fixed = TRUE)
  # Everything up to the last slash, colon or hash is the namespace
  sub("^.*[/:#]", "", x)
}

#' Read the Catalogue of Life core into the local name table
#'
#' Internal.  The Catalogue of Life is published as a Darwin Core Archive whose
#' core, \code{Taxon.tsv}, is a tab separated file of every accepted name and
#' synonym it holds.  Only the columns the matcher uses are read, because the
#' file carries several million rows.
#'
#' The header is matched on the local part of each Darwin Core term, so an
#' archive that writes prefixed or fully qualified terms reads the same as one
#' that writes bare ones.
#'
#' @param path Path to the extracted core file.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_col <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    stop("Catalogue of Life core file not found: ", path, call. = FALSE)
  }

  header <- readLines(path, n = 1L, warn = FALSE)
  if (length(header) == 0) {
    stop("Catalogue of Life core file is empty: ", path, call. = FALSE)
  }
  found <- tnrs_dwc_term(strsplit(header, "\t", fixed = TRUE)[[1]])

  wanted <- tnrs_col_terms()

  # Only the terms without which the file cannot be read at all.  An author, a
  # family or a stated nomenclatural code may all be absent from a valid
  # archive, and are handled as missing rather than refused.
  required <- c(
    "taxonID", "scientificName", "taxonRank", "taxonomicStatus",
    "acceptedNameUsageID"
  )
  absent <- setdiff(required, found)

  # The generic part of a name may be given as either term, and the importer
  # falls back from one to the other; without both, every name would import
  # with no genus and so never be found
  if (!any(c("genericName", "genus") %in% found)) {
    absent <- c(absent, "genericName or genus")
  }

  if (length(absent) > 0) {
    stop(
      "The Catalogue of Life core is missing the column(s) ",
      paste(absent, collapse = ", "),
      ". Its header reads: ", paste(utils::head(found, 15), collapse = ", "),
      if (length(found) > 15) ", ..." else "",
      ".\nThe published format may have changed.",
      call. = FALSE
    )
  }

  classes <- rep("NULL", length(found))
  index <- match(intersect(wanted, found), found)
  classes[index] <- "character"

  if (!quiet) message("Reading Catalogue of Life core ...")

  raw <- utils::read.table(
    path,
    sep = "\t", quote = "", header = TRUE, comment.char = "",
    colClasses = classes, na.strings = character(0),
    stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"
  )
  names(raw) <- tnrs_dwc_term(names(raw))

  for (column in names(raw)) {
    raw[[column]] <- iconv(raw[[column]], "UTF-8", "UTF-8", sub = "")
  }

  column <- function(field) {
    if (!field %in% names(raw)) {
      return(rep("", nrow(raw)))
    }
    value <- as.character(raw[[field]])
    value[is.na(value)] <- ""
    value
  }

  keep <- nzchar(column("scientificName"))
  raw <- raw[keep, , drop = FALSE]

  rank <- tolower(column("taxonRank"))

  # The Catalogue of Life writes the author into scientificName as well as
  # giving it separately, so 98% of its names end with their own authority.
  # WFO and WCVP store the bare name, and the local table follows them: it is
  # what Name_matched and Accepted_name report, and what the exact-match index
  # is keyed on, so leaving the author in place would both make this source
  # read differently from the others and stop a submitted bare name ever
  # matching exactly.
  full_name <- column("scientificName")
  author <- column("scientificNameAuthorship")
  suffixed <- nzchar(author) & endsWith(full_name, author)
  canonical <- full_name
  canonical[suffixed] <- trimws(substr(
    canonical[suffixed], 1L, nchar(canonical[suffixed]) - nchar(author[suffixed])
  ))

  # genericName is the generic part of the name on the row; genus is the genus
  # of the accepted taxon it belongs to, and is empty for a synonym.  Reading
  # genus would leave every synonym without the genus it is spelled with, and
  # so unfindable, which is silent rather than an error.
  generic <- column("genericName")
  classification_genus <- column("genus")
  genus <- ifelse(nzchar(generic), generic, classification_genus)

  # The archive states which code each name is governed by, which is better
  # evidence than its kingdom.  A zoological name carries no connector before
  # its subspecific epithet, so it is given no indicator.
  code <- toupper(column("nomenclaturalCode"))
  zoological <- if (any(nzchar(code))) {
    code == "ICZN"
  } else {
    tolower(column("kingdom")) == "animalia"
  }

  out <- data.frame(
    name_id = seq_len(nrow(raw)),
    source = "col",
    source_name_id = column("taxonID"),
    scientific_name = canonical,
    authorship = author,
    name_rank = rank,
    taxonomic_status = tnrs_standardize_status(column("taxonomicStatus")),
    family = column("family"),
    genus = genus,
    specific_epithet = column("specificEpithet"),
    # An animal name carries no connector before its subspecific epithet, so
    # the indicator is left out for names governed by the zoological code
    rank_indicator = ifelse(zoological, "", tnrs_wfo_rank_indicator(rank)),
    infraspecific_epithet = column("infraspecificEpithet"),
    is_hybrid = grepl(intToUtf8(0x00D7), canonical, fixed = TRUE),
    url = "",
    accepted_source_name_id = column("acceptedNameUsageID"),
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }
  out
}
