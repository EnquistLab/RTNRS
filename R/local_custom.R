#' Darwin Core names for the fields a checklist can supply
#'
#' Internal.  These are the defaults \code{TNRS_local_add_source()} looks for,
#' chosen because Darwin Core is what checklists are normally published in: WFO,
#' GBIF and Catalogue of Life all ship these column names.  A checklist that
#' uses different names is handled by the \code{columns} argument rather than by
#' being reshaped first.
#'
#' The names of this vector are the canonical fields; the values are the columns
#' they are read from.
#' @keywords internal
#' @noRd
tnrs_dwc_columns <- function() {
  c(
    source_name_id = "taxonID",
    scientific_name = "scientificName",
    authorship = "scientificNameAuthorship",
    name_rank = "taxonRank",
    taxonomic_status = "taxonomicStatus",
    family = "family",
    genus = "genus",
    specific_epithet = "specificEpithet",
    infraspecific_epithet = "infraspecificEpithet",
    accepted_source_name_id = "acceptedNameUsageID",
    url = "references"
  )
}

#' Put a taxonomic status into the vocabulary the resolver scores on
#'
#' Internal.  Candidate ranking gives accepted names precedence over synonyms,
#' and does so by looking the status up in a fixed table.  A checklist spelling
#' its statuses any other way would score every row as though its status were
#' unknown, which is a silent ranking bug rather than a visible error, so the
#' common spellings are folded onto the vocabulary here.
#'
#' Darwin Core distinguishes homotypic from heterotypic synonyms; the TNRS does
#' not, and treats both simply as synonyms.
#'
#' @param x Character vector of statuses.
#' @return The same length, using the resolver's vocabulary.
#' @keywords internal
#' @noRd
tnrs_standardize_status <- function(x) {
  x <- as.character(x)
  key <- tolower(trimws(x))
  key <- gsub("[^a-z]", "", key)

  known <- c(
    accepted = "Accepted", valid = "Accepted",
    provisionallyaccepted = "Accepted", acceptedname = "Accepted",
    synonym = "Synonym", homotypicsynonym = "Synonym",
    heterotypicsynonym = "Synonym", propartesynonym = "Synonym",
    ambiguoussynonym = "Synonym", basionym = "Synonym",
    illegitimate = "Illegitimate", illegitimatename = "Illegitimate",
    invalid = "Invalid", invalidname = "Invalid",
    unchecked = "Unchecked", doubtful = "Unchecked",
    unplaced = "Unplaced", misapplied = "Unplaced", misappliedname = "Unplaced"
  )

  out <- unname(known[key])

  # Anything unrecognised keeps its own spelling, capitalised, rather than
  # being forced into a bucket it may not belong in
  unmatched <- is.na(out)
  keep <- trimws(x[unmatched])
  substr(keep, 1, 1) <- toupper(substr(keep, 1, 1))
  out[unmatched] <- keep
  out[is.na(out)] <- ""
  out
}

#' Spell a rank indicator out as a rank name
#'
#' Internal.  The inverse of the abbreviation the parser produces, for the few
#' ranks that actually occur below species.  \code{name_rank} is reported as
#' \code{Name_matched_rank}, so a source whose ranks were derived from its names
#' should read the way WFO and WCVP read rather than showing "var.".
#' @keywords internal
#' @noRd
tnrs_rank_word <- function(indicator) {
  known <- c(
    "var." = "variety", "subsp." = "subspecies", "fo." = "form",
    "subvar." = "subvariety", "subfo." = "subform", "cv." = "cultivar",
    "sect." = "section", "subsect." = "subsection", "ser." = "series",
    "subser." = "subseries", "subgen." = "subgenus",
    "nothosubsp." = "nothosubspecies", "nothovar." = "nothovariety"
  )
  out <- unname(known[indicator])
  # Anything rarer keeps its own spelling, minus the abbreviating point
  fallback <- sub("[.]$", "", indicator)
  out[is.na(out)] <- fallback[is.na(out)]
  out[!nzchar(indicator)] <- ""
  out
}

#' Guess the delimiter of a text file from its first line
#'
#' Internal.  Checked rather than assumed from the extension, because ".csv" is
#' routinely used for tab and pipe delimited files, WCVP's own names file among
#' them.
#' @keywords internal
#' @noRd
tnrs_guess_delim <- function(path) {
  header <- readLines(path, n = 1L, warn = FALSE)
  if (length(header) == 0) {
    stop("Checklist file is empty: ", path, call. = FALSE)
  }

  candidates <- c("\t", "|", ",", ";")
  counts <- vapply(
    candidates,
    function(d) length(gregexpr(d, header, fixed = TRUE)[[1]][
      gregexpr(d, header, fixed = TRUE)[[1]] > 0
    ]),
    integer(1)
  )

  if (max(counts) == 0) {
    stop(
      "Could not find a delimiter in the header of ", path,
      ". Pass delim explicitly.",
      call. = FALSE
    )
  }
  candidates[which.max(counts)]
}

#' Read a checklist from a delimited file
#'
#' @param path File to read.
#' @param delim Field separator, guessed from the header when NULL.
#' @param quiet Suppress progress messages?
#' @return A data.frame of character columns.
#' @keywords internal
#' @noRd
tnrs_read_checklist <- function(path, delim = NULL, quiet = FALSE) {
  if (!file.exists(path)) {
    stop("Checklist file not found: ", path, call. = FALSE)
  }
  if (is.null(delim)) {
    delim <- tnrs_guess_delim(path)
  }

  if (!quiet) message("Reading checklist ...")

  out <- utils::read.table(
    path,
    sep = delim, quote = "\"", header = TRUE, comment.char = "",
    colClasses = "character", na.strings = character(0),
    stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"
  )

  # Sources are not always clean UTF-8; dropping the bad bytes keeps the rows
  for (column in names(out)) {
    out[[column]] <- iconv(out[[column]], "UTF-8", "UTF-8", sub = "")
  }
  out
}

#' Turn an arbitrary checklist into the local name table
#'
#' Internal.  Produces exactly what the built-in importers produce, so that a
#' user-supplied source is matched, scored and reported by the same code as
#' WFO and WCVP.
#'
#' Only the name itself is required.  Anything else the checklist does not
#' carry is derived from the name by the same parser the resolver uses, so a
#' bare list of names works without the author having to reshape it first.
#'
#' @param data A data.frame holding the checklist.
#' @param source Short code for the source.
#' @param columns Named character vector overriding the Darwin Core defaults.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the columns given by \code{tnrs_name_columns()}.
#' @keywords internal
#' @noRd
tnrs_import_checklist <- function(data, source, columns = NULL, quiet = FALSE) {
  if (!is.data.frame(data)) {
    stop("A checklist must be a data.frame.", call. = FALSE)
  }

  mapping <- tnrs_dwc_columns()
  if (!is.null(columns)) {
    if (is.null(names(columns)) || any(!nzchar(names(columns)))) {
      stop(
        "columns must be a named vector, for example ",
        'c(scientific_name = "name").',
        call. = FALSE
      )
    }
    unknown <- setdiff(names(columns), names(mapping))
    if (length(unknown) > 0) {
      stop(
        "Unknown field(s) in columns: ", paste(unknown, collapse = ", "),
        ". Fields are: ", paste(names(mapping), collapse = ", "), ".",
        call. = FALSE
      )
    }
    mapping[names(columns)] <- as.character(columns)
  }

  present <- vapply(mapping, function(x) x %in% names(data), logical(1))
  names(present) <- names(mapping)

  if (!present[["scientific_name"]]) {
    stop(
      "The checklist has no column '", mapping[["scientific_name"]],
      "' holding the name. Available columns: ",
      paste(names(data), collapse = ", "),
      ".\nName the right one with columns = c(scientific_name = \"...\").",
      call. = FALSE
    )
  }

  scientific_name <- trimws(as.character(data[[mapping[["scientific_name"]]]]))
  keep <- !is.na(scientific_name) & nzchar(scientific_name)
  data <- data[keep, , drop = FALSE]
  scientific_name <- scientific_name[keep]

  n <- length(scientific_name)
  if (n == 0) {
    stop("The checklist has no usable names.", call. = FALSE)
  }

  column <- function(field) {
    if (!present[[field]]) {
      return(NULL)
    }
    value <- as.character(data[[mapping[[field]]]])
    value[is.na(value)] <- ""
    trimws(value)
  }

  # The name is parsed only for the parts the checklist did not supply, since
  # parsing is much the most expensive step of an import
  derived <- c("genus", "specific_epithet", "infraspecific_epithet")
  parsed <- NULL
  if (!all(present[derived])) {
    if (!quiet) {
      message(
        "  deriving ", paste(derived[!present[derived]], collapse = ", "),
        " from the name ..."
      )
    }
    parsed <- tnrs_parse(scientific_name)
  }

  genus <- column("genus") %||% parsed$genus
  specific_epithet <- column("specific_epithet") %||% parsed$species
  infraspecific_epithet <- column("infraspecific_epithet") %||% parsed$infra1

  name_rank <- column("name_rank")
  if (is.null(name_rank)) {
    rank_indicator <- if (is.null(parsed)) rep("", n) else parsed$rank1
    # Reported to the user as Name_matched_rank, so it is spelled the way the
    # published sources spell it rather than left as the abbreviation
    name_rank <- ifelse(
      nzchar(rank_indicator), tnrs_rank_word(rank_indicator),
      ifelse(nzchar(infraspecific_epithet), "infraspecific",
        ifelse(nzchar(specific_epithet), "species",
          ifelse(nzchar(genus), "genus", "unranked")
        )
      )
    )
  } else {
    name_rank <- tolower(name_rank)
    rank_indicator <- tnrs_wfo_rank_indicator(name_rank)
  }

  status <- column("taxonomic_status")
  if (is.null(status)) {
    # A checklist with no status column is a list of names its author stands
    # behind; treating them as accepted is what makes such a list resolvable
    if (!quiet) {
      message("  no status column; treating every name as accepted")
    }
    status <- rep("Accepted", n)
  } else {
    status <- tnrs_standardize_status(status)
  }

  out <- data.frame(
    name_id = seq_len(n),
    source = source,
    source_name_id = column("source_name_id") %||% as.character(seq_len(n)),
    scientific_name = scientific_name,
    authorship = column("authorship") %||%
      (if (is.null(parsed)) rep("", n) else parsed$authorship),
    name_rank = name_rank,
    taxonomic_status = status,
    family = column("family") %||% rep("", n),
    genus = genus,
    specific_epithet = specific_epithet,
    rank_indicator = rank_indicator,
    infraspecific_epithet = infraspecific_epithet,
    # U+00D7 is the hybrid marker; built by code point to keep this file ASCII
    is_hybrid = grepl(intToUtf8(0x00D7), scientific_name, fixed = TRUE) |
      grepl("^x | x ", scientific_name),
    url = column("url") %||% rep("", n),
    accepted_source_name_id = column("accepted_source_name_id") %||% rep("", n),
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    message("  ", format(nrow(out), big.mark = ","), " names read")
  }
  out
}

#' Path to a user-supplied source's registry entry
#'
#' The presence of this file is what marks a source as user-supplied, which is
#' how \code{tnrs_source_registry()} finds sources this package does not ship.
#' @keywords internal
#' @noRd
tnrs_custom_spec_path <- function(source, dir = tnrs_cache_dir()) {
  file.path(dir, paste0(source, "-source.rds"))
}

#' User-supplied sources registered in a cache directory
#' @keywords internal
#' @noRd
tnrs_custom_sources <- function(dir = tnrs_cache_dir()) {
  if (!dir.exists(dir)) {
    return(list())
  }
  files <- list.files(dir, pattern = "-source[.]rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(list())
  }

  specs <- lapply(files, function(f) tryCatch(readRDS(f), error = function(e) NULL))
  specs <- Filter(function(x) is.list(x) && !is.null(x$source), specs)
  if (length(specs) == 0) {
    return(list())
  }
  names(specs) <- vapply(specs, function(x) x$source, character(1))
  specs[order(names(specs))]
}

#' Is a source user-supplied rather than one this package ships?
#' @keywords internal
#' @noRd
tnrs_is_custom <- function(source, dir = tnrs_cache_dir()) {
  vapply(source, function(s) file.exists(tnrs_custom_spec_path(s, dir)), logical(1))
}

#' Add your own checklist as a local taxonomic source
#'
#' Registers a checklist you supply as a source that \code{TNRS_local()} can
#' resolve against, on the same footing as the sources this package downloads.
#' Use it for a taxonomic authority the TNRS does not distribute, for a group
#' outside the flowering plants, or for an in-house list.
#'
#' Only the name itself is required.  Anything the checklist does not carry,
#' such as the genus and epithet, is derived from the name using the same parser
#' the resolver uses, so a single column of names is enough to get started.
#' Columns are looked for under their Darwin Core names by default, since that
#' is how checklists are normally published; use \code{columns} for a list that
#' names them differently.
#'
#' A source added this way behaves like any other. It can be used on its own, or
#' alongside "wfo" and "wcvp", in which case \code{Source_conflict} marks the
#' names they disagree about, and the order of \code{sources} decides precedence.
#'
#' @param x A data.frame holding the checklist, or a path to a delimited file.
#' @param source Short code naming the source, for example "cact". Lower case,
#'   letters, digits and underscores. This is what you pass to
#'   \code{TNRS_local(sources = ...)}, and what appears in the \code{Source}
#'   column of the results.
#' @param version Version of the checklist, for example a release number or the
#'   date you obtained it. Required, because it is what makes a result you
#'   report reproducible: it is recorded and reported by
#'   \code{TNRS_local_status()}.
#' @param full_name Full name of the source, for display. Defaults to
#'   \code{source}.
#' @param columns Named character vector mapping fields onto the columns of your
#'   checklist, for a list that does not use Darwin Core names. For example
#'   \code{c(scientific_name = "name", authorship = "author")}. The fields are
#'   scientific_name, authorship, name_rank, taxonomic_status, family, genus,
#'   specific_epithet, infraspecific_epithet, source_name_id,
#'   accepted_source_name_id and url. Only scientific_name is required.
#' @param nomenclature Nomenclatural code the checklist follows: "botanical"
#'   (the default), "zoological", or "mixed" for one covering both. It decides
#'   how \code{TNRS_local()} reads a family prefix in a submitted name, since
#'   botanical families end in -aceae and zoological ones in -idae. Recorded
#'   with the source, so it is applied automatically whenever the source is
#'   used.
#' @param doi,url,publisher,license,taxonomic_scope,citation Optional metadata,
#'   recorded so that the source can be cited alongside your results.
#' @param delim Field separator, when \code{x} is a file. Guessed from the
#'   header when not given.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param overwrite Replace a source of this name that is already registered?
#' @param quiet Suppress progress messages?
#' @return The output of \code{TNRS_local_status()}, invisibly.
#' @note Statuses are read into the vocabulary the resolver scores on, so the
#'   Darwin Core spellings, homotypicSynonym and the rest, are understood. A
#'   checklist with no status column is treated as a list of accepted names.
#' @note \code{TNRS_local_remove()} deletes user-supplied sources along with
#'   everything else, and this package cannot fetch them again, so keep the
#'   original file.
#' @seealso \code{\link{TNRS_local}}, \code{\link{TNRS_local_status}},
#'   \code{\link{TNRS_local_citations}}
#' @export
#' @examples \dontrun{
#' # A checklist published in Darwin Core needs no mapping
#' TNRS_local_add_source("cactaceae.csv", source = "cact", version = "2021")
#'
#' # One that names its columns differently
#' TNRS_local_add_source(
#'   my_list,
#'   source = "inhouse", version = "2026-09",
#'   columns = c(scientific_name = "taxon", authorship = "author")
#' )
#'
#' # Then use it like any other source
#' TNRS_local(c("Mamillaria elongata"), sources = "cact")
#'
#' # Or alongside a global source, to see where they disagree
#' results <- TNRS_local(names, sources = c("cact", "wfo"))
#' results[results$Source_conflict, ]
#' }
TNRS_local_add_source <- function(x,
                                  source,
                                  version,
                                  full_name = source,
                                  columns = NULL,
                                  nomenclature = "botanical",
                                  doi = NA_character_,
                                  url = NA_character_,
                                  publisher = NA_character_,
                                  license = NA_character_,
                                  taxonomic_scope = NA_character_,
                                  citation = NA_character_,
                                  delim = NULL,
                                  dir = tnrs_cache_dir(create = TRUE),
                                  overwrite = FALSE,
                                  quiet = FALSE) {
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
    !grepl("^[a-z][a-z0-9_]*$", source)) {
    stop(
      "source should be a single short code in lower case, ",
      'for example "cact".',
      call. = FALSE
    )
  }
  if (source %in% names(tnrs_builtin_registry())) {
    stop(
      "'", source, "' is one of the sources this package downloads. ",
      "Choose another code.",
      call. = FALSE
    )
  }
  if (missing(version) || !is.character(version) || length(version) != 1L ||
    is.na(version) || !nzchar(version)) {
    stop(
      "version is required, so that results resolved against this source ",
      "can be reported reproducibly. Any string will do, such as a release ",
      "number or the date you obtained the file.",
      call. = FALSE
    )
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  names_file <- tnrs_names_path(source, dir)
  if (file.exists(names_file) && !overwrite) {
    message(
      "Source '", source, "' is already registered. ",
      "Pass overwrite = TRUE to replace it."
    )
    return(invisible(suppressMessages(TNRS_local_status(dir))))
  }

  input <- NA_character_
  md5 <- NA_character_
  bytes <- NA_real_

  if (is.character(x) && length(x) == 1L) {
    input <- normalizePath(x, mustWork = FALSE)
    checklist <- tnrs_read_checklist(x, delim = delim, quiet = quiet)
    md5 <- unname(tools::md5sum(x))
    bytes <- as.numeric(file.size(x))
  } else {
    checklist <- x
  }

  names <- tnrs_import_checklist(checklist, source, columns = columns, quiet = quiet)
  names <- tnrs_link_accepted(names)

  nanoparquet::write_parquet(names, names_file, compression = "gzip")

  # A stale index would describe the previous contents of this source
  unlink(tnrs_index_path(source, dir))
  tnrs_backbone_forget()

  spec <- list(
    source = source,
    full_name = full_name,
    version = version,
    nomenclature = nomenclature,
    doi = as.character(doi),
    url = as.character(url),
    taxonomic_scope = as.character(taxonomic_scope),
    license = as.character(license),
    publisher = as.character(publisher),
    citation = as.character(citation),
    custom = TRUE,
    # Nothing to fetch: the file came from the user, so there is no download
    # to price and nothing this package could re-download
    download_mb = 0,
    disk_mb = round(as.numeric(file.size(names_file)) / 1024^2, 1)
  )
  saveRDS(spec, tnrs_custom_spec_path(source, dir))

  provenance <- spec
  provenance$archive <- NA_character_
  provenance$archive_kept <- NA
  provenance$input <- input
  provenance$md5 <- md5
  provenance$bytes <- bytes
  provenance$names <- nrow(names)
  provenance$downloaded <- as.character(Sys.Date())
  saveRDS(provenance, tnrs_provenance_path(source, dir))

  if (!quiet) {
    message(
      "Registered '", source, "' (", format(nrow(names), big.mark = ","),
      " names). Use TNRS_local(sources = \"", source, "\")."
    )
  }

  invisible(suppressMessages(TNRS_local_status(dir)))
}
