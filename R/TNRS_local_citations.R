#' Citation for the TNRS method itself
#' @keywords internal
#' @noRd
tnrs_method_citation <- function() {
  paste(
    "Boyle B., Hopkins N., Lu Z., Raygoza Garay J. A., Mozzherin D., Rees T.,",
    "Matasci N., Narro M. L., Piel W. H., McKay S. J., Lowry S., Freeland C.,",
    "Peet R. K. & Enquist B. J. (2013). The taxonomic name resolution service:",
    "an online tool for automated standardization of plant names.",
    "BMC Bioinformatics 14: 16. https://doi.org/10.1186/1471-2105-14-16"
  )
}

#' Citation for this package
#'
#' Internal.  Uses the package's own citation where it is installed, and falls
#' back to building one from the description, so this works under
#' \code{devtools::load_all()} and in a checkout as well as from a library.
#' @keywords internal
#' @noRd
tnrs_package_citation <- function() {
  # Warnings as well as errors: outside an installed library this reports that
  # the package cannot be found, which is expected and not worth showing
  built <- suppressWarnings(tryCatch(
    {
      cit <- utils::citation("TNRS")
      trimws(paste(format(cit[1], style = "text"), collapse = " "))
    },
    error = function(e) NULL
  ))
  if (!is.null(built) && nzchar(built)) {
    # citation() returns the CITATION file's entries where the package is
    # installed, so this picks up the DOI rather than a generated stand-in
    return(gsub("[[:space:]]+", " ", built))
  }

  version <- suppressWarnings(tryCatch(
    as.character(utils::packageVersion("TNRS")),
    error = function(e) "development version"
  ))
  paste0(
    "Maitner B. (", format(Sys.Date(), "%Y"), "). TNRS: Taxonomic Name ",
    "Resolution Service. R package version ", version,
    ". https://github.com/EnquistLab/RTNRS"
  )
}

#' Citations for a local name resolution
#'
#' Assembles everything a result resolved offline should be cited with: the
#' method, this package, and each taxonomic source the names were resolved
#' against, with the version actually used.
#'
#' A local result is only reproducible if the version of each source is
#' reported with it, which is why the version here comes from what was built
#' rather than from what is current.  \code{TNRS_local_status()} shows the same
#' versions.
#'
#' @param sources Character vector of sources to cite. Defaults to NULL, which
#'   cites every source that has been built. Pass the same sources you gave
#'   \code{TNRS_local()} to cite exactly what you used.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param bibtex_file Optional path. If given, the citations are also written
#'   there as BibTeX.
#' @param quiet Suppress the printed citations?
#' @return A data.frame, invisibly, with one row per work to cite:
#'   \code{what} ("method", "software" or "source"), \code{name},
#'   \code{version}, \code{doi} and \code{citation}.
#' @note The sources carry their own licences, reported by
#'   \code{TNRS_local_status()}. Citing them is a condition of some of those
#'   licences and good practice under all of them.
#' @seealso \code{\link{TNRS_local_status}} for the versions,
#'   \code{\link{TNRS_citations}} for the web service's own citation list.
#' @export
#' @examples \dontrun{
#' # Everything currently built
#' TNRS_local_citations()
#'
#' # Just what a particular analysis used
#' TNRS_local_citations(sources = c("wfo", "wcvp"))
#'
#' # Written out for a manuscript
#' TNRS_local_citations(bibtex_file = "taxonomy.bib")
#' }
TNRS_local_citations <- function(sources = NULL,
                                 dir = tnrs_cache_dir(),
                                 bibtex_file = NULL,
                                 quiet = FALSE) {
  registry <- tnrs_source_registry(dir)

  if (is.null(sources)) {
    built <- vapply(
      names(registry),
      function(s) file.exists(tnrs_names_path(s, dir)), logical(1)
    )
    sources <- names(registry)[built]
  }

  unknown <- setdiff(sources, names(registry))
  if (length(unknown) > 0) {
    stop(
      "Unknown source(s): ", paste(unknown, collapse = ", "),
      ". Available: ", paste(names(registry), collapse = ", "), ".",
      call. = FALSE
    )
  }

  out <- data.frame(
    what = c("method", "software"),
    name = c("Taxonomic Name Resolution Service", "TNRS R package"),
    version = c(NA_character_, NA_character_),
    doi = c("10.1186/1471-2105-14-16", NA_character_),
    citation = c(tnrs_method_citation(), tnrs_package_citation()),
    stringsAsFactors = FALSE
  )

  for (source in sources) {
    spec <- registry[[source]]

    # The version that was built, not the one the registry now points at: a
    # cache built against an earlier release must be cited as that release
    record <- tnrs_provenance_path(source, dir)
    version <- if (file.exists(record)) {
      readRDS(record)$version
    } else {
      spec$version
    }

    citation <- spec$citation
    if (is.null(citation) || is.na(citation) || !nzchar(citation)) {
      # A user-supplied source may have been registered without one
      citation <- paste0(
        spec$full_name, if (nzchar(version %||% "")) paste0(", version ", version) else "",
        if (!is.null(spec$publisher) && !is.na(spec$publisher)) {
          paste0(". ", spec$publisher)
        } else {
          ""
        }, "."
      )
    }

    out <- rbind(out, data.frame(
      what = "source", name = spec$full_name,
      version = as.character(version %||% NA_character_),
      doi = as.character(spec$doi %||% NA_character_),
      citation = citation, stringsAsFactors = FALSE
    ))
  }

  if (!is.null(bibtex_file)) {
    writeLines(tnrs_as_bibtex(out), bibtex_file)
    if (!quiet) message("Wrote ", nrow(out), " citations to ", bibtex_file)
  }

  if (!quiet) {
    message("Cite the method, this package, and every source you resolved against:\n")
    for (i in seq_len(nrow(out))) {
      message("  [", out$what[i], "] ", out$citation[i], "\n")
    }
  }

  invisible(out)
}

#' Render citations as BibTeX
#'
#' Internal.  Keys are built from the source name and version so that two
#' versions of the same source do not collide in one bibliography.
#' @keywords internal
#' @noRd
tnrs_as_bibtex <- function(citations) {
  keys <- gsub("[^A-Za-z0-9]", "", citations$name)
  keys <- ifelse(
    is.na(citations$version) | !nzchar(citations$version), keys,
    paste0(keys, gsub("[^A-Za-z0-9]", "", citations$version))
  )

  entries <- character(nrow(citations))
  for (i in seq_len(nrow(citations))) {
    fields <- c(
      paste0("  title = {", citations$name[i], "},"),
      paste0("  note = {", citations$citation[i], "},")
    )
    if (!is.na(citations$version[i]) && nzchar(citations$version[i])) {
      fields <- c(fields, paste0("  version = {", citations$version[i], "},"))
    }
    if (!is.na(citations$doi[i]) && nzchar(citations$doi[i])) {
      fields <- c(fields, paste0("  doi = {", citations$doi[i], "},"))
    }
    fields[length(fields)] <- sub(",$", "", fields[length(fields)])
    entries[i] <- paste(
      c(paste0("@misc{", keys[i], ","), fields, "}"),
      collapse = "\n"
    )
  }
  entries
}
