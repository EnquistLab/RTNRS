#' Directory holding the local taxonomic backbone
#'
#' The backbone is downloaded on demand rather than shipped with the package,
#' and is kept in the standard user cache directory so that it survives package
#' updates and can be removed with \code{TNRS_local_remove()}.
#'
#' @param create Should the directory be created if it does not exist?
#' @return Path to the cache directory.
#' @keywords internal
#' @noRd
tnrs_cache_dir <- function(create = FALSE) {
  dir <- getOption("TNRS.cache_dir", tools::R_user_dir("TNRS", which = "cache"))
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' Taxonomic sources available for a local build
#'
#' Internal.  Each entry records where the data comes from and how to cite it.
#' Versioned, citable artefacts are preferred over rolling "latest" downloads:
#' WFO publishes a DOI per release on Zenodo, and Kew archives each WCVP version
#' under its own filename, so a build can always be reproduced.
#'
#' @param dir Cache directory to look in for user-supplied sources.
#' @return A named list of source definitions.
#' @keywords internal
#' @noRd
tnrs_source_registry <- function(dir = tnrs_cache_dir()) {
  c(tnrs_builtin_registry(), tnrs_custom_sources(dir))
}

#' Taxonomic sources this package can download
#'
#' Internal.  The sources the package ships knowledge of, as opposed to those a
#' user has registered with \code{TNRS_local_add_source()}.
#' @return A named list of source definitions.
#' @keywords internal
#' @noRd
tnrs_builtin_registry <- function() {
  list(
    wcvp = list(
      source = "wcvp",
      full_name = "World Checklist of Vascular Plants",
      version = "v15",
      doi = NA_character_,
      url = "https://sftp.kew.org/pub/data-repositories/WCVP/Archive/wcvp_v15.zip",
      files = list(
        names = list(
          url = "https://sftp.kew.org/pub/data-repositories/WCVP/Archive/wcvp_v15.zip",
          member = "wcvp_names.csv"
        )
      ),
      taxonomic_scope = "Tracheophyta",
      license = "CC BY 4.0",
      publisher = "Royal Botanic Gardens, Kew",
      citation = paste(
        "Govaerts R., Nic Lughadha E., Black N., Turner R. & Paton A. (2021).",
        "The World Checklist of Vascular Plants, a continuously updated",
        "resource for exploring global plant diversity. Scientific Data 8: 215.",
        "https://doi.org/10.1038/s41597-021-00997-6"
      ),
      nomenclature = "botanical",
      # Approximate, for the message shown before a download is started.
      # disk_mb is the name table plus the match index, which is what remains
      # once the archive has been deleted; keep_archive = TRUE adds download_mb
      # back on top.
      download_mb = 85,
      disk_mb = 107
    ),
    wfo = list(
      source = "wfo",
      full_name = "World Flora Online",
      version = "2025-12",
      doi = "10.5281/zenodo.18007552",
      # Concept DOI, always resolving to the newest release:
      # 10.5281/zenodo.7460141
      url = paste0(
        "https://zenodo.org/api/records/18007552/files/",
        "_DwC_backbone_R.zip/content"
      ),
      files = list(
        names = list(
          url = paste0(
            "https://zenodo.org/api/records/18007552/files/",
            "_DwC_backbone_R.zip/content"
          ),
          member = "classification.csv"
        )
      ),
      taxonomic_scope = "Embryophyta",
      license = "CC0 1.0",
      publisher = "World Flora Online Consortium",
      # The form WFO asks to be cited in
      citation = paste(
        "WFO (2025): World Flora Online. Version 2025.12.",
        "Published on the Internet; http://www.worldfloraonline.org"
      ),
      nomenclature = "botanical",
      download_mb = 116,
      disk_mb = 120
    ),
    mdd = list(
      source = "mdd",
      full_name = "Mammal Diversity Database",
      version = "v2.3",
      doi = "10.5281/zenodo.17033774",
      # Concept DOI, always resolving to the newest release:
      # 10.5281/zenodo.4139722
      url = "https://zenodo.org/records/17033774",
      # Two files: the accepted species, and every name ever applied to them.
      # The second is what makes synonyms resolvable, so both are needed.
      files = list(
        species = list(
          url = paste0(
            "https://zenodo.org/api/records/17033774/files/",
            "MDD_v2.3_6836species.csv/content"
          )
        ),
        synonyms = list(
          url = paste0(
            "https://zenodo.org/api/records/17033774/files/",
            "Species_Syn_v2.3.csv/content"
          )
        )
      ),
      taxonomic_scope = "Mammalia",
      license = "CC BY 4.0",
      publisher = "American Society of Mammalogists",
      citation = paste(
        "Mammal Diversity Database (2025). Mammal Diversity Database",
        "(Version 2.3) [Data set]. Zenodo.",
        "https://doi.org/10.5281/zenodo.17033774"
      ),
      nomenclature = "zoological",
      download_mb = 68,
      disk_mb = 30
    ),
    col = list(
      source = "col",
      full_name = "Catalogue of Life",
      version = "COL26.8",
      doi = "10.48580/dgywk",
      url = "https://www.catalogueoflife.org/",
      # The dated release, not the rolling latest_dwca.zip, which says nothing
      # about which version it holds.  ChecklistBank keeps only a short window
      # of these, so this URL will stop resolving once the release rotates out;
      # the DOI above is the durable reference.  Note the export endpoint on
      # api.checklistbank.org serves JSON whatever format is asked for, so it
      # is not the download it appears to be.
      files = list(
        names = list(
          url = "https://download.checklistbank.org/col/monthly/2026-08-20_dwca.zip",
          member = "Taxon.tsv"
        )
      ),
      taxonomic_scope = "all life",
      license = "CC BY 4.0",
      publisher = "Catalogue of Life",
      citation = paste(
        "Banki O., Roskov Y., Doring M., Ower G., et al. (2026).",
        "Catalogue of Life Checklist (Version 2026-08-20).",
        "https://doi.org/10.48580/dgywk"
      ),
      # Covers plants and animals, so both codes apply
      nomenclature = "mixed",
      # Measured: 488 MB down, 135 MB name table and 257 MB match index.
      # Building it takes about 9 minutes and the index another 12, at a peak
      # of under 3 GB.
      download_mb = 488,
      disk_mb = 392
    )
  )
}

#' Files a source occupies in the cache
#'
#' Internal.  Everything a source writes is prefixed with its name, so this is
#' what it costs on disk: the name table, the match index, the provenance
#' record, and the archive if it was kept.
#' @keywords internal
#' @noRd
tnrs_source_files <- function(source, dir = tnrs_cache_dir()) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  list.files(dir, pattern = paste0("^", source, "-"), full.names = TRUE)
}

#' Path to the provenance record for a cached source
#' @keywords internal
#' @noRd
tnrs_provenance_path <- function(source, dir = tnrs_cache_dir()) {
  file.path(dir, paste0(source, "-provenance.rds"))
}

#' Download one taxonomic source into the cache
#'
#' Internal.  Records where the file came from, which version it is, when it was
#' fetched and its checksum, so that a local result can be cited as precisely as
#' an API result.
#'
#' @param source Name of a source in \code{tnrs_source_registry()}.
#' @param dir Cache directory.
#' @param overwrite Re-download even if the archive is already present?
#' @param quiet Suppress the download progress bar?
#' @return The provenance record, invisibly.
#' @keywords internal
#' @noRd
tnrs_download_source <- function(source, dir = tnrs_cache_dir(create = TRUE),
                                 overwrite = FALSE, quiet = FALSE) {
  registry <- tnrs_builtin_registry()
  if (!source %in% names(registry)) {
    if (isTRUE(unname(tnrs_is_custom(source, dir)))) {
      stop(
        "Source '", source, "' was supplied by you, so there is nothing to ",
        "download. Register it again with TNRS_local_add_source() if its ",
        "data is missing.",
        call. = FALSE
      )
    }
    stop("Unknown source '", source, "'. Available: ",
      paste(names(registry), collapse = ", "),
      call. = FALSE
    )
  }

  spec <- registry[[source]]
  files <- tnrs_source_file_spec(spec)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  archive <- character(0)
  for (part in names(files)) {
    target <- tnrs_archive_path(source, spec$version, part, files, dir)

    if (file.exists(target) && !overwrite) {
      if (!quiet) {
        message("Using cached download for ", source, " ", spec$version)
      }
    } else {
      if (!quiet) {
        message(
          "Downloading ", spec$full_name, " ", spec$version,
          if (length(files) > 1) paste0(" (", part, ")") else "", " ..."
        )
      }

      # Download to a temporary file first so that an interrupted download can
      # never be mistaken for a complete one
      partial <- paste0(target, ".part")
      status <- utils::download.file(
        url = files[[part]]$url, destfile = partial, mode = "wb",
        quiet = quiet, cacheOK = FALSE
      )
      if (status != 0 || !file.exists(partial)) {
        unlink(partial)
        stop("Download failed for source '", source, "'.", call. = FALSE)
      }
      file.rename(partial, target)
    }
    archive[part] <- target
  }

  provenance <- list(
    source = spec$source,
    full_name = spec$full_name,
    version = spec$version,
    doi = spec$doi,
    url = spec$url,
    taxonomic_scope = spec$taxonomic_scope,
    license = spec$license,
    publisher = spec$publisher,
    archive = unname(archive),
    archive_kept = TRUE,
    # Recorded before the archive is possibly deleted, so that what was
    # downloaded stays documented whether or not the file is still there
    bytes = sum(as.numeric(file.size(archive))),
    md5 = paste(unname(tools::md5sum(archive)), collapse = ", "),
    downloaded = as.character(Sys.Date())
  )

  saveRDS(provenance, tnrs_provenance_path(source, dir))
  invisible(provenance)
}

#' Files a source declares, as a named list of download specs
#'
#' Internal.  Most sources are a single download; a source such as the Mammal
#' Diversity Database publishes its accepted names and its synonyms separately,
#' and needs both.
#' @keywords internal
#' @noRd
tnrs_source_file_spec <- function(spec) {
  if (!is.null(spec$files)) {
    return(spec$files)
  }
  list(names = list(url = spec$url, member = NULL))
}

#' Where a downloaded file for a source is kept
#'
#' Internal.  A single-file source keeps the name it always had, so that a
#' cache built by an earlier version of this package is still recognised and
#' not downloaded again.  Only a multi-file source needs the part in its name.
#' @keywords internal
#' @noRd
tnrs_archive_path <- function(source, version, part, files, dir) {
  url <- files[[part]]$url
  # A query string is not part of the file name, and an export endpoint has no
  # extension to read, so an archive is assumed where a member is declared
  ext <- tools::file_ext(sub("[?].*$", "", url))
  if (!nzchar(ext) || nchar(ext) > 4L) {
    ext <- if (is.null(files[[part]]$member)) "csv" else "zip"
  }
  stem <- if (length(files) > 1L) {
    paste0(source, "-", version, "-", part)
  } else {
    paste0(source, "-", version)
  }
  file.path(dir, paste0(stem, ".", ext))
}

#' Report on the locally cached taxonomic backbone
#'
#' Shows every taxonomic source the package can use, whether it has been built
#' for offline use, and for those that have, which version it is and how much
#' space it occupies.  A local result can be cited using the version and DOI
#' reported here.
#'
#' Sources that have not been built are listed too, with \code{built} FALSE and
#' \code{download_mb} giving what fetching them would cost, so that this is the
#' one place to look to answer both "what did I resolve against" and "what else
#' could I use".  Build them with \code{TNRS_local_build()}.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @return A data.frame with one row per available source.  \code{version},
#'   \code{doi} and \code{downloaded} describe what is installed and are NA for
#'   a source that has not been built.  \code{size_mb} is what the source
#'   occupies on disk now, which is larger for a source built with
#'   \code{keep_archive = TRUE}; \code{download_mb} is what fetching it costs.
#'   \code{nomenclature} is the code the source follows, which decides how
#'   \code{TNRS_local()} reads a family prefix.
#' @seealso \code{\link{TNRS_local_build}},
#'   \code{\link{TNRS_local_add_source}}
#' @export
#' @examples {
#'   status <- TNRS_local_status()
#' }
TNRS_local_status <- function(dir = tnrs_cache_dir()) {
  registry <- tnrs_source_registry(dir)
  sources <- names(registry)

  # A provenance file records a completed download, which is not the same as a
  # finished build: an interrupted run can leave the archive without the name
  # table.  The name table is what the matcher needs, so that is what "built"
  # means here.
  built <- vapply(
    sources, function(s) file.exists(tnrs_names_path(s, dir)), logical(1)
  )

  provenance <- lapply(sources, function(s) {
    path <- tnrs_provenance_path(s, dir)
    if (file.exists(path)) readRDS(path) else NULL
  })
  names(provenance) <- sources

  # Installed detail, NA wherever a source has not been built, so that nothing
  # in these columns can be mistaken for something you could cite
  from_record <- function(field, empty) {
    vapply(sources, function(s) {
      record <- provenance[[s]]
      if (!built[[s]] || is.null(record)) {
        return(empty)
      }
      value <- record[[field]]
      if (is.null(value)) empty else value
    }, empty)
  }

  # Measured from the files that are actually there rather than taken from the
  # provenance record, which describes the download.  The two differ whenever
  # the archive has been deleted, which is the default after a build.
  size_mb <- vapply(
    sources,
    function(s) round(sum(file.size(tnrs_source_files(s, dir))) / 1024^2, 1),
    numeric(1)
  )

  out <- data.frame(
    source = sources,
    full_name = vapply(registry, function(x) x$full_name, character(1)),
    built = unname(built),
    version = unname(from_record("version", NA_character_)),
    doi = unname(from_record("doi", NA_character_)),
    downloaded = unname(from_record("downloaded", NA_character_)),
    nomenclature = unname(vapply(
      registry, function(x) as.character(x$nomenclature %||% "botanical"),
      character(1)
    )),
    size_mb = unname(size_mb),
    download_mb = vapply(registry, function(x) as.numeric(x$download_mb), numeric(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out <- out[order(out$source), ]

  absent <- out$source[!out$built]
  if (length(absent) == length(sources)) {
    # The bare call builds the default source, which is the right advice here;
    # naming one would mean picking it out of the registry order
    message(
      "No local backbone built yet in:\n  ", dir,
      "\nRun TNRS_local_build() to set one up."
    )
  } else if (length(absent) > 0) {
    message(
      "Not built: ", paste(absent, collapse = ", "),
      ". Add with TNRS_local_build(", tnrs_source_arg(absent), ")."
    )
  }

  out
}

#' Delete the locally cached taxonomic backbone
#'
#' Removes the downloaded taxonomic sources and everything derived from them.
#' The data can be downloaded again at any time.
#'
#' Checklists registered with \code{TNRS_local_add_source()} go too, and those
#' this package cannot fetch again, so it names them before asking.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @param ask Ask for confirmation before deleting? Defaults to TRUE in an
#'   interactive session.
#' @return TRUE if anything was removed, FALSE otherwise, invisibly.
#' @seealso \code{\link{TNRS_local_add_source}}
#' @export
#' @examples \dontrun{
#' TNRS_local_remove()
#' }
TNRS_local_remove <- function(dir = tnrs_cache_dir(), ask = interactive()) {
  if (!dir.exists(dir)) {
    message("Nothing to remove; no cache directory at:\n  ", dir)
    return(invisible(FALSE))
  }

  size_mb <- round(sum(file.size(list.files(dir, recursive = TRUE, full.names = TRUE)),
    na.rm = TRUE
  ) / 1024^2, 1)

  # A downloaded source can always be fetched again; one the user supplied
  # cannot, so it is called out rather than quietly included in the total
  custom <- names(tnrs_custom_sources(dir))
  warn <- if (length(custom) > 0) {
    paste0(
      "\nThis includes ", paste(custom, collapse = ", "),
      ", which you supplied and this package cannot download again."
    )
  } else {
    ""
  }

  if (ask) {
    answer <- readline(paste0(
      "Delete the local TNRS backbone (", size_mb, " MB) in\n  ", dir,
      warn, "\n? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing removed.")
      return(invisible(FALSE))
    }
  } else if (nzchar(warn)) {
    message(sub("^\n", "", warn))
  }

  unlink(dir, recursive = TRUE)
  message("Removed ", size_mb, " MB from ", dir)
  invisible(TRUE)
}

#' Default value for NULL
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
