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
#' @return A named list of source definitions.
#' @keywords internal
#' @noRd
tnrs_source_registry <- function() {
  list(
    wcvp = list(
      source = "wcvp",
      full_name = "World Checklist of Vascular Plants",
      version = "v15",
      doi = NA_character_,
      url = "https://sftp.kew.org/pub/data-repositories/WCVP/Archive/wcvp_v15.zip",
      taxonomic_scope = "Tracheophyta",
      license = "CC BY 4.0",
      publisher = "Royal Botanic Gardens, Kew",
      # Approximate, for the message shown before a download is started.
      # disk_mb covers the archive, which is kept so that a rebuild needs no
      # second download, plus the name table and the match index built from it.
      download_mb = 85,
      disk_mb = 192
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
      taxonomic_scope = "Embryophyta",
      license = "CC0 1.0",
      publisher = "World Flora Online Consortium",
      download_mb = 116,
      disk_mb = 235
    )
  )
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
  registry <- tnrs_source_registry()
  if (!source %in% names(registry)) {
    stop("Unknown source '", source, "'. Available: ",
      paste(names(registry), collapse = ", "),
      call. = FALSE
    )
  }

  spec <- registry[[source]]
  archive <- file.path(dir, paste0(source, "-", spec$version, ".zip"))

  if (file.exists(archive) && !overwrite) {
    if (!quiet) {
      message("Using cached archive for ", source, " ", spec$version)
    }
  } else {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!quiet) {
      message("Downloading ", spec$full_name, " ", spec$version, " ...")
    }

    # Download to a temporary file first so that an interrupted download can
    # never be mistaken for a complete one
    partial <- paste0(archive, ".part")
    status <- utils::download.file(
      url = spec$url, destfile = partial, mode = "wb",
      quiet = quiet, cacheOK = FALSE
    )
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for source '", source, "'.", call. = FALSE)
    }
    file.rename(partial, archive)
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
    archive = archive,
    bytes = as.numeric(file.size(archive)),
    md5 = unname(tools::md5sum(archive)),
    downloaded = as.character(Sys.Date())
  )

  saveRDS(provenance, tnrs_provenance_path(source, dir))
  invisible(provenance)
}

#' Report on the locally cached taxonomic backbone
#'
#' Shows which taxonomic sources have been downloaded for offline use, which
#' version each is, and how much space they occupy.  A local result can be cited
#' using the version and DOI reported here.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @return A data.frame with one row per cached source, invisibly if empty.
#' @export
#' @examples {
#'   status <- TNRS_local_status()
#' }
TNRS_local_status <- function(dir = tnrs_cache_dir()) {
  if (!dir.exists(dir)) {
    message(
      "No local backbone found. The cache directory does not exist yet:\n  ",
      dir
    )
    return(invisible(NULL))
  }

  files <- list.files(dir, pattern = "-provenance[.]rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("No local backbone found in:\n  ", dir)
    return(invisible(NULL))
  }

  records <- lapply(files, readRDS)

  out <- data.frame(
    source = vapply(records, function(x) x$source, character(1)),
    full_name = vapply(records, function(x) x$full_name, character(1)),
    version = vapply(records, function(x) x$version, character(1)),
    doi = vapply(records, function(x) as.character(x$doi %||% NA), character(1)),
    downloaded = vapply(records, function(x) x$downloaded, character(1)),
    size_mb = round(vapply(records, function(x) x$bytes, numeric(1)) / 1024^2, 1),
    stringsAsFactors = FALSE
  )

  out[order(out$source), ]
}

#' Delete the locally cached taxonomic backbone
#'
#' Removes the downloaded taxonomic sources and everything derived from them.
#' The data can be downloaded again at any time.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @param ask Ask for confirmation before deleting? Defaults to TRUE in an
#'   interactive session.
#' @return TRUE if anything was removed, FALSE otherwise, invisibly.
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

  if (ask) {
    answer <- readline(paste0(
      "Delete the local TNRS backbone (", size_mb, " MB) in\n  ", dir,
      "\n? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing removed.")
      return(invisible(FALSE))
    }
  }

  unlink(dir, recursive = TRUE)
  message("Removed ", size_mb, " MB from ", dir)
  invisible(TRUE)
}

#' Default value for NULL
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
