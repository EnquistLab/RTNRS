#' Build the local taxonomic backbone
#'
#' Downloads the taxonomic sources and prepares them for offline name
#' resolution.  This needs to be done once; afterwards the local functions work
#' with no internet connection.  The data is kept in the standard user cache
#' directory, and can be removed again with \code{TNRS_local_remove()}.
#'
#' The download is large and preparing it takes a few minutes.  World Flora
#' Online is about 116 MB to download and occupies roughly 120 MB once built;
#' the World Checklist of Vascular Plants about 85 MB and 107 MB.  The
#' downloaded archive is deleted once the data has been prepared, since it is
#' not needed for matching; pass \code{keep_archive = TRUE} to keep it, which
#' roughly doubles the space used but means a rebuild needs no second download.
#' Each source is recorded with its version and, where the publisher provides
#' one, its DOI, so that results obtained locally can be cited as precisely as
#' results from the web service.  Use \code{TNRS_local_status()} to see what has
#' been built.
#'
#' @param sources Character vector of sources to build. Options are "wcvp" and
#'   "wfo". Only "wfo" is built by default, matching the default of
#'   \code{TNRS_local()}; build both to reproduce the web service, which
#'   consults them together.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param overwrite Re-download and rebuild even if the data is already present?
#' @param keep_archive Keep the downloaded archive after building? It is not
#'   needed for matching and roughly doubles the space used, so it is deleted by
#'   default. Keep it if you expect to rebuild, for instance while testing, or
#'   if you want the original file to hand: rebuilding without it means
#'   downloading again. Its size and checksum are recorded either way, so
#'   removing it costs nothing in provenance. This applies to a source that is
#'   already built, so running the build again is how to reclaim the space used
#'   by archives left behind by earlier versions of this package.
#' @param quiet Suppress progress messages?
#' @return The output of \code{TNRS_local_status()}, invisibly.
#' @note The taxonomic sources are downloaded from their publishers: the World
#'   Checklist of Vascular Plants from the Royal Botanic Gardens, Kew, and the
#'   World Flora Online from Zenodo. Please cite them alongside the TNRS.
#' @export
#' @examples \dontrun{
#' # One-off setup; needs an internet connection and a few minutes
#' TNRS_local_build()
#'
#' # Add the second source, to consult what the web service consults
#' TNRS_local_build("wcvp")
#'
#' # See what is available offline
#' TNRS_local_status()
#' }
TNRS_local_build <- function(sources = "wfo",
                             dir = tnrs_cache_dir(create = TRUE),
                             overwrite = FALSE,
                             keep_archive = FALSE,
                             quiet = FALSE) {
  registry <- tnrs_builtin_registry()

  # A source the user supplied cannot be downloaded, so say so rather than
  # reporting it as unknown, which it is not
  custom <- setdiff(sources, names(registry))
  custom <- custom[unname(tnrs_is_custom(custom, dir))]
  if (length(custom) > 0) {
    message(
      "Cannot build: ", paste(custom, collapse = ", "),
      ". You supplied ", if (length(custom) == 1) "it" else "them",
      ", so there is nothing to download.",
      "
Use TNRS_local_add_source() to register the data again."
    )
    return(invisible(NULL))
  }

  unknown <- setdiff(sources, names(registry))
  if (length(unknown) > 0) {
    message(
      "Unknown source(s): ", paste(unknown, collapse = ", "),
      ". Options are: ", paste(names(registry), collapse = ", ")
    )
    return(invisible(NULL))
  }

  for (source in sources) {
    names_file <- tnrs_names_path(source, dir)

    if (file.exists(names_file) && !overwrite) {
      if (!quiet) message("Source '", source, "' is already built; skipping.")
      # Still applied, so that a source built by an earlier version of this
      # package, which always kept its archive, can be tidied by building again
      tnrs_tidy_archive(source, dir = dir, keep_archive = keep_archive, quiet = quiet)
      next
    }

    tnrs_download_source(source, dir = dir, overwrite = overwrite, quiet = quiet)

    member <- tnrs_source_member(source)
    extracted <- tnrs_unpack_source(source, member, dir = dir)

    names <- switch(source,
      wcvp = tnrs_import_wcvp(extracted, quiet = quiet),
      wfo = tnrs_import_wfo(extracted, quiet = quiet)
    )
    names <- tnrs_link_accepted(names)

    # Parquet with the gzip codec.  It is smaller than a gzipped RDS, several
    # times faster to read, and because the compression is per column chunk the
    # footer and column pruning survive it: loading three columns of the name
    # table takes about a second rather than reading all fifteen.
    #
    # gzip rather than zstd deliberately: nanoparquet 0.5.1 accepts zstd but
    # silently writes the data uncompressed.
    nanoparquet::write_parquet(names, names_file, compression = "gzip")

    # The extracted text file is large and no longer needed once the compact
    # form is saved
    unlink(extracted)
    # Remove any name table left by an earlier version of this package
    unlink(file.path(dir, paste0(source, "-names.rds")))
    rm(names)
    invisible(gc(verbose = FALSE))

    tnrs_tidy_archive(source, dir = dir, keep_archive = keep_archive, quiet = quiet)

    if (!quiet) message("Built '", source, "'.")
  }

  # Its own note about what is still unbuilt would be noise here, since the
  # caller has just chosen what to build
  status <- suppressMessages(TNRS_local_status(dir))
  if (!quiet && any(status$built)) {
    message(
      "\nLocal backbone ready. Please cite the sources listed by ",
      "TNRS_local_status()."
    )
  }
  invisible(status)
}

#' File within a source archive that carries the names
#' @keywords internal
#' @noRd
tnrs_source_member <- function(source) {
  switch(source,
    wcvp = "wcvp_names.csv",
    wfo = "classification.csv",
    stop("No archive member known for source '", source, "'.", call. = FALSE)
  )
}

#' Path to a built source's name table
#'
#' The \code{.gz.parquet} suffix is conventional and records the codec, so the
#' file is self-describing to anything else that reads it.
#' @keywords internal
#' @noRd
tnrs_names_path <- function(source, dir = tnrs_cache_dir()) {
  file.path(dir, paste0(source, "-names.gz.parquet"))
}

#' Load a built source's name table
#'
#' @param source Source name.
#' @param dir Cache directory.
#' @param columns Optional character vector of columns to read.  Parquet stores
#'   columns separately, so reading a few is far cheaper than reading all of
#'   them; the matcher only needs the name parts to build its index.
#' @return The name table.
#' @keywords internal
#' @noRd
tnrs_load_names <- function(source, dir = tnrs_cache_dir(), columns = NULL) {
  path <- tnrs_names_path(source, dir)

  if (!file.exists(path)) {
    stop(
      "Source '", source, "' has not been built. Run TNRS_local_build().",
      call. = FALSE
    )
  }

  if (is.null(columns)) {
    nanoparquet::read_parquet(path)
  } else {
    nanoparquet::read_parquet(path, col_select = columns)
  }
}

#' Delete a source's downloaded archive once it has been built
#'
#' Internal.  The archive is only needed to produce the name table, so keeping
#' it roughly doubles the space a source occupies.  It is removed unless the
#' caller asked to keep it, and only once there is a name table to show for it,
#' so an interrupted build never loses the download it would otherwise reuse.
#'
#' The size and checksum stay in the provenance record.  They describe the file
#' that was downloaded and remain true once it is gone, which is what keeps a
#' local result citable.
#'
#' @param source Source name.
#' @param dir Cache directory.
#' @param keep_archive Keep the archive?
#' @param quiet Suppress progress messages?
#' @return TRUE if an archive was removed, FALSE otherwise, invisibly.
#' @keywords internal
#' @noRd
tnrs_tidy_archive <- function(source, dir = tnrs_cache_dir(),
                              keep_archive = FALSE, quiet = FALSE) {
  if (isTRUE(keep_archive)) {
    return(invisible(FALSE))
  }
  if (!file.exists(tnrs_names_path(source, dir))) {
    return(invisible(FALSE))
  }

  record_path <- tnrs_provenance_path(source, dir)
  if (!file.exists(record_path)) {
    return(invisible(FALSE))
  }
  record <- readRDS(record_path)

  archive <- record$archive
  if (is.null(archive) || is.na(archive) || !file.exists(archive)) {
    return(invisible(FALSE))
  }

  freed <- file.size(archive)
  if (unlink(archive) != 0) {
    return(invisible(FALSE))
  }

  record$archive <- NA_character_
  record$archive_kept <- FALSE
  saveRDS(record, record_path)

  if (!quiet) {
    message(
      "  removed the ", round(freed / 1024^2, 1), " MB archive; ",
      "rebuilding '", source, "' would download it again ",
      "(keep_archive = TRUE to keep it)."
    )
  }
  invisible(TRUE)
}

#' How a set of source names would be written in a call
#' @keywords internal
#' @noRd
tnrs_source_arg <- function(sources) {
  quoted <- paste0('"', sources, '"')
  if (length(quoted) == 1) quoted else paste0("c(", paste(quoted, collapse = ", "), ")")
}

#' Make sure the requested sources are available locally
#'
#' Internal.  Compares what was asked for against what has been built, and
#' either builds the difference or explains how to.  Nothing is ever downloaded
#' without the user's agreement: either they answer the prompt, or they asked
#' for it in the call by passing \code{build_missing = TRUE}.  That matters
#' because the download is large and lands in the user's cache directory rather
#' than a temporary one.
#'
#' @param sources Character vector of requested sources.
#' @param dir Cache directory.
#' @param build_missing Build sources that are not present? Defaults to
#'   \code{interactive()}, which asks first.
#' @param quiet Suppress progress messages?
#' @return TRUE if every requested source is now available, FALSE otherwise.
#' @keywords internal
#' @noRd
tnrs_require_sources <- function(sources, dir = tnrs_cache_dir(),
                                 build_missing = interactive(),
                                 quiet = FALSE) {
  present <- vapply(
    sources, function(s) file.exists(tnrs_names_path(s, dir)), logical(1)
  )
  missing <- sources[!present]
  if (length(missing) == 0) {
    return(TRUE)
  }

  # A user-supplied source whose data has gone cannot be rebuilt from here,
  # so it is reported separately and the rest carries on
  gone <- missing[unname(tnrs_is_custom(missing, dir))]
  if (length(gone) > 0) {
    message(
      "The data for ", paste(gone, collapse = ", "),
      " is missing, and this package cannot fetch it because you supplied it.",
      "
Register it again with TNRS_local_add_source()."
    )
    return(FALSE)
  }

  registry <- tnrs_builtin_registry()
  download_mb <- sum(vapply(registry[missing], function(x) x$download_mb, numeric(1)))
  disk_mb <- sum(vapply(registry[missing], function(x) x$disk_mb, numeric(1)))
  fix <- paste0("TNRS_local_build(", tnrs_source_arg(missing), ")")

  # Mentioning what is already built saves a round trip when the user asked for
  # two sources and only meant to add one
  built <- sources[present]
  have <- if (length(built) > 0) {
    paste0(" (", paste(built, collapse = ", "), " is already built.)")
  } else {
    ""
  }

  if (!isTRUE(build_missing)) {
    message(
      "No local copy of: ", paste(missing, collapse = ", "), ".", have,
      "\nRun ", fix, " once to download and prepare it",
      " (about ", download_mb, " MB to download).",
      "\nOr call this function again with build_missing = TRUE to do it now."
    )
    return(FALSE)
  }

  # An explicit build_missing = TRUE is itself the user's agreement, so the
  # prompt is only for the interactive default
  if (interactive()) {
    answer <- readline(paste0(
      "TNRS needs to download ", paste(missing, collapse = ", "),
      ": about ", download_mb, " MB, using roughly ", disk_mb,
      " MB of disk in\n  ", dir, "\nDownload now? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing downloaded. Run ", fix, " when you are ready.")
      return(FALSE)
    }
  }

  TNRS_local_build(sources = missing, dir = dir, quiet = quiet)

  still_missing <- missing[!vapply(
    missing, function(s) file.exists(tnrs_names_path(s, dir)), logical(1)
  )]
  if (length(still_missing) > 0) {
    message("Could not build: ", paste(still_missing, collapse = ", "), ".")
    return(FALSE)
  }

  TRUE
}
