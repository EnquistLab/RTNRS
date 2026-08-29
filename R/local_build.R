#' Build the local taxonomic backbone
#'
#' Downloads the taxonomic sources and prepares them for offline name
#' resolution.  This needs to be done once; afterwards the local functions work
#' with no internet connection.  The data is kept in the standard user cache
#' directory, and can be removed again with \code{TNRS_local_remove()}.
#'
#' The download is large, roughly 200 MB for both sources, and preparing it
#' takes a few minutes.  Each source is recorded with its version and, where the
#' publisher provides one, its DOI, so that results obtained locally can be
#' cited as precisely as results from the web service.  Use
#' \code{TNRS_local_status()} to see what has been built.
#'
#' @param sources Character vector of sources to build. Options are "wcvp" and
#'   "wfo"; both are used by default, matching the web service.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param overwrite Re-download and rebuild even if the data is already present?
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
#' # See what is available offline
#' TNRS_local_status()
#' }
TNRS_local_build <- function(sources = c("wcvp", "wfo"),
                             dir = tnrs_cache_dir(create = TRUE),
                             overwrite = FALSE,
                             quiet = FALSE) {
  registry <- tnrs_source_registry()

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
    # form is saved; the archive is kept so a rebuild needs no download
    unlink(extracted)
    # Remove any name table left by an earlier version of this package
    unlink(file.path(dir, paste0(source, "-names.rds")))
    rm(names)
    invisible(gc(verbose = FALSE))

    if (!quiet) message("Built '", source, "'.")
  }

  status <- TNRS_local_status(dir)
  if (!quiet && !is.null(status)) {
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
