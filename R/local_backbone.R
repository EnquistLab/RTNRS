#' Session cache for loaded backbones
#'
#' Building the indexes over three million names takes a minute or two, so a
#' loaded backbone is kept for the life of the session rather than rebuilt per
#' call.
#' @keywords internal
#' @noRd
tnrs_backbone_cache <- new.env(parent = emptyenv())

#' Path to a source's prebuilt match index
#' @keywords internal
#' @noRd
tnrs_index_path <- function(source, dir = tnrs_cache_dir()) {
  file.path(dir, paste0(source, "-index.rds"))
}

#' Build the match index for one source's name table
#'
#' Internal.  Produces the blocking indexes for family, genus and species, plus
#' the maps from a matched name part back to the rows of the name table that
#' carry it.  This is the local equivalent of the denormalised
#' \code{famlist}/\code{genlist}/\code{splist} tables and their foreign keys.
#'
#' @param names A name table from one of the importers.
#' @param quiet Suppress progress messages?
#' @return A list of indexes.
#' @keywords internal
#' @noRd
tnrs_build_source_index <- function(names, quiet = FALSE) {
  if (!quiet) message("  building match index ...")

  has_genus <- nzchar(names$genus)
  genera <- sort(unique(names$genus[has_genus]))
  genus_of_row <- match(names$genus, genera)

  families <- sort(unique(names$family[nzchar(names$family)]))

  # Distinct genus + epithet pairs, keyed so that a matched pair can be turned
  # back into the rows of the name table that share it
  has_species <- has_genus & nzchar(names$specific_epithet)
  species_key <- paste0(names$genus, " ", names$specific_epithet)
  species_key[!has_species] <- ""
  species_levels <- sort(unique(species_key[has_species]))
  species_of_row <- match(species_key, species_levels)

  species_genus <- sub(" .*$", "", species_levels)
  species_epithet <- sub("^[^ ]+ ", "", species_levels)

  # Infraspecific names, keyed on their parent species the same way
  has_infra <- has_species & nzchar(names$infraspecific_epithet)
  infra_key <- paste0(species_key, " ", names$infraspecific_epithet)
  infra_key[!has_infra] <- ""
  infra_levels <- sort(unique(infra_key[has_infra]))
  infra_of_row <- match(infra_key, infra_levels)

  infra_species <- sub(" [^ ]+$", "", infra_levels)
  infra_epithet <- sub("^.* ", "", infra_levels)

  list(
    family = tnrs_build_rank_index(families, "genus_only"),
    genus = tnrs_build_rank_index(genera, "genus_only"),
    species = tnrs_build_rank_index(
      species_epithet, "epithet_only",
      parent = match(species_genus, genera), affix = FALSE
    ),
    infra1 = tnrs_build_rank_index(
      infra_epithet, "epithet_only",
      parent = match(infra_species, species_levels), affix = FALSE
    ),
    # Rows of the name table reachable from each matched part
    rows_by_genus = tnrs_int_index(genus_of_row, length(genera)),
    rows_by_species = tnrs_int_index(species_of_row, length(species_levels)),
    rows_by_infra1 = tnrs_int_index(infra_of_row, length(infra_levels)),
    rows_by_name = tnrs_hash_index(tnrs_toupper_ascii(names$scientific_name)),
    n_genera = length(genera),
    n_species = length(species_levels),
    n_infra1 = length(infra_levels)
  )
}

#' Load the local backbone, building its index if needed
#'
#' Internal.  Returns the name tables and match indexes for the requested
#' sources, loading them from the cache and holding them for the rest of the
#' session.
#'
#' @param sources Character vector of source names.
#' @param dir Cache directory.
#' @param quiet Suppress progress messages?
#' @return A named list, one entry per source, each holding \code{names} and
#'   \code{index}.
#' @keywords internal
#' @noRd
tnrs_backbone <- function(sources = c("wcvp", "wfo"), dir = tnrs_cache_dir(),
                          quiet = FALSE) {
  out <- list()

  for (source in sources) {
    key <- paste(source, dir, sep = "|")

    if (!is.null(tnrs_backbone_cache[[key]])) {
      out[[source]] <- tnrs_backbone_cache[[key]]
      next
    }

    names <- tnrs_load_names(source, dir)

    index_file <- tnrs_index_path(source, dir)
    if (file.exists(index_file)) {
      index <- readRDS(index_file)
    } else {
      if (!quiet) message("Preparing '", source, "' for matching ...")
      index <- tnrs_build_source_index(names, quiet = quiet)
      saveRDS(index, index_file, compress = "gzip")
    }

    loaded <- list(source = source, names = names, index = index)
    tnrs_backbone_cache[[key]] <- loaded
    out[[source]] <- loaded
  }

  out
}

#' Forget any backbone held in memory
#'
#' Internal.  Used by the tests, and useful after rebuilding the cache within a
#' single session.
#' @keywords internal
#' @noRd
tnrs_backbone_forget <- function() {
  rm(list = ls(tnrs_backbone_cache), envir = tnrs_backbone_cache)
  invisible(NULL)
}
