#' Nomenclatural codes a source can follow
#'
#' Internal.  Two codes are distinguished, because they differ in ways the
#' matcher cares about: botanical families end in -aceae where zoological ones
#' end in -idae, and a zoological trinomial carries no rank connector where a
#' botanical one does.
#'
#' A source covering all life follows both, and is recorded as "mixed".  That is
#' not a third code; it expands to both when the codes in play are worked out.
#'
#' @return The codes, in the order they are reported.
#' @keywords internal
#' @noRd
tnrs_nomenclature_values <- function() {
  c("botanical", "zoological", "mixed")
}

#' Expand a nomenclature setting into the codes it covers
#'
#' Internal.  "mixed" becomes both; anything else is itself.  Validates as it
#' goes, so a mistyped code is reported where it was given rather than surfacing
#' later as a name that mysteriously fails to match.
#'
#' @param x Character vector of settings.
#' @return The distinct codes covered, without "mixed".
#' @keywords internal
#' @noRd
tnrs_nomenclature_codes <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return("botanical")
  }

  unknown <- setdiff(x, tnrs_nomenclature_values())
  if (length(unknown) > 0) {
    stop(
      "Unknown nomenclature: ", paste(unique(unknown), collapse = ", "),
      ". Options are: ", paste(tnrs_nomenclature_values(), collapse = ", "), ".",
      call. = FALSE
    )
  }

  out <- unique(c(
    if (any(x %in% c("botanical", "mixed"))) "botanical",
    if (any(x %in% c("zoological", "mixed"))) "zoological"
  ))
  out
}

#' Nomenclature recorded for a set of sources
#'
#' Internal.  A source that predates this setting, or one registered without
#' it, is botanical: that is what every source this package shipped before now
#' was, so it keeps existing caches working unchanged.
#'
#' @param sources Character vector of source names.
#' @param dir Cache directory.
#' @return One setting per source, named by source.
#' @keywords internal
#' @noRd
tnrs_source_nomenclature <- function(sources, dir = tnrs_cache_dir()) {
  registry <- tnrs_source_registry(dir)
  out <- vapply(sources, function(s) {
    spec <- registry[[s]]
    value <- if (is.null(spec)) NULL else spec$nomenclature
    if (is.null(value) || is.na(value) || !nzchar(value)) "botanical" else value
  }, character(1))
  names(out) <- sources
  out
}

#' Codes to resolve under, given the sources and any explicit choice
#'
#' Internal.  The sources decide it unless the caller says otherwise, so that
#' asking for an animal backbone reads animal names without anyone having to
#' remember a second argument.  Asking for both a plant and an animal source
#' resolves under both, which is what makes a cross-kingdom homonym visible
#' rather than silently resolved under one code.
#'
#' @param sources Character vector of source names.
#' @param nomenclature Explicit setting, or NULL to take it from the sources.
#' @param dir Cache directory.
#' @return The codes in play.
#' @keywords internal
#' @noRd
tnrs_effective_codes <- function(sources, nomenclature = NULL,
                                 dir = tnrs_cache_dir()) {
  if (!is.null(nomenclature)) {
    return(tnrs_nomenclature_codes(nomenclature))
  }
  tnrs_nomenclature_codes(tnrs_source_nomenclature(sources, dir = dir))
}
