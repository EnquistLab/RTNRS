#' The classification ranks the local name table carries
#'
#' Internal.  Every name row records these, above the genus, so that a search
#' can be confined to a taxon at any of them.  A source that does not publish
#' a rank leaves it empty: the plant sources give the family alone, the
#' Catalogue of Life gives all five.
#' @keywords internal
#' @noRd
tnrs_classification_ranks <- function() {
  c("kingdom", "phylum", "class", "order", "family")
}

#' Fill in classification a source gives only on its accepted names
#'
#' Internal.  The Catalogue of Life puts the classification on the accepted
#' taxon and leaves every synonym row blank, so half its rows say nothing about
#' where they belong.  Each empty rank is copied from the accepted name the row
#' points at.  A rank the source never records stays empty, and a column the
#' importer did not supply is added, so the table always has the same shape.
#'
#' Applied after \code{tnrs_link_accepted()}, which is what makes the copy a
#' vector lookup rather than a join.
#'
#' @param names A linked name table.
#' @return The same table, with every classification column present and
#'   filled from the accepted name wherever it was empty.
#' @keywords internal
#' @noRd
tnrs_inherit_classification <- function(names) {
  n <- nrow(names)
  accepted <- names$accepted_name_id
  from_accepted <- !is.na(accepted) & accepted != seq_len(n)

  for (rank in tnrs_classification_ranks()) {
    value <- names[[rank]]
    if (is.null(value)) {
      value <- rep("", n)
    }
    value <- as.character(value)
    value[is.na(value)] <- ""

    fill <- from_accepted & !nzchar(value)
    if (any(fill)) {
      inherited <- value[accepted[fill]]
      inherited[is.na(inherited)] <- ""
      value[fill] <- inherited
    }
    names[[rank]] <- value
  }
  names
}

#' Work out which rows, genera and families of a source lie within a taxon
#'
#' Internal.  A scope is a set of taxon names at family rank or above.  A row
#' is in scope when any of its classification columns names one of them, so a
#' synonym counts through the classification it inherited from its accepted
#' name, and a genus that is a homonym across kingdoms keeps only the rows
#' that belong.  The genus and family masks say which entries of the match
#' index have at least one row in scope, which is what stops the fuzzy
#' matcher wandering to a lookalike in another kingdom.
#'
#' @param backbone One element of \code{tnrs_backbone()}.
#' @param within Character vector of taxon names.
#' @return A list with \code{row}, \code{genus} and \code{family} logical
#'   masks, and \code{found}, the subset of \code{within} present in this
#'   source.
#' @keywords internal
#' @noRd
tnrs_scope_mask <- function(backbone, within) {
  names <- backbone$names
  index <- backbone$index
  source <- backbone$source
  ranks <- tnrs_classification_ranks()

  absent <- setdiff(ranks, colnames(names))
  if (length(absent) > 0) {
    fix <- if (isTRUE(tnrs_is_custom(source))) {
      "register it again with TNRS_local_add_source()"
    } else {
      paste0("rebuild it with TNRS_local_build(\"", source, "\", overwrite = TRUE)")
    }
    stop(
      "Source '", source, "' was built by an earlier version of this package ",
      "and does not carry the classification that 'within' needs; ", fix, ".",
      call. = FALSE
    )
  }

  wanted <- tnrs_toupper_ascii(within)
  row <- rep(FALSE, nrow(names))
  found <- character(0)

  for (rank in ranks) {
    # Matched on the distinct values of the column, which are few, rather than
    # on every row, which for the Catalogue of Life is five million strings
    values <- unique(names[[rank]])
    values <- values[nzchar(values)]
    hit <- values[tnrs_toupper_ascii(values) %in% wanted]
    if (length(hit) == 0) {
      next
    }
    row <- row | names[[rank]] %in% hit
    found <- c(found, within[wanted %in% tnrs_toupper_ascii(hit)])
  }

  # The scope taxon's own row carries its classification only above itself:
  # the "Ixodida" order row has an empty order column.  It is in scope by
  # definition, or a name submitted as the group itself would find nothing.
  # Genus-rank rows are left out, since a genus cannot be a scope and a genus
  # homonymous with a family elsewhere must not be pulled in by its spelling.
  own <- tnrs_toupper_ascii(names$scientific_name) %in% wanted &
    !names$name_rank %in% c(
      "genus", "subgenus", "section", "series", "species", "subspecies",
      "variety", "subvariety", "form", "subform"
    )
  if (any(own)) {
    row <- row | own
    found <- c(found, within[wanted %in% tnrs_toupper_ascii(names$scientific_name[own])])
  }

  genus <- rep(FALSE, length(index$genus$name))
  family <- rep(FALSE, length(index$family$name))
  if (any(row)) {
    in_scope <- match(unique(names$genus[row]), index$genus$name)
    genus[in_scope[!is.na(in_scope)]] <- TRUE
    family <- index$family$name %in% unique(names$family[row])
  }

  list(row = row, genus = genus, family = family, found = unique(found))
}

#' The rows, genera and families two scopes have in common
#'
#' Internal.  Used when a name carries its own higher-taxon prefix inside a
#' \code{within}: the search is confined to both.  The genus and family
#' masks are intersected too; that can leave a genus allowed whose every row
#' is then refused by the row mask, which is harmless, since the row mask
#' is what decides.
#' @keywords internal
#' @noRd
tnrs_scope_intersect <- function(a, b) {
  list(
    row = a$row & b$row, genus = a$genus & b$genus, family = a$family & b$family,
    found = intersect(a$found, b$found)
  )
}

#' Keep only the name-table rows a scope allows
#' @keywords internal
#' @noRd
tnrs_scope_rows <- function(rows, scope) {
  if (is.null(scope) || length(rows) == 0) {
    return(rows)
  }
  rows[scope$row[rows]]
}

#' Keep only the index positions a mask allows
#' @keywords internal
#' @noRd
tnrs_scope_positions <- function(positions, mask) {
  if (is.null(mask) || length(positions) == 0) {
    return(positions)
  }
  positions[mask[positions]]
}
