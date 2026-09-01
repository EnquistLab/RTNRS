#' Rank names used by EDIT platform exports
#'
#' Internal.  The CDM software behind the Caryophyllales.org portals spells a
#' few ranks its own way, "Section bot." for section among them, because its
#' rank vocabulary covers zoology as well.  They are folded onto the botanical
#' spellings so that the shared rank table can do the rest.
#' @keywords internal
#' @noRd
tnrs_cdm_rank <- function(rank) {
  rank <- tolower(trimws(as.character(rank)))
  rank[is.na(rank)] <- ""
  # "bot." marks the botanical sense of a rank the vocabulary also defines for
  # zoology; the distinction does not survive into a name
  rank <- trimws(sub("[[:space:]]*bot[.]?$", "", rank))
  rank[rank == "species aggregate"] <- "species"
  rank
}

#' Status implied by the subtree a name sits in
#'
#' Internal.  A Caryophyllales.org export is not a flat list of accepted names.
#' Its accepted sheet is the whole taxonomic tree, and the authors separate the
#' core checklist from names they could not place, using placeholder nodes named
#' \code{__<Family>_<n>_<label>}.  In the Cactaceae export a third of the sheet
#' sits outside the core checklist, so reading it as accepted would promote
#' names of uncertain application to accepted taxa.
#'
#' The label after the number carries the meaning and is used across the
#' portal's families, so it is what is matched on rather than the family name.
#' Anything unrecognised is treated as unchecked rather than accepted, since
#' wrongly accepting a name is the more damaging error.
#'
#' @param label Placeholder node name, or "" for the root.
#' @return One of the statuses the resolver scores on, in lower case.
#' @keywords internal
#' @noRd
tnrs_cdm_status <- function(label) {
  label <- tolower(gsub("^_+", "", as.character(label)))
  # Strip the family and sequence number, leaving the meaningful label
  label <- sub("^[a-z]+_[0-9]+_", "", label)

  known <- c(
    "core_checklist" = "accepted",
    # Named hybrids are accepted names; the export keeps them apart only
    # because most are of garden origin
    "hybrids" = "accepted",
    "names_of_uncertain_application" = "unchecked",
    "unresolved_names" = "unchecked",
    "unplaced_taxa" = "unplaced",
    "excluded_names" = "unplaced"
  )

  out <- unname(known[label])
  out[is.na(out)] <- "unchecked"
  out[label == ""] <- "accepted"
  out
}

#' Assign each name the status of the subtree it sits under
#'
#' Internal.  Placeholder nodes prefix their descendants' \code{treeIndex}, so a
#' name belongs to whichever placeholder its own index starts with.  Without a
#' \code{treeIndex} column there is nothing to go on and every name is taken as
#' accepted, which is what a flat export means.
#'
#' @param name Character vector of names.
#' @param tree_index Character vector of tree indices, possibly all empty.
#' @return A status per name.
#' @keywords internal
#' @noRd
tnrs_cdm_subtree_status <- function(name, tree_index) {
  if (!any(nzchar(tree_index))) {
    return(rep("accepted", length(name)))
  }

  placeholder <- grepl("_", name, fixed = TRUE) & nzchar(tree_index)
  out <- rep("accepted", length(name))
  if (!any(placeholder)) {
    return(out)
  }

  # Longest index first, so a nested placeholder wins over its parent
  nodes <- data.frame(
    label = name[placeholder], index = tree_index[placeholder],
    stringsAsFactors = FALSE
  )
  nodes <- nodes[order(nchar(nodes$index)), , drop = FALSE]

  for (i in seq_len(nrow(nodes))) {
    under <- startsWith(tree_index, nodes$index[i]) &
      tree_index != nodes$index[i]
    out[under] <- tnrs_cdm_status(nodes$label[i])
  }
  out
}

#' Convert a taxonomic export from the EDIT platform to Darwin Core
#'
#' Turns the accepted-name and synonym tables exported from a CDM/EDIT
#' taxonomic database into a single Darwin Core table, which is the form
#' \code{TNRS_local_add_source()} reads without needing a column mapping.
#'
#' This is the shape the checklists at Caryophyllales.org are supplied in,
#' Cactaceae among them, exported as one sheet of accepted taxa and one of
#' synonyms carrying the identifier of the taxon each is a synonym of.
#'
#' The export carries placeholder nodes that organise the tree rather than
#' name a taxon, such as \code{__Cactaceae_2_hybrids}. They are recognised by
#' the underscore in their name, which no botanical name contains, and dropped.
#'
#' @param accepted Data.frame of accepted taxa. Needs \code{uuid},
#'   \code{pureName} and a rank column; \code{author} is used when present.
#' @param synonym Optional data.frame of synonyms. Needs the same, plus
#'   \code{accepted_ID} giving the \code{uuid} of the accepted taxon.
#' @param family Family to record for every name, since the export states it
#'   only as the root of the tree. Optional.
#' @return A data.frame in Darwin Core, ready to pass to
#'   \code{TNRS_local_add_source()}.
#' @seealso \code{\link{TNRS_local_add_source}}
#' @export
#' @examples \dontrun{
#' # The Cactaceae checklist, supplied as a workbook of two sheets
#' accepted <- readxl::read_excel("CactaceaeFullList.xlsx", sheet = "Accepted")
#' synonym <- readxl::read_excel("CactaceaeFullList.xlsx", sheet = "Synonym")
#'
#' cact <- TNRS_cdm_to_dwc(accepted, synonym, family = "Cactaceae")
#'
#' TNRS_local_add_source(
#'   cact,
#'   source = "cact", version = "2023-11-02",
#'   full_name = "Cactaceae at Caryophyllales.org",
#'   doi = "10.3372/wi.51.51208"
#' )
#' }
TNRS_cdm_to_dwc <- function(accepted, synonym = NULL, family = NA_character_) {
  one <- function(x, status, label) {
    if (!is.data.frame(x)) {
      stop("`", label, "` should be a data.frame.", call. = FALSE)
    }
    x <- as.data.frame(x, stringsAsFactors = FALSE)

    # The two sheets spell the rank column differently, RANK against rank
    rank_col <- intersect(c("RANK", "rank", "taxonRank"), names(x))
    required <- c("uuid", "pureName")
    absent <- setdiff(required, names(x))
    if (length(absent) > 0 || length(rank_col) == 0) {
      absent <- c(absent, if (length(rank_col) == 0) "RANK")
      stop(
        "`", label, "` is missing the column(s) ",
        paste(absent, collapse = ", "), ". Found: ",
        paste(names(x), collapse = ", "), ".",
        call. = FALSE
      )
    }

    text <- function(field) {
      if (!field %in% names(x)) {
        return(rep("", nrow(x)))
      }
      value <- trimws(as.character(x[[field]]))
      # Autonyms carry no author, and the export writes that as a null
      value[is.na(value) | value == "NA" | value == "null"] <- ""
      value
    }

    name <- text("pureName")

    if (status == "accepted") {
      # Read off the tree rather than assumed, so that the authors' own
      # separation of the core checklist from names they could not place is
      # carried through instead of flattened into "accepted"
      resolved <- tnrs_cdm_subtree_status(name, text("treeIndex"))
      accepted_id <- ifelse(resolved == "accepted", text("uuid"), "")
      status <- resolved
    } else {
      accepted_id <- text("accepted_ID")
    }

    data.frame(
      taxonID = text("uuid"),
      scientificName = name,
      scientificNameAuthorship = text("author"),
      taxonRank = tnrs_cdm_rank(x[[rank_col[1]]]),
      taxonomicStatus = status,
      family = as.character(family),
      acceptedNameUsageID = accepted_id,
      stringsAsFactors = FALSE
    )
  }

  out <- one(accepted, "accepted", "accepted")
  if (!is.null(synonym)) {
    if (!"accepted_ID" %in% names(synonym)) {
      stop(
        "`synonym` needs an accepted_ID column naming the accepted taxon.",
        call. = FALSE
      )
    }
    out <- rbind(out, one(synonym, "synonym", "synonym"))
  }

  # Placeholder nodes organising the tree, never real names
  out <- out[!grepl("_", out$scientificName, fixed = TRUE), , drop = FALSE]
  out <- out[nzchar(out$scientificName), , drop = FALSE]
  row.names(out) <- NULL
  out
}
