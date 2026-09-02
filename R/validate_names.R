#' Check and normalise submitted names
#'
#' Internal.  Turns whatever the caller supplied into the two-column data.frame
#' the rest of the package works with, or explains what is wrong with it.
#'
#' The web service answers a malformed request with "HTTP Status 400", which
#' says nothing about what the caller got wrong.  Everything checkable is
#' therefore checked here, before anything is sent.
#'
#' @param taxonomic_names A data.frame of two columns, identifier and name, or
#'   a character vector of names.
#' @param arg Name of the argument being checked, for the error message.
#' @return A two-column data.frame, \code{ID} and \code{name}, both character.
#' @keywords internal
#' @noRd
tnrs_check_names <- function(taxonomic_names, arg = "taxonomic_names") {
  if (missing(taxonomic_names) || is.null(taxonomic_names)) {
    stop("`", arg, "` is required: supply names to resolve.", call. = FALSE)
  }

  if (is.factor(taxonomic_names)) {
    taxonomic_names <- as.character(taxonomic_names)
  }

  if (is.character(taxonomic_names)) {
    if (length(taxonomic_names) == 0) {
      stop("`", arg, "` is empty: there are no names to resolve.", call. = FALSE)
    }
    return(data.frame(
      ID = as.character(seq_along(taxonomic_names)),
      name = as.character(taxonomic_names),
      stringsAsFactors = FALSE
    ))
  }

  if (!is.data.frame(taxonomic_names)) {
    stop(
      "`", arg, "` should be a data.frame of two columns, an identifier and a ",
      "name, or a character vector of names. Given: ",
      paste(class(taxonomic_names), collapse = "/"), ".",
      call. = FALSE
    )
  }

  if (nrow(taxonomic_names) == 0) {
    stop("`", arg, "` has no rows: there are no names to resolve.", call. = FALSE)
  }

  # The web service takes an identifier and a name, in that order, and rejects
  # anything else with a status code rather than an explanation
  if (ncol(taxonomic_names) != 2) {
    stop(
      "`", arg, "` should have exactly two columns, an identifier and a name, ",
      "but has ", ncol(taxonomic_names),
      if (!is.null(names(taxonomic_names))) {
        paste0(" (", paste(names(taxonomic_names), collapse = ", "), ")")
      } else {
        ""
      },
      ".\nSelect the two you want, for example ", arg, "[, c(1, 2)].",
      call. = FALSE
    )
  }

  out <- data.frame(
    ID = as.character(taxonomic_names[[1]]),
    name = as.character(taxonomic_names[[2]]),
    stringsAsFactors = FALSE
  )

  if (anyNA(out$ID) || any(!nzchar(trimws(out$ID)))) {
    stop(
      "The first column of `", arg, "` holds the identifiers, and some are ",
      "missing or empty. Every name needs one to be matched back to.",
      call. = FALSE
    )
  }

  if (anyDuplicated(out$ID) > 0) {
    duplicated_ids <- unique(out$ID[duplicated(out$ID)])
    stop(
      "The identifiers in the first column of `", arg, "` must be unique, ",
      "but ", length(duplicated_ids), " ",
      if (length(duplicated_ids) == 1) "is" else "are",
      " repeated: ",
      paste(utils::head(duplicated_ids, 5), collapse = ", "),
      if (length(duplicated_ids) > 5) ", ..." else "", ".",
      call. = FALSE
    )
  }

  out
}

#' Is a submitted name blank?
#'
#' Internal.  A name that is missing, empty, or only whitespace cannot be
#' resolved.  Upstream strips such names before matching, which is what shifts
#' every later name onto the wrong identifier, so they are held back here
#' instead and added to the answer afterwards.
#' @keywords internal
#' @noRd
tnrs_is_blank_name <- function(x) {
  is.na(x) | !nzchar(trimws(x))
}

#' Reduce submitted names to the request actually worth sending
#'
#' Internal.  Two things upstream does to a request make the answer hard to
#' match back to it: it drops names that are blank, which shifts the identifiers
#' of everything after them, and it combines rows sharing a name into one whose
#' identifier is the others pasted together.
#'
#' Both are avoided by sending each distinct name once, under an identifier of
#' our own, and rebuilding the caller's rows from the answer.  It is also less
#' work for the service, since a list with repeats is sent only once.
#'
#' @param submitted A checked two-column data.frame.
#' @return A data.frame of \code{ID} and \code{name} to send, one row per
#'   distinct name worth resolving.
#' @keywords internal
#' @noRd
tnrs_request_frame <- function(submitted) {
  keep <- !tnrs_is_blank_name(submitted$name)
  names_to_send <- unique(submitted$name[keep])

  # Integer, because the service documents its identifiers as unique integers
  # and is sent them as JSON numbers; the caller's own identifiers, whatever
  # they are, are put back by tnrs_reconcile_results()
  data.frame(
    ID = seq_along(names_to_send),
    name = names_to_send,
    stringsAsFactors = FALSE
  )
}

#' Rebuild the caller's rows from the service's answer
#'
#' Internal.  Restores one row per submitted name, in the order submitted,
#' whatever the service did with the request:
#'
#' \itemize{
#'   \item a name that was sent once but asked for several times is returned
#'     for each identifier that asked for it;
#'   \item a name held back as blank is returned as unmatched rather than
#'     silently dropped, so the identifiers still line up;
#'   \item an identifier the service pasted together is split apart again.
#' }
#'
#' @param results What the service returned.
#' @param submitted The checked two-column data.frame the caller supplied.
#' @param sent The frame from \code{tnrs_request_frame()}, whose names are
#'   distinct.  That is what lets one answer be handed to every identifier that
#'   asked for it without any of them being counted twice.
#' @return A data.frame with one row per submitted name, or more where several
#'   matches were asked for.
#' @keywords internal
#' @noRd
tnrs_reconcile_results <- function(results, submitted, sent) {
  if (is.null(results) || nrow(results) == 0) {
    return(results)
  }
  if (!"ID" %in% names(results)) {
    return(results)
  }

  # An identifier may come back as several pasted together, so each row is
  # repeated once per identifier it carries
  ids <- strsplit(as.character(results$ID), ",", fixed = TRUE)
  repeats <- lengths(ids)
  expanded <- results[rep(seq_len(nrow(results)), repeats), , drop = FALSE]
  expanded$ID <- trimws(unlist(ids))

  # Back from our identifier to the name it stood for.  The service returns
  # every column as text, so both sides are compared as text.
  name_of_row <- sent$name[match(expanded$ID, as.character(sent$ID))]

  # Rows grouped by the name they answer, so a name asked for twice can be
  # returned twice
  by_name <- split(seq_len(nrow(expanded)), name_of_row)

  wanted <- lapply(submitted$name, function(nm) {
    if (tnrs_is_blank_name(nm)) integer(0) else by_name[[nm]]
  })

  blank_row <- expanded[1, , drop = FALSE]
  blank_row[] <- NA
  columns <- names(expanded)

  pieces <- vector("list", nrow(submitted))
  for (i in seq_len(nrow(submitted))) {
    rows <- wanted[[i]]
    if (length(rows) == 0) {
      # Nothing came back for this name, because it was blank or because the
      # service dropped it.  The row is kept so the identifiers still line up.
      piece <- blank_row
      piece$ID <- submitted$ID[i]
      if ("Name_submitted" %in% columns) {
        piece$Name_submitted <- submitted$name[i]
      }
    } else {
      piece <- expanded[rows, , drop = FALSE]
      piece$ID <- submitted$ID[i]
      if ("Name_submitted" %in% columns) {
        piece$Name_submitted <- submitted$name[i]
      }
    }
    pieces[[i]] <- piece
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out
}
