#' Explain why a request did not reach the server
#'
#' Internal.  Every failure used to be reported as "There appears to be a
#' problem reaching the API", which cannot distinguish a problem at the server
#' from one at the caller's end.  The underlying condition says which, so it is
#' classified and passed on along with what to do about it.
#'
#' @param reason The condition message from the failed request.
#' @param url The endpoint that was tried.
#' @param timeout The number of seconds waited.
#' @return A single string to be shown to the caller.
#' @keywords internal
#' @noRd
tnrs_request_failure <- function(reason, url, timeout = NA) {
  reason <- paste(as.character(reason), collapse = " ")
  lower <- tolower(reason)

  if (grepl("timeout|timed out", lower)) {
    return(paste0(
      "The server did not answer within ", timeout, " seconds.\n",
      "  A large batch of names can legitimately take minutes. Either raise ",
      "the limit,\n  for example TNRS(names, timeout = 900), or resolve the ",
      "names offline with\n  TNRS_local(). Run TNRS_status() to see whether ",
      "the server is responding at all.\n  Reported by the connection: ", reason
    ))
  }

  if (grepl("could not resolve host|name or service not known|nodename", lower)) {
    return(paste0(
      "The address ", url, " could not be looked up.\n",
      "  This is usually a problem with your own connection or DNS rather ",
      "than the server.\n  Reported by the connection: ", reason
    ))
  }

  if (grepl("ssl|certificate", lower)) {
    return(paste0(
      "The connection to ", url, " could not be secured.\n",
      "  This is usually a certificate problem at your end; see ",
      "https://github.com/EnquistLab/RTNRS/issues/7.\n",
      "  Reported by the connection: ", reason
    ))
  }

  if (grepl("refused|could not connect|failed to connect", lower)) {
    return(paste0(
      "The server at ", url, " refused the connection.\n",
      "  The service is probably down. Run TNRS_status() to check, and see ",
      "TNRS_local()\n  for resolving names without it.\n",
      "  Reported by the connection: ", reason
    ))
  }

  paste0(
    "The request to ", url, " failed.\n",
    "  Run TNRS_status() to see whether the server is reachable.\n",
    "  Reported by the connection: ", reason
  )
}

#' Check whether the name resolution service is reachable
#'
#' Reports whether the web service is up, how quickly it answered, and which
#' version it is running.  Use it when a call fails and it is not clear whether
#' the problem is at your end or the server's, which the error alone cannot
#' always tell you.
#'
#' The check asks the server for its own version, which is the cheapest request
#' it answers, so it is quick and puts no load on the matcher.  A general
#' connectivity test is done first, because a failure there means the question
#' about the server cannot be answered either way.
#'
#' @param url Server URL to check.  Defaults to the production service.
#' @param timeout Numeric.  Seconds to wait before giving up.
#' @param quiet Suppress the printed summary?
#' @return A one-row data.frame, invisibly, with \code{internet},
#'   \code{reachable}, \code{http_status}, \code{seconds}, and the server's
#'   \code{app_version}, \code{db_version} and \code{build_date} where it
#'   answered.
#' @note If the server is down, names can still be resolved offline; see
#'   \code{\link{TNRS_local}}.
#' @seealso \code{\link{TNRS_version}} for the version alone,
#'   \code{\link{TNRS_local}} for resolving names without the server.
#' @export
#' @examples \dontrun{
#' TNRS_status()
#' }
TNRS_status <- function(url = "https://tnrsapi.xyz/tnrs_api.php",
                        timeout = 30,
                        quiet = FALSE) {
  out <- data.frame(
    url = url,
    internet = NA, reachable = NA, http_status = NA_integer_,
    seconds = NA_real_, app_version = NA_character_,
    db_version = NA_character_, build_date = NA_character_,
    stringsAsFactors = FALSE
  )

  out$internet <- check_internet()

  if (!out$internet) {
    out$reachable <- FALSE
    if (!quiet) {
      message(
        "TNRS server status\n",
        "  Internet  : no connection detected\n",
        "  Server    : cannot be checked without one\n\n",
        "Whether the service is up cannot be determined from here. If you have ",
        "built a\nlocal backbone, TNRS_local() resolves names without any ",
        "connection."
      )
    }
    return(invisible(out))
  }

  started <- Sys.time()
  response <- tryCatch(
    httr::POST(
      url = url,
      httr::add_headers("Content-Type" = "application/json"),
      httr::add_headers("Accept" = "application/json"),
      body = '{"opts":{"mode":"meta"}}',
      encode = "json",
      httr::timeout(timeout)
    ),
    error = function(e) conditionMessage(e)
  )
  out$seconds <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)

  if (is.character(response)) {
    out$reachable <- FALSE
    if (!quiet) {
      message(
        "TNRS server status\n",
        "  Internet  : connected\n",
        "  Server    : ", url, "\n",
        "  Reachable : no, after ", out$seconds, " seconds\n\n",
        tnrs_request_failure(response, url, timeout), "\n\n",
        "Names can be resolved offline meanwhile; see ?TNRS_local_build."
      )
    }
    return(invisible(out))
  }

  out$http_status <- response$status_code
  out$reachable <- identical(response$status_code, 200L)

  if (out$reachable) {
    meta <- tryCatch(
      jsonlite::fromJSON(rawToChar(response$content)),
      error = function(e) NULL
    )
    if (is.data.frame(meta) && nrow(meta) >= 1) {
      for (field in c("app_version", "db_version", "build_date")) {
        if (field %in% names(meta)) out[[field]] <- as.character(meta[[field]][1])
      }
    }
  }

  if (!quiet) {
    if (out$reachable) {
      message(
        "TNRS server status\n",
        "  Internet  : connected\n",
        "  Server    : ", url, "\n",
        "  Reachable : yes, HTTP 200 in ", out$seconds, " seconds\n",
        "  Version   : app ", out$app_version, ", database ", out$db_version,
        ", built ", out$build_date, "\n\n",
        "The service is answering. A request that still fails is more likely ",
        "to be too\nlarge or too slow than refused: try a smaller batch, or ",
        "raise the wait with\nTNRS(names, timeout = 900)."
      )
    } else {
      message(
        "TNRS server status\n",
        "  Internet  : connected\n",
        "  Server    : ", url, "\n",
        "  Reachable : answered with HTTP ", out$http_status, " after ",
        out$seconds, " seconds\n\n",
        "The server is up but refused this request, so the problem is at its ",
        "end rather\nthan yours. Names can be resolved offline meanwhile; see ",
        "?TNRS_local_build."
      )
    }
  }

  invisible(out)
}
