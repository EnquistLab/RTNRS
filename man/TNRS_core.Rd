% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TNRS_core.R
\name{TNRS_core}
\alias{TNRS_core}
\title{Handle API access and format}
\usage{
TNRS_core(
  url = "https://tnrsapi.xyz/tnrs_api.php",
  mode = "resolve",
  data_json = NULL,
  sources = c("wcvp", "wfo"),
  classification = "wfo",
  matches = "best",
  accuracy = NULL,
  batches = NULL,
  ...
)
}
\arguments{
\item{url}{Server URL to use.  Defaults to the stable production version}

\item{mode}{Character.  Options are "resolve" and "parse". Default option is "resolve"}

\item{data_json}{Either NULL (the default) or properly formatted json}

\item{sources}{Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", "wcvp", and "cact".}

\item{classification}{Character. Family classification to use. Currently options include "wfo" (the default).}

\item{matches}{Character. Should all matches be returned ("all") or only the best match ("best", the default)?}

\item{accuracy}{numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned.}

\item{batches}{NULL or Numeric.  Optional number of batches to divide the request into for parallel processing. CUrrently isn't implemented}
}
\description{
Internal function for handling accessing the API,data formatting, and errors
}
\note{
For more information on current sources, use the function TNRS_sources()
}
\keyword{internal}
