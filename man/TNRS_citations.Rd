% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TNRS_citations.R
\name{TNRS_citations}
\alias{TNRS_citations}
\title{Get citation information}
\usage{
TNRS_citations()
}
\value{
Dataframe containing bibtex-formatted citation information
}
\description{
Returns information needed to cite the TNRS
}
\note{
This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
}
\examples{
{
citation_info <- TNRS_citations()
}

}
