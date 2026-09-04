#' 100 scientific names
#'
#' A dataset containing scientific names for 100 taxa.  Names vary in accuracy and correctness.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{ID}{Unique integer identifying each row}
#'   \item{taxon}{Scientific name, possibly containing errors}
#'   ...
#' }
#' @source \url{https://github.com/ojalaquellueva/TNRSapi}
"tnrs_testfile"

#' Raw mosquito and tick names from GBIF, with GBIF's reading of each
#'
#' A few hundred scientific-name strings exactly as they appear on GBIF
#' occurrence records for mosquitoes (Culicidae) and ticks (Ixodida),
#' with the number of records carrying each string and how GBIF's own
#' backbone interpreted it. Chosen from downloads of 3.6 million mosquito
#' and 340,000 tick records so that every kind of agreement and
#' disagreement between GBIF and \code{TNRS_local()} is represented: exact
#' matches, misspellings, subgenera written as genera, spelling variants
#' that only the author settles, names one source lacks matched to a
#' neighbour, vague strings, barcode identifiers, and the two backbones
#' disagreeing on synonymy. It is the input for the example in
#' \code{\link{TNRS_triage}}.
#'
#' @format A data frame with one row per distinct raw string and 9
#'   variables:
#' \describe{
#'   \item{group}{"Culicidae" or "Ixodida", the taxon the records were
#'     downloaded under; the value to pass as \code{within}.}
#'   \item{verbatimScientificName}{The name string on the record, as
#'     published, casing and all.}
#'   \item{verbatimScientificNameAuthorship}{GBIF's separate authorship
#'     field for the record, the most common value where records
#'     differed; empty where none was given. Many strings carry no author
#'     of their own but do have one here.}
#'   \item{n_records}{Records carrying the string in the download.}
#'   \item{gbif_name}{The backbone taxon GBIF matched the records to,
#'     without authorship; empty where it matched nothing below the group.
#'     Where records with the same string were read differently, the
#'     majority reading.}
#'   \item{gbif_rank}{Its rank, in lower case.}
#'   \item{gbif_status}{Its status in the backbone: accepted, synonym,
#'     doubtful and so on.}
#'   \item{gbif_accepted_name}{The accepted name it resolves to, without
#'     authorship.}
#'   \item{gbif_taxon_key}{The backbone key of the matched taxon.}
#' }
#' @source GBIF occurrence downloads of 2026-09-03,
#'   \doi{10.15468/dl.32683p} (Culicidae) and \doi{10.15468/dl.qbkzk8}
#'   (Ixodida), and the GBIF Backbone Taxonomy. Occurrence data on GBIF are
#'   published under CC0 or CC BY licences; the backbone is CC0.
"gbif_triage_sample"
