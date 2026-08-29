context("local backbone cache and import")

# Small synthetic stand-ins for the real source files, so these tests need
# neither a download nor the network.

write_wcvp_fixture <- function(path) {
  header <- c(
    "plant_name_id", "ipni_id", "taxon_rank", "taxon_status", "family",
    "genus_hybrid", "genus", "species_hybrid", "species", "infraspecific_rank",
    "infraspecies", "parenthetical_author", "primary_author",
    "publication_author", "place_of_publication", "volume_and_page",
    "first_published", "nomenclatural_remarks", "geographic_area",
    "lifeform_description", "climate_description", "taxon_name",
    "taxon_authors", "accepted_plant_name_id", "basionym_plant_name_id",
    "replaced_synonym_author", "homotypic_synonym", "parent_plant_name_id",
    "powo_id", "hybrid_formula", "reviewed"
  )

  row <- function(id, rank, status, family, genus, species, accepted, powo,
                  name, authors, gh = "", infra_rank = "", infra = "") {
    values <- rep("", 31)
    values[c(1, 3, 4, 5, 6, 7, 9, 10, 11, 22, 23, 24, 29)] <-
      c(id, rank, status, family, gh, genus, species, infra_rank, infra,
        name, authors, accepted, powo)
    paste(values, collapse = "|")
  }

  writeLines(
    c(
      paste(header, collapse = "|"),
      row("1", "Species", "Accepted", "Fagaceae", "Quercus", "alba", "1", "urn:1",
        "Quercus alba", "L."),
      row("2", "Species", "Synonym", "Fagaceae", "Quercus", "nigra", "1", "urn:2",
        "Quercus nigra", "Wangenh."),
      row("3", "Species", "Unplaced", "Fagaceae", "Quercus", "dubia", "", "urn:3",
        "Quercus dubia", "Auct."),
      row("4", "Variety", "Accepted", "Aceraceae", "Acer", "rubrum", "4", "urn:4",
        "Acer rubrum var. rubrum", "L.", gh = "", infra_rank = "var.",
        infra = "rubrum"),
      row("5", "Species", "Accepted", "Rosaceae", "Rosa", "canina", "5", "urn:5",
        "Rosa canina", "L.", gh = "x")
    ),
    path, useBytes = TRUE
  )
}

write_wfo_fixture <- function(path) {
  header <- c(
    "taxonID", "scientificNameID", "localID", "scientificName", "taxonRank",
    "parentNameUsageID", "scientificNameAuthorship", "family", "subfamily",
    "tribe", "subtribe", "genus", "subgenus", "specificEpithet",
    "infraspecificEpithet", "verbatimTaxonRank", "nomenclaturalStatus",
    "namePublishedIn", "taxonomicStatus", "acceptedNameUsageID",
    "originalNameUsageID", "nameAccordingToID", "taxonRemarks", "created",
    "modified", "references", "source", "majorGroup", "tplID"
  )

  row <- function(id, name, rank, author, family, genus, epithet, status,
                  accepted, infra = "", refs = "") {
    values <- rep("", 29)
    values[c(1, 4, 5, 7, 8, 12, 14, 15, 19, 20, 26)] <-
      c(id, name, rank, author, family, genus, epithet, infra, status,
        accepted, refs)
    paste(values, collapse = "\t")
  }

  writeLines(
    c(
      paste(header, collapse = "\t"),
      row("wfo-1", "Quercus alba", "species", "L.", "Fagaceae", "Quercus",
        "alba", "Accepted", "wfo-1"),
      row("wfo-2", "Quercus nigra", "species", "Wangenh.", "Fagaceae", "Quercus",
        "nigra", "Synonym", "wfo-1"),
      row("wfo-3", "Acer rubrum var. rubrum", "variety", "L.", "Sapindaceae",
        "Acer", "rubrum", "Accepted", "wfo-3", infra = "rubrum"),
      row("wfo-4", "Salix form dubia", "form", "Auct.", "Salicaceae", "Salix",
        "dubia", "Unchecked", "", infra = "dubia"),
      row("wfo-5", "Rosa × hybrida", "species", "Hort.", "Rosaceae", "Rosa",
        "hybrida", "Accepted", "wfo-5", refs = "https://example.org/wfo-5")
    ),
    path, useBytes = TRUE
  )
}

test_that("the cache directory is configurable", {
  tmp <- file.path(tempdir(), "tnrs-cache-test")
  withr_option <- options(TNRS.cache_dir = tmp)
  on.exit(options(withr_option), add = TRUE)

  expect_equal(tnrs_cache_dir(), tmp)

  unlink(tmp, recursive = TRUE)
  expect_false(dir.exists(tnrs_cache_dir()))
  expect_true(dir.exists(tnrs_cache_dir(create = TRUE)))
  unlink(tmp, recursive = TRUE)
})

test_that("every registered source carries the metadata needed to cite it", {
  registry <- tnrs_source_registry()
  expect_true(all(c("wcvp", "wfo") %in% names(registry)))

  for (source in registry) {
    for (field in c(
      "source", "full_name", "version", "url", "taxonomic_scope",
      "license", "publisher"
    )) {
      expect_true(!is.null(source[[field]]) && nzchar(source[[field]]),
        info = paste(source$source, field)
      )
    }
    expect_match(source$url, "^https://")
  }

  # WFO is published with a DOI per release, so it must record one
  expect_match(registry$wfo$doi, "^10[.]5281/zenodo")
})

test_that("status and removal cope with an absent cache", {
  tmp <- file.path(tempdir(), "tnrs-cache-missing")
  unlink(tmp, recursive = TRUE)

  expect_message(result <- TNRS_local_status(dir = tmp), "No local backbone")
  expect_null(result)

  expect_message(removed <- TNRS_local_remove(dir = tmp, ask = FALSE), "Nothing to remove")
  expect_false(removed)
})

test_that("WCVP is read into the shared name table", {
  path <- file.path(tempdir(), "wcvp-fixture.csv")
  write_wcvp_fixture(path)
  on.exit(unlink(path), add = TRUE)

  names <- tnrs_import_wcvp(path, quiet = TRUE)

  expect_equal(nrow(names), 5L)
  expect_true(all(tnrs_name_columns() %in% colnames(names)))
  expect_equal(names$source, rep("wcvp", 5))
  expect_equal(names$scientific_name[1], "Quercus alba")
  expect_equal(names$authorship[1], "L.")
  expect_equal(names$name_rank[1], "species")
  expect_equal(names$genus[1], "Quercus")
  expect_equal(names$rank_indicator[4], "var.")
  expect_equal(names$infraspecific_epithet[4], "rubrum")
  # A hybrid marker in either the genus or species column flags the name
  expect_equal(names$is_hybrid, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_match(names$url[1], "powo.science.kew.org")
})

test_that("WFO is read into the shared name table", {
  path <- file.path(tempdir(), "wfo-fixture.csv")
  write_wfo_fixture(path)
  on.exit(unlink(path), add = TRUE)

  names <- tnrs_import_wfo(path, quiet = TRUE)

  expect_equal(nrow(names), 5L)
  expect_true(all(tnrs_name_columns() %in% colnames(names)))
  expect_equal(names$source, rep("wfo", 5))
  expect_equal(names$scientific_name[1], "Quercus alba")
  expect_equal(names$genus[1], "Quercus")

  # WFO spells ranks out; they become the standard indicators
  expect_equal(names$rank_indicator[1], "") # species carries none
  expect_equal(names$rank_indicator[3], "var.")
  expect_equal(names$rank_indicator[4], "fo.") # WFO spells forma as "form"

  # No hybrid flag in WFO, so it is read off the name
  expect_true(names$is_hybrid[5])
  expect_false(names$is_hybrid[1])

  # A supplied reference wins over the constructed one
  expect_equal(names$url[5], "https://example.org/wfo-5")
  expect_match(names$url[1], "worldfloraonline.org/taxon/wfo-1")
})

test_that("an unexpected column layout is refused rather than mis-mapped", {
  path <- file.path(tempdir(), "wcvp-broken.csv")
  writeLines(c("a|b|c", "1|2|3"), path)
  on.exit(unlink(path), add = TRUE)

  expect_error(tnrs_import_wcvp(path, quiet = TRUE), "column layout")
})

test_that("accepted-name links resolve to positions in the table", {
  path <- file.path(tempdir(), "wcvp-fixture2.csv")
  write_wcvp_fixture(path)
  on.exit(unlink(path), add = TRUE)

  names <- tnrs_link_accepted(tnrs_import_wcvp(path, quiet = TRUE))

  expect_true("accepted_name_id" %in% colnames(names))
  expect_false("accepted_source_name_id" %in% colnames(names))

  # The synonym points at the accepted name's row
  expect_equal(names$accepted_name_id[2], 1L)
  expect_equal(names$scientific_name[names$accepted_name_id[2]], "Quercus alba")

  # An accepted name points at itself
  expect_equal(names$accepted_name_id[1], 1L)

  # An unplaced name with no accepted name is left missing
  expect_true(is.na(names$accepted_name_id[3]))
})

test_that("WFO rank names map onto the standard indicators", {
  expect_equal(
    tnrs_wfo_rank_indicator(c("species", "variety", "subspecies", "form", "unranked")),
    c("", "var.", "subsp.", "fo.", "")
  )
})

test_that("the name table round-trips through gzipped parquet", {
  # The cache format is parquet with the gzip codec: smaller than a gzipped RDS,
  # much faster to read, and columns can be read individually.
  path <- file.path(tempdir(), "wcvp-fixture3.csv")
  write_wcvp_fixture(path)
  on.exit(unlink(path), add = TRUE)

  names <- tnrs_link_accepted(tnrs_import_wcvp(path, quiet = TRUE))

  parquet <- file.path(tempdir(), "roundtrip-names.gz.parquet")
  on.exit(unlink(parquet), add = TRUE)
  nanoparquet::write_parquet(names, parquet, compression = "gzip")

  restored <- nanoparquet::read_parquet(parquet)
  expect_equal(nrow(restored), nrow(names))
  expect_equal(colnames(restored), colnames(names))
  expect_equal(restored$scientific_name, names$scientific_name)
  expect_equal(restored$is_hybrid, names$is_hybrid)
  expect_equal(restored$accepted_name_id, names$accepted_name_id)

  # Reading a subset of columns is the point of the format
  subset <- nanoparquet::read_parquet(
    parquet,
    col_select = c("genus", "specific_epithet")
  )
  expect_equal(colnames(subset), c("genus", "specific_epithet"))
  expect_equal(subset$genus, names$genus)

  # The schema is readable without touching the data
  expect_equal(nrow(nanoparquet::read_parquet_schema(parquet)), ncol(names) + 1L)
})

test_that("loading a source that has not been built is refused", {
  tmp <- file.path(tempdir(), "tnrs-cache-unbuilt")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_error(tnrs_load_names("wcvp", dir = tmp), "has not been built")
  expect_match(tnrs_names_path("wcvp", tmp), "wcvp-names[.]gz[.]parquet$")
})

test_that("building an unknown source is refused", {
  expect_message(
    result <- TNRS_local_build(sources = "not-a-source", quiet = TRUE),
    "Unknown source"
  )
  expect_null(result)
})
