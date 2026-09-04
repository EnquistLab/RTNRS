context("animal sources")

mdd_species_fixture <- function(path) {
  utils::write.csv(
    data.frame(
      sciName = c("Panthera_leo", "Ornithorhynchus_anatinus"),
      id = c("1001", "1002"),
      family = c("FELIDAE", "Ornithorhynchidae"),
      genus = c("Panthera", "Ornithorhynchus"),
      specificEpithet = c("leo", "anatinus"),
      authoritySpeciesAuthor = c("Linnaeus", "G. K. Shaw"),
      authoritySpeciesYear = c("1758", "1799"),
      authorityParentheses = c("1", "1"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

mdd_synonym_fixture <- function(path) {
  utils::write.csv(
    data.frame(
      MDD_syn_ID = c("1", "2", "3", "4", "5"),
      MDD_species = c(
        "Panthera_leo", "Panthera_leo", "Panthera_leo", "Panthera_leo",
        "Ornithorhynchus_anatinus"
      ),
      MDD_normalized_original_combination = c(
        # The accepted taxon under the combination it was published in
        "Felis leo",
        "Tigris leo",
        "Felis leo persica",
        "Leo barbarus",
        # This one matches the current name, so it is redundant
        "Ornithorhynchus anatinus"
      ),
      MDD_validity = c("species", "synonym", "synonym", "nomen_dubium", "species"),
      # These describe the accepted taxon, not the name on the row
      MDD_genus = "Panthera",
      MDD_specificEpithet = "leo",
      MDD_subspecificEpithet = c("leo", "", "", "", ""),
      MDD_family = "Felidae",
      MDD_author = c("Linnaeus", "Cuvier", "Meyer", "Meyer", "Shaw"),
      MDD_year = c("1758", "1829", "1826", "1826", "1799"),
      MDD_authority_parentheses = c("0", "0", "0", "0", "0"),
      MDD_species_id = c("1001", "1001", "1001", "1001", "1002"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

import_fixture <- function() {
  dir <- file.path(tempdir(), "tnrs-mdd-fixture")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  sp <- file.path(dir, "species.csv")
  sy <- file.path(dir, "syn.csv")
  mdd_species_fixture(sp)
  mdd_synonym_fixture(sy)
  tnrs_import_mdd(sp, sy, quiet = TRUE)
}

test_that("a zoological citation is put back together from its parts", {
  expect_identical(
    tnrs_zoological_author("Linnaeus", "1758", "1"), "(Linnaeus, 1758)"
  )
  expect_identical(
    tnrs_zoological_author("Linnaeus", "1758", "0"), "Linnaeus, 1758"
  )
  expect_identical(tnrs_zoological_author("Gray", "", "0"), "Gray")
  expect_identical(tnrs_zoological_author(NA, NA, "1"), "")
})

test_that("the database's own validity terms map onto the resolver's", {
  expect_identical(tnrs_mdd_status(c("species", "synonym")), c("Accepted", "Synonym"))
  # A doubtful name is not a synonym: it points at no accepted name
  expect_identical(
    tnrs_mdd_status(c("nomen_dubium", "species_inquirenda")),
    c("Unchecked", "Unchecked")
  )
  expect_identical(tnrs_mdd_status("unavailable"), "Invalid")
  expect_identical(tnrs_mdd_status("something_new"), "Unchecked")
})

test_that("a synonym is indexed under the genus it is spelled with", {
  names <- import_fixture()

  # The source's genus column names the accepted taxon, so reading it would
  # index "Tigris leo" under Panthera and make it unfindable
  tigris <- names[names$scientific_name == "Tigris leo", ]
  expect_identical(tigris$genus, "Tigris")
  expect_identical(tigris$specific_epithet, "leo")

  felis <- names[names$scientific_name == "Felis leo persica", ]
  expect_identical(felis$genus, "Felis")
  expect_identical(felis$infraspecific_epithet, "persica")
  expect_identical(felis$name_rank, "subspecies")
  # Zoology writes no connector, so there is no indicator to record
  expect_identical(felis$rank_indicator, "")
})

test_that("the accepted name keeps its current combination", {
  names <- import_fixture()

  accepted <- names[names$taxonomic_status == "Accepted", ]
  expect_setequal(
    accepted$scientific_name, c("Panthera leo", "Ornithorhynchus anatinus")
  )
  expect_identical(accepted$authorship[accepted$scientific_name == "Panthera leo"],
    "(Linnaeus, 1758)"
  )

  # The original combination is kept as a synonym, since it is a name people
  # actually submit
  felis <- names[names$scientific_name == "Felis leo", ]
  expect_identical(felis$taxonomic_status, "Synonym")
  expect_identical(felis$accepted_source_name_id, "1001")

  # But one that merely repeats the current name is not duplicated
  expect_identical(sum(names$scientific_name == "Ornithorhynchus anatinus"), 1L)
})

test_that("a doubtful name is given no accepted name to point at", {
  names <- import_fixture()
  dubious <- names[names$scientific_name == "Leo barbarus", ]
  expect_identical(dubious$taxonomic_status, "Unchecked")
  expect_identical(dubious$accepted_source_name_id, "")
})

test_that("families are reported capitalised whatever the source does", {
  names <- import_fixture()
  expect_identical(unique(names$family[names$genus == "Panthera"]), "Felidae")
})

test_that("an animal source resolves zoological names end to end", {
  tmp <- file.path(tempdir(), "tnrs-mdd-resolve")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  names <- tnrs_link_accepted(import_fixture())
  nanoparquet::write_parquet(names, tnrs_names_path("mdd", tmp), compression = "gzip")
  saveRDS(
    list(
      source = "mdd", full_name = "Mammal Diversity Database", version = "v2.3",
      doi = NA_character_, downloaded = "2026-09-01", bytes = 1,
      archive = NA_character_, nomenclature = "zoological"
    ),
    tnrs_provenance_path("mdd", tmp)
  )

  result <- TNRS_local(
    c("Panthera leo", "Felis leo", "Felidae Panthera leo", "Felis leo persica"),
    sources = "mdd", dir = tmp, build_missing = FALSE, quiet = TRUE
  )

  expect_identical(result$Overall_score[1], 1)
  # A synonym resolves to the accepted name
  expect_identical(result$Taxonomic_status[2], "Synonym")
  expect_identical(result$Accepted_name[2], "Panthera leo")
  # The family prefix is stripped, because the source is zoological, and does
  # not cost the match anything
  expect_identical(result$Family_submitted[3], "Felidae")
  expect_identical(result$Overall_score[3], 1)
  # A trinomial carrying no rank connector still reaches the subspecies
  expect_identical(result$Name_matched[4], "Felis leo persica")
})

test_that("the Catalogue of Life core is checked before it is read", {
  tmp <- file.path(tempdir(), "tnrs-col")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # A header naming the wrong things should say so rather than mis-map columns
  bad <- file.path(tmp, "bad.tsv")
  writeLines(c("a\tb\tc", "1\t2\t3"), bad)
  expect_error(tnrs_import_col(bad, quiet = TRUE), "missing the column", fixed = TRUE)

  # Darwin Core terms are matched on their local part, so a prefixed or fully
  # qualified header reads the same as a bare one
  expect_identical(
    tnrs_dwc_term(c("taxonID", "dwc:taxonID",
                    "http://rs.tdwg.org/dwc/terms/taxonID")),
    rep("taxonID", 3)
  )

  good <- file.path(tmp, "good.tsv")
  writeLines(
    c(
      paste(c("dwc:taxonID", "dwc:scientificName", "dwc:scientificNameAuthorship",
              "dwc:taxonRank", "dwc:taxonomicStatus", "dwc:acceptedNameUsageID",
              "dwc:kingdom", "dwc:genus", "dwc:specificEpithet"), collapse = "\t"),
      paste(c("1", "Panthera leo", "(Linnaeus, 1758)", "species", "accepted",
              "1", "Animalia", "Panthera", "leo"), collapse = "\t")
    ),
    good
  )
  out <- tnrs_import_col(good, quiet = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$scientific_name, "Panthera leo")
  expect_identical(out$taxonomic_status, "Accepted")
  # An animal name carries no rank connector even at subspecies
  expect_identical(out$rank_indicator, "")
})

test_that("a Catalogue of Life synonym keeps the genus it is spelled with", {
  tmp <- file.path(tempdir(), "tnrs-col-generic")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # genericName is the generic part of the name itself; genus is the accepted
  # taxon's, and the real archive leaves it empty on every synonym row
  path <- file.path(tmp, "taxon.tsv")
  writeLines(
    c(
      paste(c("dwc:taxonID", "dwc:scientificName", "dwc:taxonRank",
              "dwc:taxonomicStatus", "dwc:acceptedNameUsageID",
              "dwc:genericName", "dwc:genus", "dwc:specificEpithet",
              "dwc:nomenclaturalCode", "dwc:kingdom"), collapse = "\t"),
      paste(c("1", "Panthera leo", "species", "accepted", "1",
              "Panthera", "Panthera", "leo", "ICZN", "Animalia"), collapse = "\t"),
      paste(c("2", "Felis leo", "species", "synonym", "1",
              "Felis", "", "leo", "ICZN", "Animalia"), collapse = "\t"),
      paste(c("3", "Quercus alba var. repanda", "variety", "accepted", "3",
              "Quercus", "Quercus", "alba", "ICN", "Plantae"), collapse = "\t")
    ),
    path
  )

  out <- tnrs_import_col(path, quiet = TRUE)

  synonym <- out[out$scientific_name == "Felis leo", ]
  expect_identical(synonym$genus, "Felis")
  expect_identical(synonym$taxonomic_status, "Synonym")

  # A botanical name keeps its connector, a zoological one has none
  expect_identical(out$rank_indicator[out$scientific_name == "Quercus alba var. repanda"], "var.")
  expect_identical(out$rank_indicator[out$scientific_name == "Panthera leo"], "")
})

test_that("the archive's stated code decides the connector, not the kingdom", {
  tmp <- file.path(tempdir(), "tnrs-col-code")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # No nomenclaturalCode column at all: the kingdom is the fallback
  path <- file.path(tmp, "nocode.tsv")
  writeLines(
    c(
      paste(c("taxonID", "scientificName", "taxonRank", "taxonomicStatus",
              "acceptedNameUsageID", "scientificNameAuthorship",
              "genericName", "specificEpithet", "kingdom"), collapse = "\t"),
      paste(c("1", "Panthera leo persica", "subspecies", "accepted", "1",
              "Meyer, 1826", "Panthera", "leo", "Animalia"), collapse = "\t"),
      paste(c("2", "Quercus alba var. repanda", "variety", "accepted", "2",
              "Michx.", "Quercus", "alba", "Plantae"), collapse = "\t")
    ),
    path
  )

  out <- tnrs_import_col(path, quiet = TRUE)
  expect_identical(out$rank_indicator[out$scientific_name == "Panthera leo persica"], "")
  expect_identical(out$rank_indicator[out$scientific_name == "Quercus alba var. repanda"], "var.")
})

test_that("a core with no generic column at all is refused", {
  tmp <- file.path(tempdir(), "tnrs-col-nogenus")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Either term will do, and the importer falls back between them, but without
  # one every name would import with no genus and never be found
  path <- file.path(tmp, "nogenus.tsv")
  writeLines(
    c(
      paste(c("taxonID", "scientificName", "taxonRank", "taxonomicStatus",
              "acceptedNameUsageID"), collapse = "\t"),
      paste(c("1", "Panthera leo", "species", "accepted", "1"), collapse = "\t")
    ),
    path
  )
  expect_error(
    tnrs_import_col(path, quiet = TRUE), "genericName or genus", fixed = TRUE
  )
})

test_that("an author written into the name is separated back out", {
  tmp <- file.path(tempdir(), "tnrs-col-author")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # The Catalogue of Life writes the authority into scientificName as well as
  # giving it separately.  Leaving it there would report a different name than
  # WFO does for the same taxon, and would stop a bare name matching exactly.
  path <- file.path(tmp, "authors.tsv")
  writeLines(
    c(
      paste(c("taxonID", "scientificName", "scientificNameAuthorship",
              "taxonRank", "taxonomicStatus", "acceptedNameUsageID",
              "genericName", "specificEpithet"), collapse = "\t"),
      paste(c("1", "Panthera leo (Linnaeus, 1758)", "(Linnaeus, 1758)",
              "species", "accepted", "1", "Panthera", "leo"), collapse = "\t"),
      paste(c("2", "Quercus alba L.", "L.", "species", "accepted", "2",
              "Quercus", "alba"), collapse = "\t"),
      # A name that does not end with its author is left alone
      paste(c("3", "Bacillus subtilis", "Cohn 1872", "species", "accepted", "3",
              "Bacillus", "subtilis"), collapse = "\t")
    ),
    path
  )

  out <- tnrs_import_col(path, quiet = TRUE)
  expect_identical(
    out$scientific_name, c("Panthera leo", "Quercus alba", "Bacillus subtilis")
  )
  expect_identical(out$authorship, c("(Linnaeus, 1758)", "L.", "Cohn 1872"))
  expect_false(any(endsWith(out$scientific_name, out$authorship)))
})

phylacine_fixture <- function(path) {
  gone <- "000 Species not accepted"
  utils::write.csv(
    data.frame(
      Binomial.1.2 = c("Vulpes_lagopus", "Alces_alces", "Panthera_leo", gone),
      Order.1.2 = c("Carnivora", "Artiodactyla", "Carnivora", gone),
      Family.1.2 = c("Canidae", "Cervidae", "Felidae", gone),
      Genus.1.2 = c("Vulpes", "Alces", "Panthera", gone),
      Species.1.2 = c("lagopus", "alces", "leo", gone),
      # 1.1 wrote the fox under its old genus; 1.0 split the moose
      Genus.1.1 = c("Alopex", "Alces", "Panthera", gone),
      Species.1.1 = c("lagopus", "alces", "leo", gone),
      Genus.1.0 = c("Alopex", "Alces", "Panthera", gone),
      Species.1.0 = c("lagopus", "americanus", "leo", gone),
      # EltonTraits has a name PHYLACINE rejects, and lacks the lion
      EltonTraits.1.0.Genus = c("Alopex", "Alces", "", "Cavia"),
      EltonTraits.1.0.Species = c("lagopus", "alces", "", "porcellus"),
      IUCN.2016.3.Genus = c("Vulpes", "Alces", "Panthera", gone),
      IUCN.2016.3.Species = c("lagopus", "alces", "leo", gone),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

test_that("the PHYLACINE table imports as accepted names, crosswalk synonyms and rejects", {
  path <- file.path(tempdir(), "phylacine-fixture.csv")
  phylacine_fixture(path)
  on.exit(unlink(path), add = TRUE)

  names <- tnrs_import_phylacine(path, quiet = TRUE)
  expect_setequal(names(names), tnrs_name_columns())

  accepted <- names[names$taxonomic_status == "Accepted", ]
  expect_identical(accepted$scientific_name, c("Vulpes lagopus", "Alces alces", "Panthera leo"))
  expect_identical(accepted$order, c("Carnivora", "Artiodactyla", "Carnivora"))
  expect_true(all(names$class == "Mammalia"))

  # Each differing alternative once, pointing at the PHYLACINE name
  synonyms <- names[names$taxonomic_status == "Synonym", ]
  expect_setequal(synonyms$scientific_name, c("Alopex lagopus", "Alces americanus"))
  expect_identical(
    synonyms$accepted_source_name_id[synonyms$scientific_name == "Alopex lagopus"],
    "Vulpes_lagopus"
  )
  expect_identical(synonyms$genus[synonyms$scientific_name == "Alopex lagopus"], "Alopex")

  rejected <- names[names$taxonomic_status == "Unplaced", ]
  expect_identical(rejected$scientific_name, "Cavia porcellus")
  expect_identical(rejected$accepted_source_name_id, "")

  # Linked and inherited as the build does it, a synonym carries the
  # accepted name's family and order
  linked <- tnrs_inherit_classification(tnrs_link_accepted(names))
  fox <- linked[linked$scientific_name == "Alopex lagopus", ]
  expect_identical(fox$family, "Canidae")
  expect_identical(fox$order, "Carnivora")
  expect_identical(linked$scientific_name[fox$accepted_name_id], "Vulpes lagopus")
})

test_that("a changed PHYLACINE layout is refused with the missing column named", {
  path <- file.path(tempdir(), "phylacine-bad.csv")
  utils::write.csv(data.frame(Binomial.1.2 = "Vulpes_lagopus"), path, row.names = FALSE)
  on.exit(unlink(path), add = TRUE)
  expect_error(tnrs_import_phylacine(path, quiet = TRUE), "Order.1.2")
})
