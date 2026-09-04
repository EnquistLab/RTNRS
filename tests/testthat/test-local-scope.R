context("confining a search to a taxon")

# A checklist spanning kingdoms, with the homonyms and lookalikes that make
# an all-life source dangerous: Oenanthe is a plant genus and a bird genus,
# and Ixos (a bird) is one edit from Ixodes (a tick).  The tick synonym
# carries no classification of its own, as Catalogue of Life synonyms do not.
# The tick order and family are present as names in their own right, and as
# in the Catalogue of Life each carries its classification only above itself.
mixed_checklist <- function() {
  data.frame(
    taxonID = c("p1", "p2", "b1", "b2", "t0", "tf", "t1", "t2", "t3", "x1", "o1"),
    scientificName = c(
      "Oenanthe", "Oenanthe aquatica",
      "Oenanthe", "Oenanthe oenanthe",
      "Ixodida", "Ixodidae",
      "Ixodes", "Ixodes ricinus", "Ixodes reduvius",
      "Ixos inornatus",
      # A two-letter orchid genus, which any two-letter fragment reaches at 0.5
      "Aa"
    ),
    taxonRank = c(
      "genus", "species", "genus", "species", "order", "family",
      "genus", "species", "species", "species", "genus"
    ),
    taxonomicStatus = c(
      "accepted", "accepted", "accepted", "accepted", "accepted", "accepted",
      "accepted", "accepted", "synonym", "accepted", "accepted"
    ),
    acceptedNameUsageID = c("p1", "p2", "b1", "b2", "t0", "tf", "t1", "t2", "t2", "x1", "o1"),
    kingdom = c("Plantae", "Plantae", "Animalia", "Animalia", "Animalia", "Animalia", "Animalia", "Animalia", "", "Animalia", "Plantae"),
    phylum = c("Tracheophyta", "Tracheophyta", "Chordata", "Chordata", "Arthropoda", "Arthropoda", "Arthropoda", "Arthropoda", "", "Chordata", "Tracheophyta"),
    class = c("Magnoliopsida", "Magnoliopsida", "Aves", "Aves", "Arachnida", "Arachnida", "Arachnida", "Arachnida", "", "Aves", "Liliopsida"),
    order = c("Apiales", "Apiales", "Passeriformes", "Passeriformes", "", "Ixodida", "Ixodida", "Ixodida", "", "Passeriformes", "Asparagales"),
    family = c("Apiaceae", "Apiaceae", "Muscicapidae", "Muscicapidae", "", "", "Ixodidae", "Ixodidae", "", "Pycnonotidae", "Orchidaceae"),
    genus = c("Oenanthe", "Oenanthe", "Oenanthe", "Oenanthe", "", "", "Ixodes", "Ixodes", "Ixodes", "Ixos", "Aa"),
    specificEpithet = c("", "aquatica", "", "oenanthe", "", "", "", "ricinus", "reduvius", "inornatus", ""),
    stringsAsFactors = FALSE
  )
}

scoped_dir <- function(name) {
  tmp <- file.path(tempdir(), name)
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  suppressMessages(
    TNRS_local_add_source(mixed_checklist(),
      source = "mixed", version = "1", nomenclature = "mixed",
      dir = tmp, quiet = TRUE
    )
  )
  tmp
}

test_that("a synonym inherits the classification of its accepted name", {
  tmp <- scoped_dir("tnrs-scope-inherit")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  names <- tnrs_load_names("mixed", tmp)
  syn <- names[names$scientific_name == "Ixodes reduvius", ]
  expect_identical(syn$family, "Ixodidae")
  expect_identical(syn$order, "Ixodida")
  expect_identical(syn$kingdom, "Animalia")

  # An accepted name keeps its own
  bird <- names[names$scientific_name == "Oenanthe oenanthe", ]
  expect_identical(bird$family, "Muscicapidae")

  # Every classification column is present whether or not it was supplied
  expect_true(all(tnrs_classification_ranks() %in% colnames(names)))
})

test_that("inheritance adds absent columns and leaves unlinked rows empty", {
  names <- data.frame(
    scientific_name = c("Aus", "Bus"),
    family = c("Aidae", ""),
    accepted_name_id = c(1L, NA),
    stringsAsFactors = FALSE
  )
  out <- tnrs_inherit_classification(names)
  expect_identical(out$family, c("Aidae", ""))
  expect_identical(out$order, c("", ""))
  expect_true(all(tnrs_classification_ranks() %in% colnames(out)))
})

test_that("the scope mask picks rows through any rank and through inheritance", {
  tmp <- scoped_dir("tnrs-scope-mask")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  backbone <- tnrs_backbone("mixed", dir = tmp, quiet = TRUE)[["mixed"]]
  names <- backbone$names

  ticks <- tnrs_scope_mask(backbone, "Ixodida")
  # The order's own row and its family are in scope too, though neither
  # carries "Ixodida" in a classification column
  expect_setequal(
    names$scientific_name[ticks$row],
    c("Ixodida", "Ixodidae", "Ixodes", "Ixodes ricinus", "Ixodes reduvius")
  )
  expect_identical(ticks$found, "Ixodida")

  # Scoped to the family, the order above it is out but the family row is in
  family <- tnrs_scope_mask(backbone, "Ixodidae")
  expect_setequal(
    names$scientific_name[family$row],
    c("Ixodidae", "Ixodes", "Ixodes ricinus", "Ixodes reduvius")
  )
  # Ixos is a bird, so its genus is out even though it is one edit away
  expect_false(ticks$genus[backbone$index$genus$name == "Ixos"])
  expect_true(ticks$genus[backbone$index$genus$name == "Ixodes"])

  # Case does not matter, and several taxa can be named at different ranks
  both <- tnrs_scope_mask(backbone, c("aves", "APIACEAE"))
  expect_setequal(both$found, c("aves", "APIACEAE"))
  expect_setequal(
    names$scientific_name[both$row],
    c("Oenanthe", "Oenanthe aquatica", "Oenanthe oenanthe", "Ixos inornatus")
  )

  # A genus is not a scope
  none <- tnrs_scope_mask(backbone, "Ixodes")
  expect_length(none$found, 0)
  expect_false(any(none$row))
})

test_that("the scope taxon itself still resolves", {
  tmp <- scoped_dir("tnrs-scope-self")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # A record identified only to the order is common in occurrence data, and
  # "Ixodida" submitted within Ixodida must not come back unmatched
  result <- TNRS_local(c("Ixodida", "Ixodidae", "Ixodes ricinus"),
    sources = "mixed", within = "Ixodida", dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(result$Name_matched, c("Ixodida", "Ixodidae", "Ixodes ricinus"))
  expect_identical(result$Name_matched_rank, c("order", "family", "species"))
})

test_that("the default accuracy drops a match poor in every part and keeps a genus", {
  tmp <- scoped_dir("tnrs-scope-accuracy")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # "Ab" reaches the orchid genus Aa by the fuzzy genus step alone, one edit
  # on a two-letter name: genus score 0.5, overall 0.5, nothing else scored.
  # This is the shape of "A. (A.) intermedius" -> "Aa" in real GBIF data.
  kept <- TNRS_local("Ab",
    sources = "mixed", dir = tmp, accuracy = NULL,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(kept$Name_matched, "Aa")
  expect_equal(kept$Overall_score, 0.5)
  expect_equal(kept$Genus_score, 0.5)

  dropped <- TNRS_local("Ab",
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(dropped$Name_matched, "[No match found]")
  expect_true(is.na(dropped$Overall_score))

  # A genus-only fallback scores 0.5 overall but 1 on the genus, and the web
  # service's rule keeps it
  partial <- TNRS_local("Ixodes nonexistens",
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(partial$Name_matched, "Ixodes")
  expect_equal(partial$Overall_score, 0.5)
  expect_equal(partial$Genus_score, 1)

  # A bare genus, exact or misspelt, always clears the bar on its genus score
  genus <- TNRS_local(c("Ixodes", "Ixodez"),
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(genus$Name_matched, c("Ixodes", "Ixodes"))

  # 0 means keep everything, as it does for the web service
  expect_identical(
    TNRS_local("Ab", sources = "mixed", dir = tmp, accuracy = 0,
      build_missing = FALSE, quiet = TRUE)$Name_matched,
    "Aa"
  )
  expect_error(TNRS_local("Ab", sources = "mixed", dir = tmp, accuracy = 2), "accuracy")
})

test_that("a fuzzy match whose author disagrees is flagged, an exact one is not", {
  tmp <- file.path(tempdir(), "tnrs-author-flag")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ticks <- data.frame(
    taxonID = c("g", "s1", "s2"),
    scientificName = c("Ixodes", "Ixodes bakeri", "Ixodes ricinus"),
    scientificNameAuthorship = c("Latreille, 1795", "Arthur & Clifford, 1961", "(Linnaeus, 1758)"),
    taxonRank = c("genus", "species", "species"),
    taxonomicStatus = "accepted",
    acceptedNameUsageID = c("g", "s1", "s2"),
    family = "Ixodidae", genus = "Ixodes",
    specificEpithet = c("", "bakeri", "ricinus"),
    stringsAsFactors = FALSE
  )
  suppressMessages(TNRS_local_add_source(ticks,
    source = "ticks", version = "1", nomenclature = "zoological",
    dir = tmp, quiet = TRUE
  ))

  result <- TNRS_local(
    c(
      # A species the source lacks, matched to its neighbour; the author is
      # what gives it away
      "Ixodes barkeri Barker 2019",
      # The same without an author: nothing to contradict, so no flag
      "Ixodes barkeri",
      # An exact name with a later combination's author: sources cite these
      # routinely, so no flag
      "Ixodes ricinus Neumann, 1911",
      # Fuzzy, but the author agrees
      "Ixodes bakeri Arthur & Clifford 1961"
    ),
    sources = "ticks", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(result$Name_matched, c("Ixodes bakeri", "Ixodes bakeri", "Ixodes ricinus", "Ixodes bakeri"))
  flagged <- bitwAnd(result$Warnings, 16L) > 0L
  expect_identical(flagged, c(TRUE, FALSE, FALSE, FALSE))
  expect_match(result$WarningsEng[1], "[Author]", fixed = TRUE)
  expect_false(grepl("Author", result$WarningsEng[3], fixed = TRUE))
})

test_that("a leading higher taxon the source knows confines the name it opens", {
  tmp <- scoped_dir("tnrs-scope-prefix")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- TNRS_local(
    c(
      # order + species: the order is taken off and the species found
      "Ixodida Ixodes ricinus",
      # kingdom + homonym genus: the prefix decides which Oenanthe
      "Plantae Oenanthe", "Aves Oenanthe",
      # a prefix that rules the name out: the tick is not a bird
      "Passeriformes Ixodes ricinus",
      # a family the source knows, written under the botanical code: family path
      "Ixodidae Ixodes ricinus",
      # the order alone is a name to match, not a prefix
      "Ixodida"
    ),
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(result$Name_matched, c(
    "Ixodes ricinus", "Oenanthe", "Oenanthe", "[No match found]", "Ixodes ricinus", "Ixodida"
  ))
  expect_identical(result$Name_matched_accepted_family[2:3], c("Apiaceae", "Muscicapidae"))
  expect_identical(result$Family_submitted[5], "Ixodidae")
  expect_equal(result$Overall_score[c(1, 5)], c(1, 1))
  # The prefix was used, so it is not reported as an unmatched term
  expect_identical(result$Unmatched_terms[1], "")

  # The pre-processor reports what it took off
  pre <- tnrs_preprocess(c("Ixodida Ixodes ricinus", "Ixodida", "Fagales Quercus alba"),
    codes = "mixed", higher = c("IXODIDA"), families = "IXODIDAE"
  )
  expect_identical(pre$cleaned, c("Ixodes ricinus", "Ixodida", "Fagales Quercus alba"))
  expect_identical(pre$higher, c("Ixodida", "", ""))
})

test_that("a homonym genus resolves to the kingdom asked for", {
  tmp <- scoped_dir("tnrs-scope-homonym")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  bird <- TNRS_local("Oenanthe",
    sources = "mixed", within = "Aves", dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(bird$Name_matched, "Oenanthe")
  expect_identical(bird$Name_matched_accepted_family, "Muscicapidae")

  plant <- TNRS_local("Oenanthe",
    sources = "mixed", within = "Plantae", dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(plant$Name_matched_accepted_family, "Apiaceae")

  # The species too: an exact hit outside the scope is not an answer
  expect_identical(
    TNRS_local("Oenanthe oenanthe",
      sources = "mixed", within = "Apiaceae", dir = tmp,
      build_missing = FALSE, quiet = TRUE
    )$Name_matched,
    "Oenanthe"
  )
})

test_that("a lookalike in another kingdom is never reached", {
  tmp <- scoped_dir("tnrs-scope-lookalike")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Unscoped, the tick name CoL lacks drifts to the bird
  free <- TNRS_local("Ixodes inopinatus",
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(free$Name_matched, "Ixos inornatus")

  scoped <- TNRS_local("Ixodes inopinatus",
    sources = "mixed", within = "Ixodidae", dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(scoped$Name_matched, "Ixodes")
  expect_identical(scoped$Name_matched_rank, "genus")

  # A synonym is in scope through the classification it inherited
  syn <- TNRS_local("Ixodes reduvius",
    sources = "mixed", within = "Ixodida", dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )
  expect_identical(syn$Name_matched, "Ixodes reduvius")
  expect_identical(syn$Accepted_name, "Ixodes ricinus")
})

test_that("a scope no source knows is reported rather than silently empty", {
  tmp <- scoped_dir("tnrs-scope-unknown")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_message(
    result <- TNRS_local("Ixodes ricinus",
      sources = "mixed", within = "Culicidae", dir = tmp,
      build_missing = FALSE, quiet = TRUE
    ),
    "not a taxon at family rank or above"
  )
  expect_null(result)

  # A genus in particular is refused, with the reason
  expect_message(
    TNRS_local("Ixodes ricinus",
      sources = "mixed", within = "Ixodes", dir = tmp,
      build_missing = FALSE, quiet = TRUE
    ),
    "genus"
  )

  expect_error(
    TNRS_local("Ixodes ricinus", sources = "mixed", within = "", dir = tmp),
    "within"
  )
})

test_that("a source built without the classification asks to be rebuilt", {
  tmp <- scoped_dir("tnrs-scope-old")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Strip the columns, as a name table from an earlier version lacks them
  path <- tnrs_names_path("mixed", tmp)
  old <- nanoparquet::read_parquet(path)
  old <- old[, setdiff(colnames(old), c("kingdom", "phylum", "class", "order"))]
  nanoparquet::write_parquet(old, path, compression = "gzip")
  tnrs_backbone_forget()

  expect_error(
    TNRS_local("Ixodes ricinus",
      sources = "mixed", within = "Ixodida", dir = tmp,
      build_missing = FALSE, quiet = TRUE
    ),
    "earlier version"
  )

  # Without a scope the old table still works
  result <- TNRS_local("Ixodes ricinus",
    sources = "mixed", dir = tmp, build_missing = FALSE, quiet = TRUE
  )
  expect_identical(result$Name_matched, "Ixodes ricinus")
})
