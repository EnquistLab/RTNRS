context("nomenclatural codes")

test_that("a mixed source covers both codes", {
  expect_identical(tnrs_nomenclature_codes("botanical"), "botanical")
  expect_identical(tnrs_nomenclature_codes("zoological"), "zoological")
  expect_setequal(tnrs_nomenclature_codes("mixed"), c("botanical", "zoological"))
  # Asking for a plant and an animal source resolves under both
  expect_setequal(
    tnrs_nomenclature_codes(c("botanical", "zoological")),
    c("botanical", "zoological")
  )
  expect_identical(tnrs_nomenclature_codes(character(0)), "botanical")
  expect_error(tnrs_nomenclature_codes("cladistic"), "Unknown nomenclature", fixed = TRUE)
})

test_that("the family pattern follows the code in play", {
  botanical <- tnrs_family_pattern("botanical")
  expect_true(grepl("aceae", botanical, fixed = TRUE))
  expect_false(grepl("idae", botanical, fixed = TRUE))

  zoological <- tnrs_family_pattern("zoological")
  expect_true(grepl("idae", zoological, fixed = TRUE))

  both <- tnrs_family_pattern(c("botanical", "zoological"))
  expect_true(grepl("aceae", both, fixed = TRUE))
  expect_true(grepl("idae", both, fixed = TRUE))
})

test_that("a zoological family prefix is stripped only when asked for", {
  # Under the botanical default an animal family is left in place.  That is
  # what stops a bird name reaching a plant genus it merely resembles, so it
  # is deliberate rather than an oversight.
  botanical <- tnrs_preprocess("Emberizidae Dendroica palmarum")
  expect_identical(botanical$family, "")
  expect_identical(botanical$cleaned, "Emberizidae Dendroica palmarum")

  zoological <- tnrs_preprocess("Felidae Panthera leo", codes = "zoological")
  expect_identical(zoological$family, "Felidae")
  expect_identical(zoological$cleaned, "Panthera leo")

  # A botanical family still works when both codes are in play
  both <- tnrs_preprocess(
    c("Fagaceae Quercus alba", "Felidae Panthera leo"),
    codes = c("botanical", "zoological")
  )
  expect_identical(both$family, c("Fagaceae", "Felidae"))
})

test_that("the code is taken from the sources unless overridden", {
  tmp <- file.path(tempdir(), "tnrs-nom-src")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(
      data.frame(
        taxonID = "1", scientificName = "Panthera leo",
        taxonomicStatus = "accepted", acceptedNameUsageID = "1",
        stringsAsFactors = FALSE
      ),
      source = "zoo", version = "1", nomenclature = "zoological",
      dir = tmp, quiet = TRUE
    )
  )

  expect_identical(unname(tnrs_source_nomenclature("zoo", tmp)), "zoological")
  expect_identical(tnrs_effective_codes("zoo", NULL, dir = tmp), "zoological")
  # An explicit setting wins over the source
  expect_identical(tnrs_effective_codes("zoo", "botanical", dir = tmp), "botanical")

  # A source registered without saying is botanical, which keeps every cache
  # built before this setting existed working unchanged
  expect_identical(unname(tnrs_source_nomenclature("wfo", tmp)), "botanical")

  status <- suppressMessages(TNRS_local_status(dir = tmp))
  expect_identical(status$nomenclature[status$source == "zoo"], "zoological")
  expect_identical(status$nomenclature[status$source == "wcvp"], "botanical")
})

test_that("a family prefix that agrees with the match is not penalised", {
  # The web service scores a submitted family it recognises as 1.  Leaving it
  # unscored still counts it as a component the query offered, which divided
  # the score by a part contributing nothing and penalised a correct family
  # exactly as though it had been wrong.
  tmp <- file.path(tempdir(), "tnrs-nom-fam")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(
      data.frame(
        taxonID = c("1", "2"),
        scientificName = c("Quercus alba", "Acer rubrum"),
        taxonomicStatus = "accepted",
        family = c("Fagaceae", "Sapindaceae"),
        acceptedNameUsageID = c("1", "2"),
        stringsAsFactors = FALSE
      ),
      source = "fam", version = "1", dir = tmp, quiet = TRUE
    )
  )

  result <- TNRS_local(
    c("Fagaceae Quercus alba", "Quercus alba", "Helotiaceae Quercus alba"),
    sources = "fam", dir = tmp, build_missing = FALSE, quiet = TRUE
  )

  expect_identical(result$Family_score[1], 1)
  expect_identical(result$Overall_score[1], 1)
  # A bare name scores the same; the family was never the point
  expect_identical(result$Overall_score[2], 1)
  # A family the backbone does not recognise stays unscored and does penalise,
  # which is what the web service does
  expect_true(is.na(result$Family_score[3]))
  expect_lt(result$Overall_score[3], 0.7)
})
