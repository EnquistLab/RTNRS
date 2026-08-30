context("local resolution")

# These need a built backbone, which is a large download, so they skip unless
# one is already present.
skip_without_backbone <- function(sources = "wfo") {
  for (source in sources) {
    skip_if_not(
      file.exists(tnrs_names_path(source)),
      paste0("no local backbone for '", source, "'; run TNRS_local_build()")
    )
  }
}

test_that("a missing backbone is reported rather than erroring", {
  tmp <- file.path(tempdir(), "tnrs-cache-none")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_message(
    result <- TNRS_local("Acer rubrum", dir = tmp, quiet = TRUE),
    "TNRS_local_build"
  )
  expect_null(result)
})

test_that("arguments are validated", {
  expect_error(TNRS_local("Acer rubrum", accuracy = "high"), "accuracy")
  expect_message(
    result <- TNRS_local("Acer rubrum", sources = "not-a-source", quiet = TRUE),
    "Invalid source"
  )
  expect_null(result)
})

test_that("exact names resolve with a perfect score", {
  skip_without_backbone()

  result <- TNRS_local(
    c("Acer rubrum", "Solanum lycopersicum"),
    sources = "wcvp", quiet = TRUE
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$Name_matched, c("Acer rubrum", "Solanum lycopersicum"))
  expect_equal(result$Overall_score, c(1, 1))
  expect_equal(result$Genus_matched, c("Acer", "Solanum"))
  expect_true(all(result$Source == "wcvp"))
})

test_that("misspellings are corrected", {
  skip_without_backbone()

  result <- TNRS_local(
    c("Quercuss alba", "Xantium strumarium"),
    sources = "wcvp", quiet = TRUE
  )

  expect_equal(result$Name_matched, c("Quercus alba", "Xanthium strumarium"))
  # A near miss scores highly but not perfectly
  expect_true(all(result$Overall_score > 0.9 & result$Overall_score < 1))
})

test_that("infraspecific names match at infraspecific rank", {
  skip_without_backbone()

  result <- TNRS_local(
    c("Acer rubrum var. rubrum", "Quercus alba"),
    sources = "wcvp", quiet = TRUE
  )

  # The variety must not collapse to its parent species.  Name_matched_rank is
  # the taxonomic rank of the matched name, as the web service reports it.
  expect_equal(result$Name_matched_rank, c("variety", "species"))
  expect_match(result$Name_matched[1], "var\\.")
  expect_equal(result$Infraspecific_epithet_matched, c("rubrum", ""))
})

test_that("a name with no plausible match is reported as unmatched", {
  skip_without_backbone()

  result <- TNRS_local("Zzzzzzz qqqqqqq", sources = "wcvp", quiet = TRUE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$Name_matched, "[No match found]")
  expect_true(is.na(result$Overall_score))
  expect_equal(result$Accepted_name, "")
})

test_that("synonyms are resolved to their accepted name", {
  skip_without_backbone()

  names <- tnrs_load_names("wcvp")
  synonym <- which(
    names$taxonomic_status == "Synonym" &
      !is.na(names$accepted_name_id) &
      names$name_rank == "species" &
      nzchar(names$specific_epithet)
  )[1]

  result <- TNRS_local(
    names$scientific_name[synonym],
    sources = "wcvp", quiet = TRUE
  )

  expect_equal(result$Taxonomic_status, "Synonym")
  expect_equal(
    result$Accepted_name,
    names$scientific_name[names$accepted_name_id[synonym]]
  )
})

test_that("input can be a character vector or a two-column data.frame", {
  skip_without_backbone()

  from_vector <- TNRS_local("Acer rubrum", sources = "wcvp", quiet = TRUE)
  from_frame <- TNRS_local(
    data.frame(ID = 99, taxon = "Acer rubrum"),
    sources = "wcvp", quiet = TRUE
  )

  expect_equal(from_vector$ID, "1")
  expect_equal(from_frame$ID, "99")
  expect_equal(from_vector$Name_matched, from_frame$Name_matched)
})

test_that("matches = 'all' returns at least as many rows as 'best'", {
  skip_without_backbone(c("wcvp", "wfo"))

  best <- TNRS_local("Acer rubrum", sources = c("wcvp", "wfo"),
    matches = "best", quiet = TRUE
  )
  all_matches <- TNRS_local("Acer rubrum", sources = c("wcvp", "wfo"),
    matches = "all", quiet = TRUE
  )

  expect_equal(nrow(best), 1L)
  expect_gte(nrow(all_matches), nrow(best))
})

test_that("accuracy discards low-scoring matches", {
  skip_without_backbone()

  # Unlike the web service, the threshold applies to the overall score
  loose <- TNRS_local("Quercuss alba", sources = "wcvp", quiet = TRUE)
  strict <- TNRS_local("Quercuss alba", sources = "wcvp",
    accuracy = 0.999, quiet = TRUE
  )

  expect_equal(loose$Name_matched, "Quercus alba")
  expect_equal(strict$Name_matched, "[No match found]")
})

test_that("a single source is consulted by default", {
  skip_without_backbone()

  # The default differs from TNRS(), which blends wcvp and wfo.  One source
  # means one authority, and no conflict is possible.
  result <- TNRS_local("Acer rubrum", quiet = TRUE)

  expect_equal(result$Source, "wfo")
  expect_false(result$Source_conflict)
})

test_that("disagreement between sources is flagged", {
  skip_without_backbone(c("wcvp", "wfo"))

  # Bartlettina macrocephala is accepted in WFO 2025-12 and a synonym of
  # B. ehrenbergii in WCVP v15, so the two sources lead to different accepted
  # names.  Acer rubrum is uncontroversial in both.
  result <- TNRS_local(
    c("Bartlettina macrocephala", "Acer rubrum"),
    sources = c("wcvp", "wfo"), quiet = TRUE
  )

  expect_true(result$Source_conflict[1])
  expect_false(result$Source_conflict[2])

  # A single source cannot conflict with itself
  alone <- TNRS_local("Bartlettina macrocephala", sources = "wfo", quiet = TRUE)
  expect_false(alone$Source_conflict)

  # matches = "all" shows what each source said
  detail <- TNRS_local("Bartlettina macrocephala",
    sources = c("wcvp", "wfo"), matches = "all", quiet = TRUE
  )
  expect_setequal(detail$Source, c("wcvp", "wfo"))
  expect_gt(length(unique(detail$Accepted_name)), 1L)
})

test_that("every submitted name yields exactly one row when matches = 'best'", {
  skip_without_backbone()

  submitted <- c("Acer rubrum", "Zzzzzzz qqqqqqq", "Quercuss alba", "Miconia")
  result <- TNRS_local(submitted, sources = "wcvp", matches = "best", quiet = TRUE)

  expect_equal(nrow(result), length(submitted))
  expect_equal(result$Name_submitted, submitted)
  expect_equal(result$ID, as.character(seq_along(submitted)))
})

test_that("the output carries the same columns as the web service", {
  skip_without_backbone()
  skip_if_not_installed("yaml")

  cassette <- testthat::test_path("..", "fixtures", "tnrs_base.yml")
  skip_if_not(file.exists(cassette))

  y <- yaml::read_yaml(cassette)
  api <- do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))

  result <- TNRS_local("Acer rubrum", quiet = TRUE)

  # Every column the web service returns must be present, so that code written
  # against TNRS() keeps working
  expect_true(all(colnames(api) %in% colnames(result)))
  # The only addition is our own provenance flag
  expect_equal(setdiff(colnames(result), colnames(api)), "Source_conflict")
})

test_that("warning flags are set and rendered", {
  skip_without_backbone()

  # A binomial that can only be matched at genus rank is Partial
  partial <- TNRS_local("Quercus zzzzzzzz", quiet = TRUE)
  expect_true(bitwAnd(partial$Warnings, 1L) > 0L)
  expect_match(partial$WarningsEng, "Partial")
  # Upstream blanks the author fields on a partial match
  expect_equal(partial$Author_matched, "")

  # A clean match carries no warnings
  clean <- TNRS_local("Acer rubrum", quiet = TRUE)
  expect_equal(clean$Warnings, 0L)
  expect_equal(clean$WarningsEng, "")
})

test_that("unmatched terms report what the match did not account for", {
  skip_without_backbone()

  # The family is accounted for when it agrees with the matched name's family
  matched_family <- TNRS_local("Sapindaceae Acer rubrum", quiet = TRUE)
  expect_equal(matched_family$Unmatched_terms, "")

  # Junk that no component explains is reported
  leftover <- TNRS_local("Acer rubrum zzzzqqq", quiet = TRUE)
  expect_match(leftover$Unmatched_terms, "zzzzqqq")
})

test_that("a second infraspecific epithet resolves when the source has the name", {
  skip_without_backbone()

  # WFO carries 44 names with two infraspecific epithets, all accepted taxa.
  # The web service does not resolve to that level; it returns the parent, or
  # nothing at all.  These resolve exactly here, via the whole-name lookup.
  parsed <- tnrs_parse("Cirsium japonicum var. vestitum f. arakii")
  expect_equal(parsed$infra1, "vestitum")
  expect_equal(parsed$infra2, "arakii")

  result <- TNRS_local("Cirsium japonicum var. vestitum f. arakii", quiet = TRUE)

  expect_equal(result$Name_matched, "Cirsium japonicum var. vestitum f. arakii")
  expect_equal(result$Name_matched_rank, "form")
  expect_equal(result$Overall_score, 1)
  # A complete match carries no warning
  expect_equal(result$WarningsEng, "")
})

test_that("an unresolvable second epithet falls back to the parent and says so", {
  skip_without_backbone()

  # No such forma exists, so the name resolves to the variety it sits within.
  # That parent is correct as far as it goes, but the resolution is incomplete.
  result <- TNRS_local("Cirsium japonicum var. vestitum f. zzzzzzz", quiet = TRUE)

  expect_equal(result$Name_matched, "Cirsium japonicum var. vestitum")
  expect_true(bitwAnd(result$Warnings, 1L) > 0L)
  expect_match(result$WarningsEng, "Partial")
})

test_that("forma names reach the whole-name lookup despite the rank spelling", {
  skip_without_backbone()

  # The parser standardizes "f." to "fo." while the sources write "f.", so the
  # reassembled key never matches these; the whole-name key does.
  result <- TNRS_local("Acer rubrum f. rubrum", quiet = TRUE)

  expect_equal(result$Name_matched, "Acer rubrum f. rubrum")
  expect_equal(result$Overall_score, 1)
})

test_that("batching does not change the result", {
  skip_without_backbone()

  names <- c(
    "Acer rubrum", "Quercuss alba", "Zzzzzzz qqqqqqq", "Miconia",
    "Xantium strumarium", "Acer rubrum var. rubrum", "Solanum lycopersicum"
  )

  whole <- TNRS_local(names, batch_size = 10000, quiet = TRUE)
  in_twos <- TNRS_local(names, batch_size = 2, quiet = TRUE)
  one_by_one <- TNRS_local(names, batch_size = 1, quiet = TRUE)

  expect_identical(whole, in_twos)
  expect_identical(whole, one_by_one)
  expect_equal(nrow(whole), length(names))
})

test_that("batch_size is validated", {
  expect_error(TNRS_local("Acer rubrum", batch_size = 0), "batch_size")
  expect_error(TNRS_local("Acer rubrum", batch_size = -5), "batch_size")
  expect_error(TNRS_local("Acer rubrum", batch_size = "many"), "batch_size")
})

test_that("a missing source names itself and the call that would build it", {
  tmp <- file.path(tempdir(), "tnrs-cache-req")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Nothing built: the message should name the source and the exact fix
  expect_message(
    ok <- tnrs_require_sources("wcvp", dir = tmp, build_missing = FALSE),
    'TNRS_local_build("wcvp")',
    fixed = TRUE
  )
  expect_false(ok)

  # Two missing sources are offered as a single call
  expect_message(
    tnrs_require_sources(c("wcvp", "wfo"), dir = tmp, build_missing = FALSE),
    'TNRS_local_build(c("wcvp", "wfo"))',
    fixed = TRUE
  )
})

test_that("only the sources that are actually missing are reported", {
  tmp <- file.path(tempdir(), "tnrs-cache-partial")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Stand in for a built source; only its presence is checked
  file.create(tnrs_names_path("wfo", tmp))

  expect_true(tnrs_require_sources("wfo", dir = tmp, build_missing = FALSE))

  msg <- capture_messages(
    tnrs_require_sources(c("wfo", "wcvp"), dir = tmp, build_missing = FALSE)
  )
  msg <- paste(msg, collapse = "")
  expect_match(msg, "No local copy of: wcvp")
  expect_match(msg, "wfo is already built")
  expect_false(grepl("No local copy of: wfo", msg))
})

test_that("build_missing = FALSE never downloads", {
  tmp <- file.path(tempdir(), "tnrs-cache-nodl")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    result <- TNRS_local("Acer rubrum", sources = "wcvp", dir = tmp,
                         build_missing = FALSE, quiet = TRUE)
  )
  expect_null(result)
  # An empty cache directory is the proof that nothing was fetched
  expect_length(list.files(tmp), 0)
})

test_that("a source set is rendered as it would be typed", {
  expect_identical(tnrs_source_arg("wfo"), '"wfo"')
  expect_identical(tnrs_source_arg(c("wcvp", "wfo")), 'c("wcvp", "wfo")')
})
