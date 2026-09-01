context("user-supplied sources")

# A small Darwin Core checklist, the shape a published one arrives in
dwc_checklist <- function() {
  data.frame(
    taxonID = c("c1", "c2", "c3"),
    scientificName = c(
      "Mammillaria elongata", "Mammillaria echinaria",
      "Echinocereus triglochidiatus"
    ),
    scientificNameAuthorship = c("DC.", "DC.", "Engelm."),
    taxonRank = c("species", "species", "species"),
    taxonomicStatus = c("accepted", "heterotypicSynonym", "accepted"),
    family = "Cactaceae",
    genus = c("Mammillaria", "Mammillaria", "Echinocereus"),
    specificEpithet = c("elongata", "echinaria", "triglochidiatus"),
    infraspecificEpithet = "",
    acceptedNameUsageID = c("c1", "c1", "c3"),
    stringsAsFactors = FALSE
  )
}

fresh_dir <- function(name) {
  tmp <- file.path(tempdir(), name)
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  tmp
}

test_that("statuses are folded onto the vocabulary the resolver scores on", {
  expect_identical(
    tnrs_standardize_status(c("accepted", "ACCEPTED", " Accepted ")),
    rep("Accepted", 3)
  )
  # Darwin Core distinguishes these; the TNRS does not
  expect_identical(
    tnrs_standardize_status(c("synonym", "homotypicSynonym", "heterotypicSynonym")),
    rep("Synonym", 3)
  )
  expect_identical(tnrs_standardize_status("doubtful"), "Unchecked")
  expect_identical(tnrs_standardize_status("misapplied"), "Unplaced")

  # Something unrecognised keeps its own spelling rather than being forced
  # into a bucket it may not belong in
  expect_identical(tnrs_standardize_status("provisional name"), "Provisional name")
  expect_identical(tnrs_standardize_status(""), "")
})

test_that("rank indicators are spelled out the way the sources spell them", {
  expect_identical(
    tnrs_rank_word(c("var.", "subsp.", "fo.", "cv.")),
    c("variety", "subspecies", "form", "cultivar")
  )
  expect_identical(tnrs_rank_word(""), "")
  # A rarer rank keeps its own spelling minus the point
  expect_identical(tnrs_rank_word("proles"), "proles")
})

test_that("the delimiter is read off the header rather than the extension", {
  tmp <- fresh_dir("tnrs-delim")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # A pipe-delimited file named .csv, which is what WCVP ships
  pipe_file <- file.path(tmp, "names.csv")
  writeLines(c("a|b|c", "1|2|3"), pipe_file)
  expect_identical(tnrs_guess_delim(pipe_file), "|")

  tab_file <- file.path(tmp, "names.tsv")
  writeLines(c("a\tb\tc", "1\t2\t3"), tab_file)
  expect_identical(tnrs_guess_delim(tab_file), "\t")

  comma_file <- file.path(tmp, "real.csv")
  writeLines(c("a,b,c", "1,2,3"), comma_file)
  expect_identical(tnrs_guess_delim(comma_file), ",")
})

test_that("a Darwin Core checklist needs no mapping", {
  names <- tnrs_import_checklist(dwc_checklist(), "cact", quiet = TRUE)

  expect_setequal(colnames(names), tnrs_name_columns())
  expect_identical(nrow(names), 3L)
  expect_identical(names$source, rep("cact", 3))
  expect_identical(names$genus[1], "Mammillaria")
  expect_identical(names$taxonomic_status, c("Accepted", "Synonym", "Accepted"))
})

test_that("missing name parts are derived from the name", {
  bare <- data.frame(
    taxon = c(
      "Mammillaria elongata DC.",
      "Echinocereus triglochidiatus var. mojavensis",
      "Ferocactus"
    ),
    stringsAsFactors = FALSE
  )

  names <- tnrs_import_checklist(
    bare, "bare",
    columns = c(scientific_name = "taxon"), quiet = TRUE
  )

  expect_identical(names$genus, c("Mammillaria", "Echinocereus", "Ferocactus"))
  expect_identical(names$specific_epithet[3], "")
  expect_identical(names$infraspecific_epithet[2], "mojavensis")
  expect_identical(names$rank_indicator[2], "var.")
  # Spelled as a rank name, since this is reported as Name_matched_rank
  expect_identical(names$name_rank, c("species", "variety", "genus"))
  expect_identical(names$authorship[1], "DC.")

  # With no status column every name is treated as accepted, which is what
  # makes a bare list resolvable at all
  expect_identical(unique(names$taxonomic_status), "Accepted")
})

test_that("a checklist without a usable name column says so", {
  bad <- data.frame(taxon = "Mammillaria elongata", stringsAsFactors = FALSE)

  expect_error(
    tnrs_import_checklist(bad, "bad", quiet = TRUE),
    "scientificName",
    fixed = TRUE
  )
  # The message names the columns that are actually there
  expect_error(tnrs_import_checklist(bad, "bad", quiet = TRUE), "taxon", fixed = TRUE)

  expect_error(
    tnrs_import_checklist(dwc_checklist(), "x", columns = c(nonsense = "a")),
    "Unknown field",
    fixed = TRUE
  )
  expect_error(
    tnrs_import_checklist(dwc_checklist(), "x", columns = "unnamed"),
    "named vector",
    fixed = TRUE
  )
})

test_that("rows without a name are dropped rather than carried through", {
  x <- dwc_checklist()
  x$scientificName[2] <- ""
  names <- tnrs_import_checklist(x, "cact", quiet = TRUE)
  expect_identical(nrow(names), 2L)
})

test_that("registration is refused without the metadata that makes it citable", {
  tmp <- fresh_dir("tnrs-custom-args")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_error(
    TNRS_local_add_source(dwc_checklist(), source = "cact", dir = tmp),
    "version is required",
    fixed = TRUE
  )
  # A code that would shadow a source this package downloads
  expect_error(
    TNRS_local_add_source(dwc_checklist(), source = "wfo", version = "1", dir = tmp),
    "Choose another code",
    fixed = TRUE
  )
  expect_error(
    TNRS_local_add_source(dwc_checklist(), source = "Not A Code", version = "1", dir = tmp),
    "short code",
    fixed = TRUE
  )
})

test_that("a registered source joins the registry and the status table", {
  tmp <- fresh_dir("tnrs-custom-reg")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(
      dwc_checklist(),
      source = "cact", version = "2021",
      full_name = "Cactaceae at Caryophyllales.org",
      doi = "10.3372/wi.51.51208", dir = tmp, quiet = TRUE
    )
  )

  registry <- tnrs_source_registry(tmp)
  expect_true("cact" %in% names(registry))
  expect_identical(registry$cact$version, "2021")
  # Nothing to fetch, so nothing to price
  expect_identical(registry$cact$download_mb, 0)
  expect_true(unname(tnrs_is_custom("cact", tmp)))
  expect_false(unname(tnrs_is_custom("wfo", tmp)))

  status <- suppressMessages(TNRS_local_status(dir = tmp))
  cact <- status[status$source == "cact", ]
  expect_true(cact$built)
  expect_identical(cact$version, "2021")
  expect_match(cact$doi, "10.3372", fixed = TRUE)
})

test_that("a registered source resolves names, fuzzily and through synonymy", {
  tmp <- fresh_dir("tnrs-custom-resolve")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(dwc_checklist(),
      source = "cact",
      version = "2021", dir = tmp, quiet = TRUE
    )
  )

  result <- TNRS_local(
    c("Mamillaria elongata", "Mammillaria echinaria"),
    sources = "cact", dir = tmp, build_missing = FALSE, quiet = TRUE
  )

  expect_identical(nrow(result), 2L)
  expect_identical(result$Source, rep("cact", 2))
  # A misspelling still matches
  expect_identical(result$Name_matched[1], "Mammillaria elongata")
  expect_gt(result$Overall_score[1], 0.9)
  # A synonym resolves to its accepted name, so acceptedNameUsageID was linked
  expect_identical(result$Taxonomic_status[2], "Synonym")
  expect_identical(result$Accepted_name[2], "Mammillaria elongata")
})

test_that("a user-supplied source blends with others like any built-in", {
  tmp <- fresh_dir("tnrs-custom-blend")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # The two sources disagree: one treats echinaria as a synonym, the other
  # accepts it in its own right
  splitter <- data.frame(
    taxonID = c("b1", "b2"),
    scientificName = c("Mammillaria elongata", "Mammillaria echinaria"),
    taxonomicStatus = c("accepted", "accepted"),
    acceptedNameUsageID = c("b1", "b2"),
    stringsAsFactors = FALSE
  )

  suppressMessages({
    TNRS_local_add_source(dwc_checklist(), source = "lumper", version = "1",
                          dir = tmp, quiet = TRUE)
    TNRS_local_add_source(splitter, source = "splitter", version = "1",
                          dir = tmp, quiet = TRUE)
  })

  result <- TNRS_local("Mammillaria echinaria",
    sources = c("lumper", "splitter"), dir = tmp,
    build_missing = FALSE, quiet = TRUE
  )

  expect_true(result$Source_conflict)
})

test_that("a user-supplied source is never treated as downloadable", {
  tmp <- fresh_dir("tnrs-custom-guard")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(dwc_checklist(),
      source = "cact",
      version = "2021", dir = tmp, quiet = TRUE
    )
  )

  expect_message(
    result <- TNRS_local_build("cact", dir = tmp),
    "nothing to download",
    fixed = TRUE
  )
  expect_null(result)

  expect_error(
    tnrs_download_source("cact", dir = tmp),
    "nothing to download",
    fixed = TRUE
  )

  # And if its data goes missing, the advice is to register it again rather
  # than to run a build that cannot help
  unlink(tnrs_names_path("cact", tmp))
  expect_message(
    ok <- tnrs_require_sources("cact", dir = tmp, build_missing = TRUE),
    "TNRS_local_add_source",
    fixed = TRUE
  )
  expect_false(ok)
})

test_that("re-registering is refused unless asked for", {
  tmp <- fresh_dir("tnrs-custom-overwrite")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(dwc_checklist(), source = "cact", version = "1",
                          dir = tmp, quiet = TRUE)
  )

  expect_message(
    TNRS_local_add_source(dwc_checklist(), source = "cact", version = "2",
                          dir = tmp, quiet = TRUE),
    "already registered",
    fixed = TRUE
  )
  status <- suppressMessages(TNRS_local_status(dir = tmp))
  expect_identical(status$version[status$source == "cact"], "1")

  suppressMessages(
    TNRS_local_add_source(dwc_checklist(), source = "cact", version = "2",
                          dir = tmp, overwrite = TRUE, quiet = TRUE)
  )
  status <- suppressMessages(TNRS_local_status(dir = tmp))
  expect_identical(status$version[status$source == "cact"], "2")
})

test_that("a checklist can be registered from a file", {
  tmp <- fresh_dir("tnrs-custom-file")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- file.path(tmp, "cact.csv")
  utils::write.csv(dwc_checklist(), path, row.names = FALSE)

  suppressMessages(
    TNRS_local_add_source(path, source = "cact", version = "2021",
                          dir = tmp, quiet = TRUE)
  )

  # The file is recorded, checksum and all, since it is the only record of
  # where the data came from
  record <- readRDS(tnrs_provenance_path("cact", tmp))
  expect_match(record$input, "cact.csv", fixed = TRUE)
  expect_true(nzchar(record$md5))
  expect_identical(record$names, 3L)
  expect_true(is.na(record$archive))
})

test_that("removal names the sources it cannot fetch again", {
  tmp <- fresh_dir("tnrs-custom-remove")

  suppressMessages(
    TNRS_local_add_source(dwc_checklist(), source = "cact", version = "1",
                          dir = tmp, quiet = TRUE)
  )

  expect_message(
    removed <- TNRS_local_remove(dir = tmp, ask = FALSE),
    "cannot download again",
    fixed = TRUE
  )
  expect_true(removed)
  expect_false(dir.exists(tmp))
})
