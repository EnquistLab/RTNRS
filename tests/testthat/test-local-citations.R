context("citing a local resolution")

test_that("the method and this package are always cited", {
  tmp <- file.path(tempdir(), "tnrs-cite-empty")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  out <- TNRS_local_citations(dir = tmp, quiet = TRUE)

  # Nothing is built here, so only the method and the software are cited
  expect_identical(out$what, c("method", "software"))
  expect_match(out$citation[1], "Boyle", fixed = TRUE)
  expect_match(out$citation[1], "BMC Bioinformatics", fixed = TRUE)
  expect_match(out$doi[1], "10.1186/1471-2105-14-16", fixed = TRUE)
  expect_match(out$citation[2], "TNRS", fixed = TRUE)
})

test_that("each source is cited with the version that was built", {
  tmp <- file.path(tempdir(), "tnrs-cite-built")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # A cache built against an earlier release must be cited as that release,
  # not as whatever the registry now points at
  file.create(tnrs_names_path("wfo", tmp))
  saveRDS(
    list(source = "wfo", full_name = "World Flora Online", version = "2023-06",
         doi = "10.5281/zenodo.old", downloaded = "2023-07-01", bytes = 1),
    tnrs_provenance_path("wfo", tmp)
  )

  out <- TNRS_local_citations(dir = tmp, quiet = TRUE)
  wfo <- out[out$what == "source", ]

  expect_identical(nrow(wfo), 1L)
  expect_identical(wfo$version, "2023-06")
  expect_match(wfo$citation, "World Flora Online", fixed = TRUE)
})

test_that("only the sources asked for are cited", {
  tmp <- file.path(tempdir(), "tnrs-cite-subset")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  out <- TNRS_local_citations(sources = c("wfo", "mdd"), dir = tmp, quiet = TRUE)
  expect_setequal(
    out$name[out$what == "source"],
    c("World Flora Online", "Mammal Diversity Database")
  )

  expect_error(
    TNRS_local_citations(sources = "nonesuch", dir = tmp, quiet = TRUE),
    "Unknown source", fixed = TRUE
  )
})

test_that("a checklist the user registered is cited too", {
  tmp <- file.path(tempdir(), "tnrs-cite-custom")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(
      data.frame(taxonID = "1", scientificName = "Mammillaria elongata",
                 taxonomicStatus = "accepted", acceptedNameUsageID = "1",
                 stringsAsFactors = FALSE),
      source = "cact", version = "2021", full_name = "Cactaceae checklist",
      citation = "Korotkova N. et al. 2021. Willdenowia 51: 251-271.",
      dir = tmp, quiet = TRUE
    )
  )

  out <- TNRS_local_citations(dir = tmp, quiet = TRUE)
  cact <- out[out$name == "Cactaceae checklist", ]
  expect_identical(nrow(cact), 1L)
  expect_match(cact$citation, "Korotkova", fixed = TRUE)
  expect_identical(cact$version, "2021")
})

test_that("a source registered without a citation still gets one", {
  tmp <- file.path(tempdir(), "tnrs-cite-nocite")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  suppressMessages(
    TNRS_local_add_source(
      data.frame(taxonID = "1", scientificName = "Quercus alba",
                 taxonomicStatus = "accepted", acceptedNameUsageID = "1",
                 stringsAsFactors = FALSE),
      source = "inhouse", version = "2026-09", full_name = "In-house list",
      publisher = "Our lab", dir = tmp, quiet = TRUE
    )
  )

  out <- TNRS_local_citations(dir = tmp, quiet = TRUE)
  mine <- out[out$name == "In-house list", ]
  # Built from what was recorded, rather than left blank
  expect_match(mine$citation, "In-house list", fixed = TRUE)
  expect_match(mine$citation, "2026-09", fixed = TRUE)
  expect_match(mine$citation, "Our lab", fixed = TRUE)
})

test_that("bibtex is written with keys that do not collide across versions", {
  tmp <- file.path(tempdir(), "tnrs-cite-bib")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  bib <- file.path(tmp, "refs.bib")
  out <- TNRS_local_citations(sources = c("wfo", "wcvp"), dir = tmp,
                              bibtex_file = bib, quiet = TRUE)
  expect_true(file.exists(bib))

  text <- readLines(bib)
  # Split on the punctuation rather than escaping it, which reads more plainly
  starts <- grep("^@misc", text, value = TRUE)
  keys <- vapply(strsplit(starts, "[{,]"), function(p) p[[2]], character(1))
  expect_identical(length(keys), nrow(out))
  expect_identical(anyDuplicated(keys), 0L)
  # The version is part of the key, so two releases of one source can coexist
  expect_true(any(grepl("WorldFloraOnline", keys, fixed = TRUE)))
  expect_true(any(grepl("10.1186", text, fixed = TRUE)))
})

test_that("the package ships a citation file that renders", {
  path <- system.file("CITATION", package = "TNRS")
  if (!nzchar(path)) path <- "../../inst/CITATION"
  skip_if_not(file.exists(path), "CITATION not found from this working directory")

  cit <- utils::readCitationFile(path, meta = list(Version = "0.4.0"))

  # Both the software and the method it implements
  expect_gte(length(cit), 2L)
  text <- paste(format(cit, style = "text"), collapse = " ")
  expect_match(text, "10.5281/zenodo.11186237", fixed = TRUE)
  expect_match(text, "10.1186/1471-2105-14-16", fixed = TRUE)
  # Author names the right way round: the paper is Boyle et al., not B. B.
  expect_match(text, "Boyle B", fixed = TRUE)
  expect_false(grepl("B. B,", text, fixed = TRUE))
})
