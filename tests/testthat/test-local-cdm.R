context("EDIT platform exports")

# Shaped like the Caryophyllales.org workbooks: one sheet of accepted taxa and
# one of synonyms carrying the identifier of the taxon each belongs to.
cdm_accepted <- function() {
  data.frame(
    type = "Taxon",
    uuid = c("u1", "u2", "u3", "u4", "u5"),
    pureName = c(
      "Cactaceae",
      "Cactaceae_1_core_checklist",
      "__Cactaceae_2_hybrids",
      "Mammillaria elongata",
      "Acanthocalycium rhodotrichum subsp. chacoanum"
    ),
    author = c("Juss.", NA, NA, "DC.", "(Schutz) Schlumpb."),
    RANK = c("Family", "Family", "Family", "Species", "Subspecies"),
    stringsAsFactors = FALSE
  )
}

cdm_synonym <- function() {
  data.frame(
    type = "Synonym",
    uuid = c("s1", "s2", "s3"),
    pureName = c(
      "Cactus elongatus", "Cereus subg. Eriocereus", "Opuntia aggregata"
    ),
    author = c("null", "A.Berger", NA),
    rank = c("Species", "Subgenus", "Species Aggregate"),
    accepted_ID = c("u4", "u4", "u4"),
    stringsAsFactors = FALSE
  )
}

test_that("the two sheets are stacked into one Darwin Core table", {
  out <- TNRS_cdm_to_dwc(cdm_accepted(), cdm_synonym(), family = "Cactaceae")

  expect_true(all(
    c(
      "taxonID", "scientificName", "scientificNameAuthorship", "taxonRank",
      "taxonomicStatus", "family", "acceptedNameUsageID"
    ) %in% names(out)
  ))
  expect_identical(unique(out$family), "Cactaceae")
  expect_setequal(unique(out$taxonomicStatus), c("accepted", "synonym"))

  # An accepted name points at itself, a synonym at its accepted taxon
  accepted <- out[out$taxonomicStatus == "accepted", ]
  expect_identical(accepted$taxonID, accepted$acceptedNameUsageID)
  expect_identical(unique(out$acceptedNameUsageID[out$taxonomicStatus == "synonym"]), "u4")
})

test_that("placeholder nodes are dropped but real names are kept", {
  out <- TNRS_cdm_to_dwc(cdm_accepted())

  # The two organising nodes carry underscores, which no botanical name does
  expect_false(any(grepl("_", out$scientificName, fixed = TRUE)))
  expect_identical(nrow(out), 3L)
  # The real family row survives
  expect_true("Cactaceae" %in% out$scientificName)
})

test_that("the platform's own rank spellings are folded onto botanical ones", {
  expect_identical(
    tnrs_cdm_rank(c("Section bot.", "Subsection bot.", "Species Aggregate")),
    c("section", "subsection", "species")
  )
  expect_identical(tnrs_cdm_rank(c("Species", "SUBSPECIES")), c("species", "subspecies"))
  expect_identical(tnrs_cdm_rank(NA), "")

  out <- TNRS_cdm_to_dwc(cdm_accepted(), cdm_synonym())
  expect_true("subgenus" %in% out$taxonRank)
  # "Species Aggregate" is a species for our purposes
  expect_identical(out$taxonRank[out$scientificName == "Opuntia aggregata"], "species")
})

test_that("the ranks a checklist spells out all reach an indicator", {
  # Ranks below species must produce one, or an infraspecific name imports
  # without the connector that makes it findable
  expect_identical(
    tnrs_wfo_rank_indicator(c(
      "subspecies", "variety", "form", "subvariety",
      "subgenus", "section", "subsection", "cultivar"
    )),
    c("subsp.", "var.", "fo.", "subvar.", "subgen.", "sect.", "subsect.", "cv.")
  )
  # At and above species there is none
  expect_identical(
    tnrs_wfo_rank_indicator(c("species", "genus", "family")),
    rep("", 3)
  )
})

test_that("absent authors become empty rather than the word NA", {
  out <- TNRS_cdm_to_dwc(cdm_accepted(), cdm_synonym())
  # The export writes a missing author as a null, and read_excel gives NA
  expect_false(any(out$scientificNameAuthorship %in% c("NA", "null")))
  expect_identical(
    out$scientificNameAuthorship[out$scientificName == "Cactus elongatus"], ""
  )
})

test_that("a malformed export is reported rather than silently mis-read", {
  expect_error(
    TNRS_cdm_to_dwc(data.frame(a = 1)),
    "missing the column",
    fixed = TRUE
  )
  # The rank column may be RANK or rank, but one of them must be there
  expect_error(
    TNRS_cdm_to_dwc(data.frame(uuid = "u1", pureName = "Foo bar")),
    "RANK",
    fixed = TRUE
  )
  expect_error(
    TNRS_cdm_to_dwc(cdm_accepted(), data.frame(uuid = "s1", pureName = "x", rank = "Species")),
    "accepted_ID",
    fixed = TRUE
  )
  expect_error(TNRS_cdm_to_dwc("not a data frame"), "data.frame", fixed = TRUE)
})

test_that("a converted export registers and resolves", {
  tmp <- file.path(tempdir(), "tnrs-cdm-register")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  out <- TNRS_cdm_to_dwc(cdm_accepted(), cdm_synonym(), family = "Cactaceae")
  suppressMessages(
    TNRS_local_add_source(out,
      source = "cact", version = "2023-11-02",
      dir = tmp, quiet = TRUE
    )
  )

  result <- TNRS_local(
    c("Mamillaria elongata", "Cactus elongatus"),
    sources = "cact", dir = tmp, build_missing = FALSE, quiet = TRUE
  )

  # A misspelling still reaches the name
  expect_identical(result$Name_matched[1], "Mammillaria elongata")
  # And the synonym resolves through the accepted_ID link
  expect_identical(result$Taxonomic_status[2], "Synonym")
  expect_identical(result$Accepted_name[2], "Mammillaria elongata")
})

# The real exports carry a treeIndex, which is how the authors separate their
# core checklist from names they could not place
cdm_with_tree <- function() {
  data.frame(
    uuid = c("u1", "u2", "u3", "u4", "u5", "u6", "u7"),
    pureName = c(
      "Cactaceae_1_core_checklist", "__Cactaceae_3_names_of_uncertain_application",
      "__Cactaceae_6_excluded_names", "__Cactaceae_2_hybrids",
      "Mammillaria elongata", "Cereus dubius", "Opuntia excludenda"
    ),
    author = "",
    RANK = c(rep("Family", 4), "Species", "Species", "Species"),
    treeIndex = c(
      "#t42#33837#33839#", "#t42#33837#11499#", "#t42#33837#28799#",
      "#t42#33837#23243#",
      "#t42#33837#33839#1#", "#t42#33837#11499#2#", "#t42#33837#28799#3#"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("a name inherits the status of the subtree it sits under", {
  out <- TNRS_cdm_to_dwc(cdm_with_tree())
  status <- setNames(out$taxonomicStatus, out$scientificName)

  # Only the core checklist is accepted
  expect_identical(unname(status["Mammillaria elongata"]), "accepted")
  # A name of uncertain application is not an accepted taxon
  expect_identical(unname(status["Cereus dubius"]), "unchecked")
  expect_identical(unname(status["Opuntia excludenda"]), "unplaced")

  # And a name that is not accepted claims no accepted name
  not_accepted <- out[out$taxonomicStatus != "accepted", ]
  expect_true(all(!nzchar(not_accepted$acceptedNameUsageID)))
})

test_that("subtree labels map to statuses, and the unknown case is cautious", {
  expect_identical(tnrs_cdm_status("Cactaceae_1_core_checklist"), "accepted")
  expect_identical(tnrs_cdm_status("__Cactaceae_2_hybrids"), "accepted")
  expect_identical(tnrs_cdm_status("__Cactaceae_4_unplaced_taxa"), "unplaced")
  expect_identical(tnrs_cdm_status("__Cactaceae_5_unresolved_names"), "unchecked")
  # The label is what is matched, so the same works for another family
  expect_identical(tnrs_cdm_status("__Aizoaceae_5_unresolved_names"), "unchecked")
  # Wrongly accepting a name is the more damaging error
  expect_identical(tnrs_cdm_status("__Cactaceae_9_something_new"), "unchecked")
  expect_identical(tnrs_cdm_status(""), "accepted")
})

test_that("a flat export with no tree is read as accepted throughout", {
  x <- cdm_with_tree()
  x$treeIndex <- ""
  out <- TNRS_cdm_to_dwc(x)
  expect_identical(unique(out$taxonomicStatus), "accepted")
})
