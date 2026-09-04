context("triage by comparing two resolutions")

# One row per case, built by hand: no backbone is needed, since the function
# only reads the result columns
side <- function(submitted, matched, rank, accepted, author_score = NA, warnings = 0L) {
  data.frame(
    Name_submitted = submitted, Name_matched = matched, Name_matched_rank = rank,
    Accepted_name = accepted, Author_score = author_score, Warnings = warnings,
    stringsAsFactors = FALSE
  )
}

test_that("each tier is reached by its rule", {
  a <- side(
    c(
      "Ixodes ricinus", "Hyalomma rufipes", "Ixodes sp.", "Argas vespertilionis",
      "Culiseta melanura (Coquillett, 1902)", "Culex spec.", "BOLD:AAA1",
      "Ablyomma chabaudi", "Hyalomma anatolicum", "Ixodes barkeri Barker 2019",
      "Ixodes barkeri", "Boophilus annulatus", "Xyz"
    ),
    c(
      "Ixodes ricinus", "Hyalomma rufipes", "Ixodes", "Argas vespertilionis",
      "Culiseta (Climacura) melanurus", "Culex", "[No match found]",
      "Amblyomma chabaudi", "Hyalomma", "Ixodes bakeri",
      "Ixodes bakeri", "Boophilus annulatus", "[No match found]"
    ),
    c(
      "species", "species", "genus", "species",
      "species", "genus", "",
      "species", "genus", "species",
      "species", "species", ""
    ),
    c(
      "Ixodes ricinus", "Hyalomma turanicum", "Ixodes", "Carios vespertilionis",
      "Culiseta (Climacura) melanurus", "Culex", "",
      "Amblyomma chabaudi", "Hyalomma", "Ixodes bakeri",
      "Ixodes bakeri", "Boophilus annulatus", ""
    ),
    author_score = c(NA, NA, NA, NA, 1, NA, NA, NA, NA, 0.11, NA, NA, NA),
    warnings = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 16L, 0L, 0L, 0L)
  )
  b <- side(
    a$Name_submitted,
    c(
      "Ixodes ricinus", "Hyalomma rufipes", "Ixodes", "Carios vespertilionis",
      "Culiseta melanura", "Culex spec", "BOLD:AAA1",
      "Ixodidae", "Hyalomma anatolicum", "Ixodes barkeri",
      "Ixodes barkeri", "Rhipicephalus", "[No match found]"
    ),
    c(
      "species", "species", "genus", "species",
      "species", "species", "unranked",
      "family", "species", "species",
      "species", "genus", ""
    ),
    c(
      "Ixodes ricinus", "Hyalomma rufipes", "Ixodes", "Carios vespertilionis",
      "Culiseta melanura", "Culex spec", "",
      "Ixodidae", "Hyalomma anatolicum", "Ixodes barkeri",
      "Ixodes barkeri", "Rhipicephalus", ""
    )
  )

  t <- TNRS_triage(a, b)
  expect_equal(nrow(t), 13L)
  expect_equal(t$Tier, c(1L, 2L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 7L, 5L))
  expect_equal(t$Detail[c(4, 5)], c("different synonyms, same accepted name", "spelling variant, author confirms"))
  expect_equal(t$Detail[c(10, 11, 12)], c(
    "fuzzy match, author contradicts it", "same genus, different species", "different genus"
  ))
  expect_equal(t$Detail[13], "neither side could read it")

  # Suggestions: the agreed reading, the deeper one, the genus for a vague string
  expect_equal(t$Suggested_from[1:3], c("both", "both", "both"))
  expect_equal(t$Suggested_name[8], "Amblyomma chabaudi")
  expect_equal(t$Suggested_from[8], "a")
  expect_equal(t$Suggested_name[9], "Hyalomma anatolicum")
  expect_equal(t$Suggested_from[9], "b")
  expect_equal(t$Suggested_name[6], "Culex")
  expect_equal(t$Suggested_rank[6], "genus")
  expect_equal(t$Suggested_from[10:12], c("", "", ""))
})

test_that("a subgenus in parentheses and a rank connector do not break agreement", {
  a <- side("Aedes aegypti", "Aedes (Stegomyia) aegypti", "species", "Aedes (Stegomyia) aegypti")
  b <- side("Aedes aegypti", "Aedes aegypti", "species", "Aedes aegypti")
  expect_equal(TNRS_triage(a, b)$Tier, 1L)

  a <- side("Acer rubrum var. rubrum", "Acer rubrum var. rubrum", "variety", "Acer rubrum var. rubrum")
  b <- side("Acer rubrum var. rubrum", "Acer rubrum rubrum", "subspecies", "Acer rubrum rubrum")
  expect_equal(TNRS_triage(a, b)$Tier, 1L)
})

test_that("a bare subgenus agrees with the same name ranked as a genus elsewhere", {
  a <- side("Culex (Melanoconion)", "Culex (Melanoconion)", "subgenus", "Culex (Melanoconion)")
  b <- side("Culex (Melanoconion)", "Melanoconion", "genus", "Melanoconion")
  t <- TNRS_triage(a, b)
  expect_equal(t$Tier, 3L)
  expect_equal(t$Suggested_name, "Culex (Melanoconion)")
})

test_that("a close epithet without an author is a contradiction, not a variant", {
  a <- side("Ixodes barkeri", "Ixodes bakeri", "species", "Ixodes bakeri")
  b <- side("Ixodes barkeri", "Ixodes barkeri", "species", "Ixodes barkeri")
  expect_equal(TNRS_triage(a, b)$Tier, 7L)
})

test_that("a reading with no rank column is sorted by the shape of the name", {
  a <- data.frame(
    Name_submitted = c("Ixodes ricinus", "Ixodes"),
    Name_matched = c("Ixodes ricinus", "Ixodes"), Name_matched_rank = "",
    Accepted_name = c("Ixodes ricinus", "Ixodes"), stringsAsFactors = FALSE
  )
  b <- data.frame(
    Name_submitted = c("Ixodes ricinus", "Ixodes"),
    Name_matched = c("Ixodes ricinus", "Ixodidae"), Name_matched_rank = "",
    Accepted_name = c("Ixodes ricinus", "Ixodidae"), stringsAsFactors = FALSE
  )
  t <- TNRS_triage(a, b)
  expect_equal(t$Tier, c(1L, 6L))
  expect_equal(t$Suggested_from[2], "a")
})

test_that("the shipped GBIF sample has the shape the help page relies on", {
  s <- gbif_triage_sample
  expect_true(all(c(
    "group", "verbatimScientificName", "verbatimScientificNameAuthorship", "n_records",
    "gbif_name", "gbif_rank", "gbif_status", "gbif_accepted_name", "gbif_taxon_key"
  ) %in% names(s)))
  expect_setequal(unique(s$group), c("Culicidae", "Ixodida"))
  expect_false(any(duplicated(paste(s$group, s$verbatimScientificName))))
  expect_true(all(s$n_records >= 1L))
  # The examples the documentation names are present
  expect_true(all(c(
    "Ixodes barkeri Barker 2019", "Culiseta melanura (Coquillett, 1902)",
    "Culex (Melanoconion)", "Hyalomma marginatum", "no pcr done"
  ) %in% s$verbatimScientificName))

  # GBIF's side alone, compared with itself, is all agreement: the shape
  # feeds TNRS_triage() without a backbone
  gbif <- data.frame(
    Name_submitted = s$verbatimScientificName,
    Name_matched = ifelse(nzchar(s$gbif_name), s$gbif_name, "[No match found]"),
    Name_matched_rank = s$gbif_rank, Accepted_name = s$gbif_accepted_name,
    stringsAsFactors = FALSE
  )
  t <- TNRS_triage(gbif, gbif)
  expect_true(all(t$Tier %in% c(1L, 3L, 5L)))
})

test_that("mismatched inputs are refused", {
  a <- side(c("A b", "C d"), c("A b", "C d"), "species", c("A b", "C d"))
  expect_error(TNRS_triage(a, a[1, ]), "one row per name")
  b <- a
  b$Name_submitted[2] <- "E f"
  expect_error(TNRS_triage(a, b), "different names")
  expect_error(TNRS_triage(a, a[, c("Name_submitted", "Name_matched")]), "lacks")
})
