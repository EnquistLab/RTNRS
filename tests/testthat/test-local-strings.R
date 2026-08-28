context("local string primitives")

# These test the building blocks of the local matcher against the behaviour of
# the upstream PHP.  Expected values are derived by hand from the TNRSbatch
# source; the final block checks them against real API output recorded in the
# vcr cassettes, which needs no network.

test_that("tnrs_normalize reproduces the upstream normalize()", {
  # Only the FIRST parenthesised and first bracketed group is dropped
  expect_equal(
    tnrs_normalize("Barbatia (Mesocibota) bistrigata (Dunker, 1866)"),
    "BARBATIA BISTRIGATA (Dunker, 1866)"
  )
  expect_equal(tnrs_normalize("Aphis [?] ficus Theobald"), "APHIS FICUS Theobald")

  # Angle-bracket content is stripped
  expect_equal(tnrs_normalize("<i>Acer</i> rubrum"), "ACER RUBRUM")

  # Uncertainty indicators are dropped, and require their surrounding spaces
  expect_equal(tnrs_normalize("Solanum sp. nigrum"), "SOLANUM NIGRUM")
  expect_equal(tnrs_normalize("Solanum cf. nigrum"), "SOLANUM NIGRUM")

  # good_chars() keeps only A-Z, space and full stop, so hyphens close up and
  # accented characters vanish rather than being transliterated
  expect_equal(tnrs_normalize("Foo-bar baz"), "FOOBAR BAZ")
  # U+00FC is u-umlaut; built by code point to keep this file ASCII
  expect_equal(
    tnrs_normalize(paste0("M", intToUtf8(0x00FC), "ller alba")),
    "MLLER ALBA"
  )

  # The authority is appended unaltered: neither upper cased nor filtered
  expect_equal(tnrs_normalize("Quercus alba L."), "QUERCUS ALBA L.")
  auth <- paste0("M", intToUtf8(0x00FC), "ll. Hal.")
  expect_equal(
    tnrs_normalize(paste("Hypopterygium incrassatolimbatum", auth)),
    paste("HYPOPTERYGIUM INCRASSATOLIMBATUM", auth)
  )

  # Blank and missing input
  expect_equal(tnrs_normalize(c("", "   ", NA)), c("", "", ""))
  expect_equal(length(tnrs_normalize(character(0))), 0L)
})

test_that("tnrs_near_match reproduces the Rees phonetic key", {
  # Leading digraph replacement, then the ordered soundalike substitutions
  expect_equal(tnrs_near_match("Aesculus", "genus_only"), "ESILIS")
  expect_equal(tnrs_near_match("Knautia", "genus_only"), "NAITA")
  expect_equal(tnrs_near_match("Mcintosha", "genus_only"), "MACINTASA")
  expect_equal(tnrs_near_match("Quercus", "genus_only"), "QIRCIS")

  # Only one leading replacement fires per word (if/elseif upstream)
  expect_equal(tnrs_near_match("Xanthium", "genus_only"), "ZANTIM")

  # Variant endings are normalized for epithets but not for genera
  expect_equal(tnrs_near_match("Alopecurus", "genus_only"), "ALAPICIRIS")
  expect_equal(tnrs_near_match("Alopecurus", "epithet_only"), "ALAPICIRA")
  expect_equal(tnrs_near_match("strumarium", "epithet_only"), "STRIMARA")

  # Ending stripping only applies to keys longer than four characters
  expect_equal(tnrs_near_match("bipatens", "epithet_only"), "BIPATINS")

  # Default: first word treated as a genus, later words as epithets
  expect_equal(tnrs_near_match("Quercus alba"), "QIRCIS ALBA")
  expect_equal(tnrs_near_match("Xanthium strumarium"), "ZANTIM STRIMARA")

  expect_equal(tnrs_near_match(c("", NA)), c("", ""))
})

test_that("tnrs_mdld_r handles the cases a plain edit distance does not", {
  expect_equal(tnrs_mdld_r("QUERCUS", "QUERCUS"), 0L)
  expect_equal(tnrs_mdld_r("QUERCUS", "QVERCUS"), 1L)

  # Adjacent transposition costs one, not two
  expect_equal(tnrs_mdld_r("ACER", "ACRE"), 1L)

  # Block transposition: common affixes trim to "IE" vs "EI"
  expect_equal(tnrs_mdld_r("ABIES", "ABEIS"), 1L)

  expect_equal(tnrs_mdld_r("", "ABC"), 3L)
  expect_equal(tnrs_mdld_r("A", "B"), 1L)
  expect_true(is.na(tnrs_mdld_r(NA, "ABC")))
})

test_that("edit distances agree with those implied by recorded API scores", {
  # The API reports component scores as 1 - ED / max(nchar), so the recorded
  # scores invert back to the edit distances the server actually computed.
  skip_if_not_installed("yaml")

  cassette <- testthat::test_path("..", "fixtures", "tnrs_base.yml")
  skip_if_not(file.exists(cassette))

  y <- yaml::read_yaml(cassette)
  res <- do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))

  implied_ed <- function(submitted, matched, score) {
    round((1 - as.numeric(score)) * pmax(nchar(matched), nchar(submitted)))
  }

  compare <- function(submitted, matched, score, block_limit, max_distance) {
    keep <- !is.na(suppressWarnings(as.numeric(score))) &
      nzchar(submitted) & nzchar(matched)
    submitted <- submitted[keep]
    matched <- matched[keep]
    # The compiled implementation is what ships, so validate that one against
    # the API; test-local-mdld.R proves it matches the R reference exactly.
    ours <- tnrs_mdld(
      tnrs_toupper_ascii(submitted), tnrs_toupper_ascii(matched),
      block_limit, max_distance
    )
    expect_equal(
      as.integer(ours),
      as.integer(implied_ed(submitted, matched, score[keep]))
    )
  }

  compare(res$Genus_submitted, res$Genus_matched, res$Genus_score, 2, 3)
  compare(
    res$Specific_epithet_submitted, res$Specific_epithet_matched,
    res$Specific_epithet_score, 4, 4
  )
  compare(res$Family_submitted, res$Family_matched, res$Family_score, 2, 3)
})

test_that("phonetic keys agree with the flag recorded by the API", {
  # Upstream ANDs the phonetic flag across the components actually compared, so
  # the comparison depends on the rank the name matched at.
  skip_if_not_installed("yaml")

  cassette <- testthat::test_path("..", "fixtures", "tnrs_base.yml")
  skip_if_not(file.exists(cassette))

  y <- yaml::read_yaml(cassette)
  res <- do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))

  key <- function(x, type) ifelse(nzchar(x), tnrs_near_match(x, type), "")

  genus_ok <- key(res$Genus_submitted, "genus_only") ==
    key(res$Genus_matched, "genus_only")
  epithet_ok <- key(res$Specific_epithet_submitted, "epithet_only") ==
    key(res$Specific_epithet_matched, "epithet_only")
  family_ok <- key(res$Family_submitted, "genus_only") ==
    key(res$Family_matched, "genus_only")

  # Names that matched below family rank are judged on genus and epithet;
  # names that only reached family rank are judged on the family alone.  Names
  # that matched nothing carry no flag at all, so they must be excluded rather
  # than being treated as two equal empty keys.
  matched_at_all <- nzchar(res$Name_matched_rank)
  matched_below_family <- nzchar(res$Genus_matched)

  ours <- ifelse(
    !matched_at_all,
    "",
    ifelse(
      matched_below_family,
      ifelse(genus_ok & epithet_ok, "Y", ""),
      ifelse(family_ok, "Y", "")
    )
  )

  expect_equal(ours, res$Phonetic)
})
