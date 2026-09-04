context("local author matching and scoring")

read_fixture <- function() {
  cassette <- testthat::test_path("..", "fixtures", "tnrs_base.yml")
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(cassette))
  y <- yaml::read_yaml(cassette)
  do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))
}

test_that("tnrs_utf8_to_ascii reproduces the upstream table", {
  # U+00FC u-umlaut, U+00E9 e-acute, U+00C6 AE ligature, U+00DF sharp s
  expect_equal(tnrs_utf8_to_ascii(intToUtf8(0x00FC)), "u")
  expect_equal(tnrs_utf8_to_ascii(intToUtf8(0x00E9)), "e")
  expect_equal(tnrs_utf8_to_ascii(intToUtf8(0x00C6)), "AE")
  expect_equal(tnrs_utf8_to_ascii(intToUtf8(0x0153)), "oe")
  # Upstream maps the sharp s to an upper case B; kept deliberately
  expect_equal(tnrs_utf8_to_ascii(intToUtf8(0x00DF)), "B")
  # ASCII passes through untouched
  expect_equal(tnrs_utf8_to_ascii("Mueller"), "Mueller")
})

test_that("tnrs_normalize_auth reproduces the upstream special cases", {
  expect_equal(tnrs_normalize_auth("L."), "LINNAEUS")
  expect_equal(tnrs_normalize_auth("DC"), "DE CANDOLLE")
  expect_equal(tnrs_normalize_auth("(DC)"), "(DE CANDOLLE)")

  # "et" and "and" both become an ampersand, but "et al" is protected
  expect_equal(tnrs_normalize_auth("Born. et Flah."), "BORN. & FLAH.")
  expect_equal(tnrs_normalize_auth("Smith and Jones"), "SMITH & JONES")
  expect_equal(tnrs_normalize_auth("Smith et al."), "SMITH ET AL.")

  # Commas are removed before years only
  expect_equal(tnrs_normalize_auth("Dunker, 1866"), "DUNKER 1866")
  expect_equal(tnrs_normalize_auth("Smith, Jones"), "SMITH, JONES")

  expect_equal(tnrs_normalize_auth(c("", NA)), c("", ""))
})

test_that("tnrs_ngram computes Dice's coefficient with padding", {
  expect_equal(tnrs_ngram("abc", "abc", 2), 1)
  expect_equal(tnrs_ngram("abc", "xyz", 2), 0)
  # "ab" padded to " ab " gives 3 bigrams; identical strings match all of them
  expect_equal(tnrs_ngram("ab", "ab", 2), 1)
  expect_true(tnrs_ngram("Smith", "Smyth", 2) > 0)
  expect_true(tnrs_ngram("Smith", "Smyth", 2) < 1)
})

test_that("author scores agree exactly with those returned by the API", {
  res <- read_fixture()

  keep <- nzchar(res$Author_submitted) & nzchar(res$Author_matched) &
    !is.na(suppressWarnings(as.numeric(res$Author_score)))

  ours <- tnrs_compare_auth(res$Author_submitted[keep], res$Author_matched[keep])
  api <- as.numeric(res$Author_score)[keep]

  expect_gt(length(ours), 50)
  # One deliberate divergence: an abbreviated surname is expanded against the
  # other authority before comparing, which the web service intended but
  # never enabled.  Where that applies our score is higher than the service's;
  # everywhere else the two must agree exactly.
  same <- abs(ours - api) < 1e-6
  expect_true(all(same | ours > api))
  expect_gte(sum(same), length(ours) - 3L)
})

test_that("an absent authority yields no author score", {
  # Upstream returns NULL, and the aggregator then leaves the author term out of
  # the overall score entirely
  expect_true(is.na(tnrs_compare_auth("", "L.")))
  expect_true(is.na(tnrs_compare_auth("L.", "")))
  expect_true(is.na(tnrs_compare_auth(NA, "L.")))
})

test_that("tnrs_overall_score blends the name and author scores", {
  # Author weight is 0.2
  expect_equal(tnrs_overall_score(1, 1, 0), 1)
  expect_equal(tnrs_overall_score(0.5, 1, 0), 0.5 * 0.8 + 1 * 0.2)
  # No authority: the name score is used unchanged
  expect_equal(tnrs_overall_score(0.5, NA, 0), 0.5)
  # Surplus terms attract a flat 0.1 penalty
  expect_equal(tnrs_overall_score(0.5, NA, 1), 0.4)
  expect_equal(tnrs_overall_score(0.5, NA, 3), 0.4)
})

test_that("recorded overall scores are consistent with the scoring formula", {
  # extra_part depends on parser output that is not yet implemented, so this
  # checks the *form* of the score: every recorded Overall_score must be either
  # the unpenalised blend or that blend less exactly one penalty.
  res <- read_fixture()

  name_score <- suppressWarnings(as.numeric(res$Name_score))
  author_score <- suppressWarnings(as.numeric(res$Author_score))
  overall <- suppressWarnings(as.numeric(res$Overall_score))

  keep <- !is.na(name_score) & !is.na(overall)
  name_score <- name_score[keep]
  author_score <- author_score[keep]
  overall <- overall[keep]

  unpenalised <- tnrs_overall_score(name_score, author_score, 0)
  penalised <- tnrs_overall_score(name_score, author_score, 1)

  matches_one <- abs(overall - unpenalised) < 1e-9 | abs(overall - penalised) < 1e-9
  expect_true(all(matches_one))

  # And the penalty is only ever applied where terms went unmatched
  was_penalised <- abs(overall - penalised) < 1e-9 &
    abs(overall - unpenalised) >= 1e-9
  expect_true(all(nzchar(res$Unmatched_terms[keep][was_penalised])))
})

test_that("tnrs_extra_part only penalises genuinely surplus words", {
  # A family-rank match whose family name is the sole unmatched term is fully
  # accounted for: one unmatched word, matched_part 0, parsed_part 1
  expect_equal(tnrs_extra_part(1, 0, 1), 0)

  # A duplicated family name is one word more than the ranks can account for
  expect_equal(tnrs_extra_part(1, 3, 3), 1)

  # A parsed infraspecific rank indicator absorbs one word
  expect_equal(tnrs_extra_part(2, 3, 3, has_rank1 = TRUE), 1)

  # No unmatched terms means no surplus, whatever the ranks
  expect_equal(tnrs_extra_part(0, 4, 1), 0)
})

test_that("tnrs_ed_score and tnrs_num_to_score behave as upstream", {
  expect_equal(tnrs_ed_score(0, "ACER", "ACER"), 1)
  expect_equal(tnrs_ed_score(1, "ACER", "ACER"), 0.75)
  # Both strings empty scores zero rather than dividing by zero
  expect_equal(tnrs_ed_score(0, "", ""), 0)
  # Longer of the two strings is the denominator
  expect_equal(tnrs_ed_score(1, "ACER", "ACERACEAE"), 1 - 1 / 9)

  # A perfect component sum maps to exactly 1, and the transform is monotonic
  expect_equal(tnrs_num_to_score(2, 2), 1)
  expect_lt(tnrs_num_to_score(1.5, 2), tnrs_num_to_score(1.9, 2))
  expect_equal(tnrs_num_to_score(1, 2), 0.5)
})

test_that("component scores reconstruct the recorded genus scores", {
  res <- read_fixture()

  score <- suppressWarnings(as.numeric(res$Genus_score))
  keep <- !is.na(score) & nzchar(res$Genus_matched) & nzchar(res$Genus_submitted)

  ed <- tnrs_mdld(
    tnrs_toupper_ascii(res$Genus_submitted[keep]),
    tnrs_toupper_ascii(res$Genus_matched[keep]),
    2, 3
  )

  expect_equal(
    tnrs_ed_score(ed, res$Genus_matched[keep], res$Genus_submitted[keep]),
    score[keep]
  )
})

test_that("an abbreviated surname is expanded against the other authority", {
  # Each of these is one author written two ways.  The n-gram comparison
  # alone scores them between 0.07 and 0.4; a missing year still costs
  # points after expansion, so the bar is clearing 0.5, which is what the
  # [Author] warning turns on
  expect_gte(tnrs_compare_auth("Edw.", "Edwards, 1914"), 0.7)
  expect_gte(tnrs_compare_auth("Edw.", "(Edwards, 1914)"), 0.55)
  expect_gte(tnrs_compare_auth("Theob.", "Theobald, 1901"), 0.7)
  expect_gte(tnrs_compare_auth("Dyar & Kn.", "Dyar & Knab, 1906"), 0.8)
  # No stop at all, as GBIF strings often have it
  expect_gte(tnrs_compare_auth("Th", "(Theobald, 1903)"), 0.55)
  # It works in the other direction too
  expect_gte(tnrs_compare_auth("Edwards, 1914", "Edw."), 0.7)
  # A parenthesised abbreviation gains as much as an unparenthesised one
  expect_gt(tnrs_compare_auth("(Sup.)", "Supino, 1897"), tnrs_compare_auth("(Xup.)", "Supino, 1897") + 0.2)

  # A prefix of two surnames is ambiguous and left alone, so the score stays low
  expect_lt(tnrs_compare_auth("Th.", "Theobald & Thomson, 1901"), 0.5)
  # Not an abbreviation of anything on the other side: unchanged
  expect_lt(tnrs_compare_auth("Marks No.", "Meigen, 1818"), 0.3)
  # A one-letter initial is not expanded
  expect_lt(tnrs_compare_auth("B.", "Bonne-Wepster, 1920"), 0.5)
  # A second author that happens to open a hyphenated name is not an
  # abbreviation of it, so this pair scores as it did before the expansion
  expect_gte(tnrs_compare_auth("Bonne-Wepster", "Bonne-Wepster & Bonne, 1920"), 0.65)
  # Whole words that already agree are not touched
  expect_equal(tnrs_compare_auth("Neumann, 1901", "Neumann, 1901"), 1)
})
