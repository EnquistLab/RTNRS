context("blocking and candidate generation")

# A small reference with predictable morphology
reference_genera <- function() {
  stems <- c(
    "acer", "quer", "carex", "pinus", "abies", "sola", "rosa", "salix",
    "poa", "viola", "aster", "ficus", "hedera", "ilex", "malus", "olea",
    "dendro", "xantho", "erio", "calli", "chryso", "melano", "leuco"
  )
  tails <- c(
    "carpus", "phyllum", "anthus", "cladus", "spermum", "stachys",
    "lepis", "pogon", "thrix", "dendron", "stemon", "pteris"
  )
  out <- unique(as.vector(outer(outer(stems, c("", "o", "i"), paste0), tails, paste0)))
  paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
}

# Literal transcription of the WHERE clause of Queries::genus_cur3(), used as
# the oracle for the indexed implementation
naive_genus_candidates <- function(query, index, window = 2L) {
  upper <- tnrs_toupper_ascii(query)
  len <- nchar(upper, type = "bytes")
  key <- tnrs_near_match(query, "genus_only")

  shorter <- pmin(len, index$len)
  in_window <- abs(index$len - len) <= window
  affix <-
    (shorter < 5 & (index$h1 == substr(upper, 1, 1) |
      index$t1 == substr(upper, len, len))) |
      (shorter == 5 & (index$h2 == substr(upper, 1, 2) |
        index$t3 == substr(upper, max(1L, len - 2L), len))) |
      (shorter > 5 & (index$h3 == substr(upper, 1, 3) |
        index$t3 == substr(upper, max(1L, len - 2L), len)))

  which(index$key == key | (in_window & affix))
}

test_that("the hash index round-trips keys to positions", {
  keys <- c("A", "B", "A", "", NA, "C", "B")
  idx <- tnrs_hash_index(keys)

  expect_equal(tnrs_lookup(idx, "A"), c(1L, 3L))
  expect_equal(tnrs_lookup(idx, "B"), c(2L, 7L))
  expect_equal(sort(tnrs_lookup(idx, c("A", "C"))), c(1L, 3L, 6L))

  # Absent, empty and missing keys yield nothing rather than erroring
  expect_equal(tnrs_lookup(idx, "ZZZ"), integer(0))
  expect_equal(tnrs_lookup(idx, ""), integer(0))
  expect_equal(tnrs_lookup(idx, NA_character_), integer(0))
  expect_equal(tnrs_lookup(idx, character(0)), integer(0))
})

test_that("the rank index records the columns the blocking relies on", {
  idx <- tnrs_build_rank_index(c("Quercus", "Acer"), "genus_only")

  expect_equal(idx$upper, c("QUERCUS", "ACER"))
  expect_equal(idx$len, c(7L, 4L))
  expect_equal(idx$h1, c("Q", "A"))
  expect_equal(idx$h3, c("QUE", "ACE"))
  expect_equal(idx$t1, c("S", "R"))
  expect_equal(idx$t3, c("CUS", "CER"))
  expect_equal(idx$key, tnrs_near_match(c("Quercus", "Acer"), "genus_only"))

  # Length window slicing
  expect_equal(sort(tnrs_in_length_window(idx, 7L, 2L)), 1L)
  expect_equal(sort(tnrs_in_length_window(idx, 5L, 3L)), c(1L, 2L))
  expect_equal(tnrs_in_length_window(idx, 30L, 2L), integer(0))
})

test_that("indexed candidate generation matches a literal reading of the SQL", {
  set.seed(7)
  genera <- reference_genera()
  idx <- tnrs_build_rank_index(genera, "genus_only")

  mutate <- function(x, k) {
    chars <- strsplit(x, "")[[1]]
    for (i in seq_len(k)) {
      pos <- sample(seq_along(chars), 1)
      chars <- switch(sample(c("sub", "del", "ins", "swap"), 1),
        sub = {
          chars[pos] <- sample(letters, 1)
          chars
        },
        del = chars[-pos],
        ins = append(chars, sample(letters, 1), after = pos - 1),
        swap = {
          other <- min(pos + 1, length(chars))
          chars[c(pos, other)] <- chars[c(other, pos)]
          chars
        }
      )
    }
    paste(chars, collapse = "")
  }

  queries <- c(
    sample(genera, 60),
    vapply(sample(genera, 60), mutate, character(1), k = 1),
    vapply(sample(genera, 60), mutate, character(1), k = 2),
    replicate(30, paste(sample(LETTERS, sample(3:12, 1), TRUE), collapse = ""))
  )

  for (query in queries) {
    expect_equal(
      as.integer(tnrs_genus_candidates(query, "", idx)),
      as.integer(naive_genus_candidates(query, idx, 2L)),
      info = query
    )
    expect_equal(
      as.integer(tnrs_genus_candidates(query, "", idx, search_mode = "extended")),
      as.integer(naive_genus_candidates(query, idx, 4L)),
      info = query
    )
  }
})

test_that("an exact name is always among its own candidates", {
  genera <- reference_genera()
  idx <- tnrs_build_rank_index(genera, "genus_only")

  for (genus in sample(genera, 40)) {
    cand <- tnrs_genus_candidates(genus, "", idx)
    expect_true(match(tnrs_toupper_ascii(genus), idx$upper[cand], nomatch = 0) > 0)
  }
})

test_that("blocking is selective", {
  # The point of the index is to compare against a small fraction of the
  # reference; if this regresses, matching gets slow rather than wrong.
  set.seed(3)
  genera <- reference_genera()
  idx <- tnrs_build_rank_index(genera, "genus_only")

  sizes <- vapply(
    sample(genera, 100),
    function(g) length(tnrs_genus_candidates(g, "", idx)), integer(1)
  )
  expect_lt(mean(sizes) / length(genera), 0.15)
})

test_that("the epithet rescue recovers a badly misspelled genus", {
  genera <- c("Quercus", "Acer", "Betula")
  gidx <- tnrs_build_rank_index(genera, "genus_only")
  # Species pointing at their parent genus by position
  sidx <- tnrs_build_rank_index(
    c("alba", "rubrum", "pendula"), "epithet_only",
    parent = c(1L, 2L, 3L)
  )

  # "Zzzrcxx" shares neither its first three nor its last three characters with
  # Quercus, so the affix test cannot reach it and it has no candidates at all
  expect_equal(length(tnrs_genus_candidates("Zzzrcxx", "", gidx)), 0L)

  # Supplying the epithet recovers it: Quercus carries a species "alba", whose
  # phonetic key matches, and Quercus is within the wider length window
  rescued <- tnrs_genus_candidates("Zzzrcxx", "alba", gidx, species_index = sidx)
  expect_true(1L %in% rescued)

  # A genus whose length is far from the query is not rescued even so
  far <- tnrs_build_rank_index(c("Quercusaurantiacum"), "genus_only")
  far_species <- tnrs_build_rank_index("alba", "epithet_only", parent = 1L)
  expect_equal(
    tnrs_genus_candidates("Zzzrcxx", "alba", far, species_index = far_species),
    integer(0)
  )
})

test_that("family candidates use the leading character only", {
  families <- c("Arecaceae", "Asteraceae", "Poaceae", "Fabaceae", "Brassicaceae")
  idx <- tnrs_build_rank_index(families, "genus_only")

  cand <- tnrs_family_candidates("Arecaceae", idx)
  expect_true(match("ARECACEAE", idx$upper[cand], nomatch = 0) > 0)

  # A misspelling that keeps the first letter and the length is still reachable
  cand <- tnrs_family_candidates("Arecacaea", idx)
  expect_true(match("ARECACEAE", idx$upper[cand], nomatch = 0) > 0)

  expect_equal(tnrs_family_candidates("", idx), integer(0))
})

test_that("child candidates are confined to matched parents and a length window", {
  species <- tnrs_build_rank_index(
    c("alba", "rubrum", "pendula", "verylongepithetname"), "epithet_only",
    parent = c(1L, 2L, 1L, 1L)
  )

  # Only children of parent 1, and only those within four characters of "alba"
  cand <- tnrs_child_candidates("alba", 1L, species)
  expect_true(1L %in% cand)
  expect_false(2L %in% cand) # different parent
  expect_false(4L %in% cand) # far outside the length window

  expect_equal(tnrs_child_candidates("alba", integer(0), species), integer(0))
  expect_equal(tnrs_child_candidates("", 1L, species), integer(0))
})

test_that("component matching reproduces the upstream acceptance rules", {
  # Exact match
  m <- tnrs_match_component("Quercus", "Quercus", "genus")
  expect_true(m$match)
  expect_true(m$phonetic)
  expect_equal(m$edit_distance, 0L)

  # One edit in a seven-character name is inside the ratio threshold
  expect_true(tnrs_match_component("Quercus", "Qvercus", "genus")$match)

  # Unrelated names are rejected
  expect_false(tnrs_match_component("Quercus", "Betula", "genus")$match)

  # Vectorised over candidates
  m <- tnrs_match_component("Quercus", c("Quercus", "Qvercus", "Betula"), "genus")
  expect_equal(m$match, c(TRUE, TRUE, FALSE))

  # Precomputed keys must not change the answer
  cand <- c("Quercus", "Qvercus", "Betula")
  expect_equal(
    tnrs_match_component("Quercus", cand, "genus"),
    tnrs_match_component("Quercus", cand, "genus",
      query_key = tnrs_near_match("Quercus", "genus_only"),
      candidate_key = tnrs_near_match(cand, "genus_only")
    )
  )

  # An empty string never matches
  expect_false(tnrs_match_component("Quercus", "", "genus")$match)
})

test_that("combining components sums distances and requires all to match", {
  good <- tnrs_match_component("Quercus", "Quercus", "genus")
  near <- tnrs_match_component("alba", "albus", "epithet")
  bad <- tnrs_match_component("Quercus", "Betula", "genus")

  combined <- tnrs_combine_matches(list(good, near))
  expect_true(combined$match)
  expect_equal(combined$edit_distance, good$edit_distance + near$edit_distance)
  # Phonetic only when every component is phonetic
  expect_equal(combined$phonetic, good$phonetic && near$phonetic)

  expect_false(tnrs_combine_matches(list(good, bad))$match)
})
