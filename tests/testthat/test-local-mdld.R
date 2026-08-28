context("modified Damerau-Levenshtein")

test_that("the compiled and reference implementations agree on known cases", {
  cases <- list(
    list("QUERCUS", "QUERCUS", 0L),
    list("QUERCUS", "QVERCUS", 1L),
    # Adjacent transposition costs one, not two
    list("ACER", "ACRE", 1L),
    # Block transposition: common affixes trim to "IE" against "EI"
    list("ABIES", "ABEIS", 1L),
    list("", "ABC", 3L),
    list("ABC", "", 3L),
    list("A", "B", 1L),
    list("", "", 0L)
  )

  for (case in cases) {
    expect_equal(tnrs_mdld_r(case[[1]], case[[2]], 2, 3), case[[3]])
    expect_equal(tnrs_mdld(case[[1]], case[[2]], 2, 3), case[[3]])
  }
})

test_that("the compiled implementation matches the reference exactly", {
  # The reference implementation in R is the oracle; the compiled version is
  # what the matcher actually uses, so they must not drift apart.
  skip_if_not_installed("yaml")
  cassette <- testthat::test_path("..", "fixtures", "tnrs_base.yml")
  skip_if_not(file.exists(cassette))

  y <- yaml::read_yaml(cassette)
  res <- do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))

  real <- unique(c(
    res$Genus_submitted, res$Genus_matched,
    res$Specific_epithet_submitted, res$Specific_epithet_matched,
    res$Family_submitted, res$Family_matched
  ))
  real <- tnrs_toupper_ascii(real[nzchar(real)])

  set.seed(42)

  random_name <- function(n) paste(sample(LETTERS, n, replace = TRUE), collapse = "")
  mutate <- function(x) {
    chars <- strsplit(x, "")[[1]]
    if (length(chars) < 2) {
      return(x)
    }
    for (i in seq_len(sample(1:3, 1))) {
      pos <- sample(seq_along(chars), 1)
      chars <- switch(sample(c("sub", "del", "ins", "swap"), 1),
        sub = {
          chars[pos] <- sample(LETTERS, 1)
          chars
        },
        del = chars[-pos],
        ins = append(chars, sample(LETTERS, 1), after = pos - 1),
        swap = {
          other <- min(pos + 1, length(chars))
          chars[c(pos, other)] <- chars[c(other, pos)]
          chars
        }
      )
      if (length(chars) == 0) {
        return("")
      }
    }
    paste(chars, collapse = "")
  }

  base <- replicate(600, random_name(sample(1:14, 1)))
  pairs <- rbind(
    # near misses, which is where the block transposition logic bites
    data.frame(a = base, b = vapply(base, mutate, character(1)), stringsAsFactors = FALSE),
    # unrelated real names
    data.frame(
      a = sample(real, 600, TRUE), b = sample(real, 600, TRUE),
      stringsAsFactors = FALSE
    ),
    # identical pairs
    data.frame(a = real, b = real, stringsAsFactors = FALSE),
    # degenerate inputs
    data.frame(
      a = c("", "A", "AB", "AA", "ABAB", "", "ABCD"),
      b = c("", "A", "BA", "AA", "BABA", "XYZ", ""),
      stringsAsFactors = FALSE
    )
  )

  # Every parameter pairing upstream uses, plus two others
  for (params in list(c(2, 3), c(4, 4), c(1, 2), c(3, 5))) {
    reference <- mapply(
      function(x, y) tnrs_mdld_r(x, y, params[1], params[2]),
      pairs$a, pairs$b
    )
    compiled <- tnrs_mdld(pairs$a, pairs$b, params[1], params[2])
    expect_equal(as.integer(compiled), as.integer(reference),
      info = paste("block_limit =", params[1], "max_distance =", params[2])
    )
  }
})

test_that("the compiled implementation is vectorised and recycles", {
  expect_equal(
    tnrs_mdld(c("ACER", "QUERCUS"), c("ACRE", "QVERCUS"), 2, 3),
    c(1L, 1L)
  )
  # Shorter argument is recycled
  expect_equal(tnrs_mdld("ACER", c("ACER", "ACRE"), 2, 3), c(0L, 1L))
  expect_equal(length(tnrs_mdld(character(0), character(0))), 0L)
})

test_that("missing values propagate rather than erroring", {
  expect_equal(tnrs_mdld(c(NA, "AB"), c("AB", NA), 2, 3), c(NA_integer_, NA_integer_))
  expect_true(is.na(tnrs_mdld_r(NA, "ABC")))
})

test_that("comparison is done on bytes, as upstream does", {
  # PHP indexes strings by byte, so a two-byte character counts as two edits.
  # U+00FC is u-umlaut.
  u_umlaut <- intToUtf8(0x00FC)
  expect_equal(tnrs_mdld(paste0("M", u_umlaut, "LLER"), "MLLER", 2, 4), 2L)
  expect_equal(
    tnrs_mdld_r(paste0("M", u_umlaut, "LLER"), "MLLER", 2, 4),
    tnrs_mdld(paste0("M", u_umlaut, "LLER"), "MLLER", 2, 4)
  )
})
