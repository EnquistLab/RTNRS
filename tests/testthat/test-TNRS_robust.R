context("tnrs robust")

# These tests exercise the retry logic only, with a stubbed TNRS(), and so need
# neither the network nor a cassette.  Stubbing is done by re-parenting the
# function's environment rather than by editing the namespace, which keeps the
# tests portable across testthat editions.

with_stubbed_TNRS <- function(stub, fun = TNRS_robust) {
  env <- new.env(parent = asNamespace("TNRS"))
  env$TNRS <- stub
  environment(fun) <- env
  fun
}

# Minimal stand-in for a TNRS result.  A result is treated as suspicious when
# Name_submitted == Unmatched_terms and Overall_score is not NA.
fake_result <- function(ids, names, unmatched, score = 1) {
  data.frame(
    ID = as.character(ids),
    Name_submitted = names,
    Overall_score = score,
    Unmatched_terms = unmatched,
    stringsAsFactors = FALSE
  )
}

test_that("suspicious results are actually re-queried", {
  calls <- 0L

  robust <- with_stubbed_TNRS(function(taxonomic_names, ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      # ID 2 came back with the whole submitted name unmatched
      fake_result(1:3, c("Acer rubrum", "Abies alba", "Pinus taeda"),
        unmatched = c("", "Abies alba", "")
      )
    } else {
      fake_result(2, "Abies alba", unmatched = "")
    }
  })

  results <- suppressMessages(robust(
    taxonomic_names = data.frame(ID = 1:3, taxon = c("a", "b", "c")),
    skip_internet_check = TRUE
  ))

  expect_equal(calls, 2L)
  expect_equal(nrow(results), 3L)
  expect_equal(results$ID, c("1", "2", "3"))
  expect_true(all(results$Unmatched_terms == ""))
})

test_that("names that are never fixed are retained, not dropped", {
  calls <- 0L

  robust <- with_stubbed_TNRS(function(taxonomic_names, ...) {
    calls <<- calls + 1L
    fake_result(2, "Abies alba", unmatched = "Abies alba")
  })

  results <- suppressMessages(robust(
    taxonomic_names = data.frame(ID = 2, taxon = "b"),
    attempts = 3,
    skip_internet_check = TRUE
  ))

  # one initial query plus three retries
  expect_equal(calls, 4L)
  expect_equal(nrow(results), 1L)
  expect_equal(results$Unmatched_terms, "Abies alba")
})

test_that("clean results are returned without any retry", {
  calls <- 0L

  robust <- with_stubbed_TNRS(function(taxonomic_names, ...) {
    calls <<- calls + 1L
    fake_result(1:2, c("Acer rubrum", "Abies alba"), unmatched = c("", ""))
  })

  results <- robust(
    taxonomic_names = data.frame(ID = 1:2, taxon = c("a", "b")),
    skip_internet_check = TRUE
  )

  expect_equal(calls, 1L)
  expect_equal(nrow(results), 2L)
})

test_that("all rows for a suspicious name are re-done when matches = 'all'", {
  calls <- 0L

  robust <- with_stubbed_TNRS(function(taxonomic_names, ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      # ID 1 has two matches, one of which is suspicious
      fake_result(c(1, 1, 2), c("Acer rubrum", "Acer rubrum", "Abies alba"),
        unmatched = c("", "Acer rubrum", "")
      )
    } else {
      fake_result(c(1, 1), c("Acer rubrum", "Acer rubrum"), unmatched = c("", ""))
    }
  })

  results <- suppressMessages(robust(
    taxonomic_names = data.frame(ID = 1:2, taxon = c("a", "b")),
    matches = "all",
    skip_internet_check = TRUE
  ))

  # both matches for ID 1 replaced, neither duplicated
  expect_equal(nrow(results), 3L)
  expect_equal(sum(results$ID == "1"), 2L)
})

test_that("an unavailable API returns NULL rather than an error", {
  robust <- with_stubbed_TNRS(function(...) invisible(NULL))

  expect_null(robust(
    taxonomic_names = data.frame(ID = 1, taxon = "a"),
    skip_internet_check = TRUE
  ))
})

test_that("an API failure partway through keeps the results already obtained", {
  calls <- 0L

  robust <- with_stubbed_TNRS(function(taxonomic_names, ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      fake_result(1:2, c("Acer rubrum", "Abies alba"), unmatched = c("", "Abies alba"))
    } else {
      invisible(NULL)
    }
  })

  results <- suppressMessages(robust(
    taxonomic_names = data.frame(ID = 1:2, taxon = c("a", "b")),
    skip_internet_check = TRUE
  ))

  expect_equal(nrow(results), 2L)
  expect_equal(results$ID, c("1", "2"))
})
