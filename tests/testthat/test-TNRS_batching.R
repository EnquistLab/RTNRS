context("tnrs batching")

# Exercises the multi-batch path of TNRS() with a stubbed TNRS_base(), so no
# network or cassette is needed.

with_stubbed_TNRS_base <- function(stub, fun = TNRS) {
  env <- new.env(parent = asNamespace("TNRS"))
  env$TNRS_base <- stub
  environment(fun) <- env
  fun
}

test_that("batches cover every row exactly once", {
  batches <- character(0)

  tnrs <- with_stubbed_TNRS_base(function(taxonomic_names, ...) {
    batches <<- c(
      batches,
      paste(range(as.integer(taxonomic_names[[1]])), collapse = "-")
    )
    taxonomic_names
  })

  names_in <- data.frame(ID = 1:12000, taxon = paste0("name", 1:12000))

  results <- tnrs(
    taxonomic_names = names_in,
    name_limit = 5000,
    skip_internet_check = TRUE
  )

  expect_equal(batches, c("1-5000", "5001-10000", "10001-12000"))
  expect_equal(nrow(results), 12000L)
  expect_equal(results$ID, names_in$ID)
})

test_that("a query that is an exact multiple of name_limit is batched correctly", {
  batches <- character(0)

  tnrs <- with_stubbed_TNRS_base(function(taxonomic_names, ...) {
    batches <<- c(
      batches,
      paste(range(as.integer(taxonomic_names[[1]])), collapse = "-")
    )
    taxonomic_names
  })

  results <- tnrs(
    taxonomic_names = data.frame(ID = 1:10000, taxon = paste0("name", 1:10000)),
    name_limit = 5000,
    skip_internet_check = TRUE
  )

  expect_equal(batches, c("1-5000", "5001-10000"))
  expect_equal(nrow(results), 10000L)
})

test_that("a failed batch warns instead of silently dropping names", {
  tnrs <- with_stubbed_TNRS_base(function(taxonomic_names, ...) {
    if (taxonomic_names[[1]][1] == 5001) {
      return(invisible(NULL))
    }
    taxonomic_names
  })

  names_in <- data.frame(ID = 1:12000, taxon = paste0("name", 1:12000))

  expect_warning(
    results <- tnrs(
      taxonomic_names = names_in,
      name_limit = 5000,
      skip_internet_check = TRUE
    ),
    regexp = "5001 to 10000"
  )

  expect_equal(nrow(results), 7000L)
})

test_that("a wholly unavailable API returns NULL rather than an error", {
  tnrs <- with_stubbed_TNRS_base(function(...) invisible(NULL))

  results <- suppressWarnings(tnrs(
    taxonomic_names = data.frame(ID = 1:12000, taxon = paste0("name", 1:12000)),
    name_limit = 5000,
    skip_internet_check = TRUE
  ))

  expect_null(results)
})
