context("tnrs synonyms")

test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("tnrs_synonyms", {
    results <- TNRS_synonyms(
      taxonomic_name = "Sabal palmetto",
      source = "wfo",
      url = url,
      skip_internet_check = TRUE
    )
  })

  expect_s3_class(results, "data.frame")
  expect_gte(object = nrow(results), expected = 1)
})
