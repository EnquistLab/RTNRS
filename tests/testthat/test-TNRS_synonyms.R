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

  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_gte(object = nrow(results), expected = 1)
})
