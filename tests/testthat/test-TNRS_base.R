context("tnrs base")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("tnrs_base", {
    results <- TNRS:::TNRS_base(taxonomic_names = tnrs_testfile, url = url, skip_internet_check = TRUE)
  })

  expect_s3_class(results, "data.frame")
  expect_equal(object = nrow(results), expected = nrow(tnrs_testfile))
})
