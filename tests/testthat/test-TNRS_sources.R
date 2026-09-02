context("tnrs sources")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("tnrs_sources", {
    sources <- TNRS_sources(
      url = url,
      skip_internet_check = TRUE
    )
  })


  expect_s3_class(sources, "data.frame")
  expect_gt(object = nrow(sources), expected = 1)
})
