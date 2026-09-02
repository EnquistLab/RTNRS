context("tnrs citations")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("tnrs_citations", {
    citation_info <- TNRS_citations(
      url = url,
      skip_internet_check = TRUE
    )
  })


  expect_s3_class(citation_info, "data.frame")
  expect_gt(object = nrow(citation_info), expected = 2)
})
