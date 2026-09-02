context("tnrs version")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("tnrs_version", {
    TNRS_version_metadata <- TNRS_version(
      url = url,
      skip_internet_check = TRUE
    )
  })


  expect_s3_class(TNRS_version_metadata, "data.frame")
  expect_equal(object = nrow(TNRS_version_metadata), expected = 1)
})
