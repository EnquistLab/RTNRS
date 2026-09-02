context("tnrs metadata")


test_that("example works", {
  # because of the way this call is structured, I can't capture it via vcr, so I skip it on cran/offline
  skip_if_offline(host = "r-project.org")
  skip_on_cran()

  metadata <- TNRS_metadata()

  expect_type(metadata, "list")

  expect_equal(object = class(metadata[[1]]), expected = "data.frame")
  expect_equal(object = class(metadata[[2]]), expected = "data.frame")
  expect_equal(object = class(metadata[[3]]), expected = "data.frame")

  expect_equal(object = length(metadata), expected = 3)
})
