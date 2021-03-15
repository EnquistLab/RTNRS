context("tnrs citations")


test_that("example works", {

  TNRS_version_metadata <- TNRS_version()
  
  expect_equal(object = class(TNRS_version_metadata), expected = "data.frame")
  expect_equal(object = nrow(TNRS_version_metadata),expected = 1)

    
})
