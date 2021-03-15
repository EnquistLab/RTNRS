context("tnrs citations")


test_that("example works", {

  citation_info <- TNRS_citations()

  expect_equal(object = class(citation_info), expected = "data.frame")
  expect_gt(object = nrow(citation_info),expected = 2)
  
})
