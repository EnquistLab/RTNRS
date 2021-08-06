context("tnrs citations")


test_that("example works", {

  skip_if_offline(host = "r-project.org")  
  
  sources <- TNRS_sources()
  
  expect_equal(object = class(sources), expected = "data.frame")
  expect_gt(object = nrow(sources),expected = 2)

    
})
