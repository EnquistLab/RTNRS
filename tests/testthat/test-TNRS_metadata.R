context("tnrs citations")


test_that("example works", {
  
  skip_if_offline(host = "r-project.org")
  
  metadata <- TNRS_metadata()
  
  expect_equal(object = class(metadata), expected = "list")
  expect_equal(object = class(metadata[[1]]), expected = "data.frame")
  expect_equal(object = class(metadata[[2]]), expected = "data.frame")
  expect_equal(object = class(metadata[[3]]), expected = "data.frame")
  
  expect_equal(object = length(metadata),expected = 3)
    
})
