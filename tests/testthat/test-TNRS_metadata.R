context("tnrs citations")


test_that("example works", {
  
  #skip_if_offline(host = "r-project.org")

  vcr::use_cassette("gnrs_metadata",
                    {   metadata <- TNRS_metadata()})

  #test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(metadata) == "list")
  
  expect_equal(object = class(metadata[[1]]), expected = "data.frame")
  expect_equal(object = class(metadata[[2]]), expected = "data.frame")
  expect_equal(object = class(metadata[[3]]), expected = "data.frame")
  
  expect_equal(object = length(metadata),expected = 3)
    
})
