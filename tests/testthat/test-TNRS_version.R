context("tnrs citations")


test_that("example works", {

  #skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("gnrs_citations",
                    { TNRS_version_metadata <- TNRS_version()})
  
  
  #test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(TNRS_version_metadata) == "data.frame")
  expect_equal(object = nrow(TNRS_version_metadata),expected = 1)

    
})
