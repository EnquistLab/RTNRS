context("tnrs base")


test_that("example works", {
  
  #skip_if_offline(host = "r-project.org")
  
  
  vcr::use_cassette("gnrs_base",
                    { results <- TNRS:::.TNRS_base(taxonomic_names = tnrs_testfile)})

  #test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = nrow(tnrs_testfile))

  
})
