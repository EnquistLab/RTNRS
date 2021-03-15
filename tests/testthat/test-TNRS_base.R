context("tnrs base")


test_that("example works", {
  
  tnrs_testfile <- 
  read.csv(system.file("extdata", "tnrs_testfile.csv", package = "TNRS", mustWork = TRUE),
  stringsAsFactors = FALSE)
  
  results <- TNRS:::.TNRS_base(taxonomic_names = tnrs_testfile)
  
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results), expected = nrow(tnrs_testfile))

  
})
