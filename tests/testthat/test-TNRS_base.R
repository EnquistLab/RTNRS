context("tnrs base")


test_that("example works", {
  
  skip_if_offline(host = "r-project.org")

  results <- TNRS:::.TNRS_base(taxonomic_names = tnrs_testfile)
  
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results), expected = nrow(tnrs_testfile))

  
})
