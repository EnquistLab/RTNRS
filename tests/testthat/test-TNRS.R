context("tnrs citations")


test_that("example works", {

  
  results <- TNRS(taxonomic_names = tnrs_testfile)
  
  
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = nrow(tnrs_testfile))

    
})


test_that("character vector returns results", {
  
  results  <- TNRS(taxonomic_names = c("Acer rubrum", "Xanthium strumarium", "Abies alba"))
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = 3)
  
  
  })
  
test_that("character returns results", {
  
  results  <- TNRS(taxonomic_names = c("Epidendrum boyleii"))
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = 1)
  
  
})


test_that("all sources work", {
  
  species <- c("Epidenrum boyleii","Acer rubrum", "Xanthium strumarium", "Abies alba")
  
  results  <- TNRS(taxonomic_names = species,sources = "tpl")
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = 4)
  
  results  <- TNRS(taxonomic_names = species,sources = "usda")
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = 4)
  
  results  <- TNRS(taxonomic_names = species,sources = "tropicos")
  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results),expected = 4)
  
  
})

test_that("bad sources throw an error", {
  
  expect_error(object = TNRS(taxonomic_names = "Optimus Prime",sources = "Teletraan-1"))  
  
})
  

test_that("matches all returns more rows than best", {
  
  all <- TNRS(taxonomic_names = "Epidendon boyleii",matches = "all")
  best <- TNRS(taxonomic_names = "Epidendon boyleii",matches = "best")
  
  expect_gt(object = nrow(all),expected = nrow(best))
    
  
})

test_that("parsing returns matches", {
  
  parsed <- TNRS(taxonomic_names = "Epidentrum boyleii", mode = "parse")
  resolved <- TNRS(taxonomic_names = "Epidentrum boyleii", mode = "resolve")

  expect_gt(object = ncol(resolved),expected = ncol(parsed))
  
  species <- c("Epidenrum boyleii","Acer rubrum", "Xanthium strumarium", "Abies alba")
  parsed <- TNRS(taxonomic_names = species)
  expect_equal(object = nrow(parsed),expected = length(species))
  

})


test_that("changing accuracy changes results", {
  
  high <- TNRS(taxonomic_names = "Brad boyle", accuracy = 0.99, matches = "all")
  low <- TNRS(taxonomic_names = "Brad boyle", accuracy = 0.01, matches = "all")
  
  expect_gt(object = nrow(low), expected = nrow(high))
  

  
})



