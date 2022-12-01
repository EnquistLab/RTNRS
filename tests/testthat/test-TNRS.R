context("tnrs main")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("tnrs_example", {
    results <- TNRS(
      taxonomic_names = tnrs_testfile,
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = nrow(tnrs_testfile))
})

test_that("character vector returns results", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("tnrs_character_vector", {
    results <- TNRS(
      taxonomic_names = c("Acer rubrum", "Xanthium strumarium", "Abies alba"),
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 3)
})

test_that("character returns results", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("tnrs_character", {
    results <- TNRS(
      taxonomic_names = c("Epidendrum boyleii"),
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 1)
})


test_that("all sources work", {
  # skip_if_offline(host = "r-project.org")



  species <- c("Epidenrum boyleii", "Acer rubrum", "Xanthium strumarium", "Abies alba")

  # WCVP

  vcr::use_cassette("tnrs_wcvp", {
    results <- TNRS(
      taxonomic_names = species,
      sources = "wcvp",
      url = url,
      skip_internet_check = TRUE
    )
  })

  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 4)

  # WFO

  vcr::use_cassette("tnrs_wfo", {
    results <- TNRS(
      taxonomic_names = species,
      sources = "wfo",
      url = url,
      skip_internet_check = TRUE
    )
  })

  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 4)


  # USDA

  vcr::use_cassette("tnrs_usda", {
    results <- TNRS(
      taxonomic_names = species,
      sources = "usda",
      url = url,
      skip_internet_check = TRUE
    )
  })

  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 4)


  # USDA

  vcr::use_cassette("tnrs_tropicos", {
    results <- TNRS(
      taxonomic_names = species,
      sources = "tropicos",
      url = url,
      skip_internet_check = TRUE
    )
  })

  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 4)
})

test_that("bad sources throw a message and return null", {
  skip_if_offline(host = "r-project.org")
  skip_on_cran()


  expect_message(object = TNRS(
    taxonomic_names = "Optimus Prime",
    sources = "Teletraan-1",
    url = url,
    skip_internet_check = TRUE
  ))

  expect_null(object = suppressMessages(TNRS(
    taxonomic_names = "Optimus Prime",
    sources = "Teletraan-1",
    url = url,
    skip_internet_check = TRUE
  )))
})


test_that("matches all returns more rows than best", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("tnrs_all", {
    all <- TNRS(
      taxonomic_names = "Epidendon boyleii",
      matches = "all",
      url = url,
      skip_internet_check = TRUE
    )
  })

  vcr::use_cassette("tnrs_best", {
    best <- TNRS(
      taxonomic_names = "Epidendon boyleii",
      matches = "best",
      url = url,
      skip_internet_check = TRUE
    )
  })

  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(all) == "data.frame")
  skip_if_not(class(best) == "data.frame")

  expect_gt(object = nrow(all), expected = nrow(best))
})

test_that("parsing returns matches", {
  # skip_if_offline(host = "r-project.org")

  # Check that resolving names returns more names than matching

  vcr::use_cassette("tnrs_parsed", {
    parsed <- TNRS(
      taxonomic_names = "Epidentrum boyleii",
      mode = "parse",
      url = url,
      skip_internet_check = TRUE
    )
  })

  vcr::use_cassette("tnrs_resolved", {
    resolved <- TNRS(
      taxonomic_names = "Epidentrum boyleii",
      mode = "resolve",
      url = url,
      skip_internet_check = TRUE
    )
  })

  skip_if_not(class(parsed) == "data.frame")
  skip_if_not(class(resolved) == "data.frame")


  expect_gt(object = ncol(resolved), expected = ncol(parsed))


  # Check for matching number of names between supplied and parsed names

  species <- c("Epidenrum boyleii", "Acer rubrum", "Xanthium strumarium", "Abies alba")

  vcr::use_cassette("tnrs_parsed_2", {
    parsed <- TNRS(
      taxonomic_names = species,
      mode = "parse",
      url = url,
      skip_internet_check = TRUE
    )
  })

  skip_if_not(class(parsed) == "data.frame")

  expect_equal(
    object = nrow(parsed),
    expected = length(species)
  )
})


test_that("changing accuracy changes results", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("tnrs_high", {
    high <- TNRS(
      taxonomic_names = "Brad boyle",
      accuracy = 0.99,
      matches = "all",
      url = url,
      skip_internet_check = TRUE
    )
  })

  vcr::use_cassette("tnrs_low", {
    low <- TNRS(
      taxonomic_names = "Brad boyle",
      accuracy = 0.01,
      matches = "all",
      url = url,
      skip_internet_check = TRUE
    )
  })

  skip_if_not(class(high) == "data.frame")
  skip_if_not(class(low) == "data.frame")


  expect_gt(object = nrow(low), expected = nrow(high))
})
