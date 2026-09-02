context("submitted names are checked and matched back")

# What the web service returns, in the shape the reported problems describe.
# Only the columns the reconciler touches are needed.
service_answer <- function(id, name, score = 1) {
  data.frame(
    ID = id, Name_submitted = name, Overall_score = score,
    Name_matched = name, stringsAsFactors = FALSE
  )
}

test_that("a data.frame of the wrong shape is explained, not sent", {
  # Reported as issue 13: three columns produced "HTTP Status 400"
  expect_error(
    tnrs_check_names(data.frame(ID = 1, name = "Helianthus annuus", other = "bla")),
    "exactly two columns",
    fixed = TRUE
  )
  # The message names the columns that are there, so the caller can see which
  # two to keep
  expect_error(
    tnrs_check_names(data.frame(ID = 1, name = "x", other = "bla")),
    "ID, name, other",
    fixed = TRUE
  )

  expect_error(tnrs_check_names(list(1, 2)), "should be a data.frame", fixed = TRUE)
  expect_error(
    tnrs_check_names(data.frame(name = "x")), "exactly two columns",
    fixed = TRUE
  )
  expect_error(
    tnrs_check_names(data.frame(a = character(0), b = character(0))),
    "no rows",
    fixed = TRUE
  )
  expect_error(tnrs_check_names(character(0)), "is empty", fixed = TRUE)
})

test_that("identifiers must be present and unique, since answers key on them", {
  expect_error(
    tnrs_check_names(data.frame(ID = c("a", ""), n = c("x", "y"))),
    "missing or empty",
    fixed = TRUE
  )
  expect_error(
    tnrs_check_names(data.frame(ID = c(1, 1, 2), n = c("a", "b", "c"))),
    "must be unique",
    fixed = TRUE
  )
})

test_that("a character vector is numbered, and a data.frame is taken as given", {
  from_vector <- tnrs_check_names(c("Quercus alba", "Acer rubrum"))
  expect_identical(from_vector$ID, c("1", "2"))
  expect_identical(from_vector$name, c("Quercus alba", "Acer rubrum"))

  from_frame <- tnrs_check_names(
    data.frame(ID = c("a", "b"), n = c("x", "y"), stringsAsFactors = FALSE)
  )
  expect_identical(from_frame$ID, c("a", "b"))
  # Factors are a common accident and should not become integer codes
  expect_identical(
    tnrs_check_names(factor(c("Quercus alba")))$name, "Quercus alba"
  )
})

test_that("blank names are held back and repeats are sent once", {
  submitted <- data.frame(
    ID = paste0("t-", 1:6),
    name = c("Helianthus", NA, "Helianthus", "", " ", "Quercus alba"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)

  # Upstream strips blank names, which is what shifts every later identifier,
  # so they never leave here
  expect_false(any(tnrs_is_blank_name(sent$name)))
  # And a name asked for twice is sent once
  expect_identical(sent$name, c("Helianthus", "Quercus alba"))
})

test_that("a name asked for twice comes back twice, under its own identifier", {
  # Reported as issue 15: the service answers with "test-2,test-1" in one row
  submitted <- data.frame(
    ID = c("test-1", "test-2"), name = c("Helianthus", "Helianthus"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  answer <- service_answer("1", "Helianthus")

  out <- tnrs_reconcile_results(answer, submitted, sent)

  expect_identical(nrow(out), 2L)
  expect_identical(out$ID, c("test-1", "test-2"))
  expect_identical(out$Name_matched, c("Helianthus", "Helianthus"))
})

test_that("an identifier the service pasted together is split apart", {
  # Sending each distinct name once should stop this arising, since the service
  # only combines rows that share a name.  Split anyway, so that a service that
  # combines them for some other reason cannot merge two callers' rows into one.
  submitted <- data.frame(
    ID = c("a", "b"), name = c("Helianthus", "Quercus alba"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  answer <- service_answer("1,2", "Helianthus")

  out <- tnrs_reconcile_results(answer, submitted, sent)
  expect_identical(nrow(out), 2L)
  expect_identical(out$ID, c("a", "b"))
  # Each identifier keeps the name that was submitted under it
  expect_identical(out$Name_submitted, c("Helianthus", "Quercus alba"))
})

test_that("a blank name is returned unmatched rather than dropped", {
  # Reported as issue 14: the empty string vanished and NA lost its identifier
  submitted <- data.frame(
    ID = paste0("test-", 1:4),
    name = c(NA, "Helianthus", "", " "),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  answer <- service_answer("1", "Helianthus")

  out <- tnrs_reconcile_results(answer, submitted, sent)

  expect_identical(nrow(out), 4L)
  expect_identical(out$ID, submitted$ID)
  # The one real name matched; the blanks came back empty but present
  expect_identical(out$Name_matched[2], "Helianthus")
  expect_true(all(is.na(out$Name_matched[c(1, 3, 4)])))
})

test_that("names stay on their own identifiers when the service drops a row", {
  # Reported as issue 16: a blank name shifted every later name onto the
  # identifier before it, silently
  submitted <- data.frame(
    ID = paste0("splot-", 1:6),
    name = c("Chlorophytum", "Echinochloa", "Polygala", "", "Species", "Fabaceae"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  # The service answers for the names it was sent, in its own order
  answer <- service_answer(sent$ID, sent$name)

  out <- tnrs_reconcile_results(answer, submitted, sent)

  expect_identical(out$ID, submitted$ID)
  expect_identical(out$Name_submitted, submitted$name)
  # Every name that was sent is answered against the identifier that asked
  matched <- out$Name_matched[!tnrs_is_blank_name(submitted$name)]
  expect_identical(matched, submitted$name[!tnrs_is_blank_name(submitted$name)])
})

test_that("a name the service simply did not answer is still accounted for", {
  submitted <- data.frame(
    ID = c("a", "b", "c"), name = c("Quercus alba", "Acer rubrum", "Pinus strobus"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  # Nothing came back for Acer rubrum
  answer <- service_answer(c("1", "3"), c("Quercus alba", "Pinus strobus"))

  out <- tnrs_reconcile_results(answer, submitted, sent)

  expect_identical(out$ID, c("a", "b", "c"))
  expect_true(is.na(out$Name_matched[2]))
  expect_identical(out$Name_submitted[2], "Acer rubrum")
})

test_that("several matches per name are all kept", {
  submitted <- data.frame(
    ID = c("a", "b"), name = c("Quercus alba", "Acer rubrum"),
    stringsAsFactors = FALSE
  )
  sent <- tnrs_request_frame(submitted)
  # matches = "all" answers a name more than once
  answer <- service_answer(c("1", "1", "2"),
                           c("Quercus alba", "Quercus alba", "Acer rubrum"))

  out <- tnrs_reconcile_results(answer, submitted, sent)
  expect_identical(nrow(out), 3L)
  expect_identical(out$ID, c("a", "a", "b"))
})
