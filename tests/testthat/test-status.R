context("server status and failure reporting")

test_that("a failure says which kind it was, and what to do", {
  url <- "https://tnrsapi.xyz/tnrs_api.php"

  timeout <- tnrs_request_failure(
    "Timeout was reached: [tnrsapi.xyz] Operation timed out after 30000 ms", url, 30
  )
  expect_match(timeout, "did not answer within 30 seconds", fixed = TRUE)
  # A timeout on a big batch is often a size problem, not an outage
  expect_match(timeout, "timeout = 900", fixed = TRUE)

  dns <- tnrs_request_failure("Could not resolve host: tnrsapi.xyz", url)
  expect_match(dns, "could not be looked up", fixed = TRUE)
  expect_match(dns, "your own connection", fixed = TRUE)

  # The reporter of issue 20 tried the certificate workaround by guesswork,
  # because nothing said whether it applied
  ssl <- tnrs_request_failure("SSL certificate problem: unable to get local issuer", url)
  expect_match(ssl, "could not be secured", fixed = TRUE)
  expect_match(ssl, "issues/7", fixed = TRUE)

  refused <- tnrs_request_failure("Failed to connect to tnrsapi.xyz port 443: Connection refused", url)
  expect_match(refused, "refused the connection", fixed = TRUE)
  expect_match(refused, "probably down", fixed = TRUE)

  # Anything unrecognised still names the endpoint and passes the reason on
  other <- tnrs_request_failure("something new and strange", url)
  expect_match(other, url, fixed = TRUE)
  expect_match(other, "something new and strange", fixed = TRUE)
})

test_that("every failure repeats what the connection actually reported", {
  # The original message discarded the condition, which is why a timeout and a
  # certificate problem were indistinguishable
  for (reason in c("Timeout was reached", "Could not resolve host: x",
                   "SSL certificate problem", "Connection refused", "unknown")) {
    expect_match(
      tnrs_request_failure(reason, "https://example.org", 10),
      "Reported by the connection", fixed = TRUE
    )
  }
})

test_that("no connection is reported as such, not as a server fault", {
  env <- new.env(parent = asNamespace("TNRS"))
  env$check_internet <- function() FALSE
  status <- TNRS_status
  environment(status) <- env

  expect_message(out <- status(quiet = FALSE), "no connection detected", fixed = TRUE)
  expect_false(out$internet)
  expect_false(out$reachable)
  expect_true(is.na(out$http_status))
})
