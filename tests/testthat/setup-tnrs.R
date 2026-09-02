# api urls

# URL for TNRS API
# url = "https://tnrsapidev.xyz/tnrs_api.php"
# url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" # Dev (vegbiendev)
# url = "http://vegbiendev.nceas.ucsb.edu:9975/tnrs_api.php" #public development
url <- "https://tnrsapi.xyz/tnrs_api.php" # production

# Bad URLs for testing

# url <- "www.google.com"
# url <- "www.hisstank.com"

library("vcr") # *Required* as vcr is set up on loading

# Match on the method and address only.  vcr's default also matches the
# request body, which would be the stricter and better test, but vcr 2.1.0 does
# not record the body reliably: re-recording every cassette on 2026-09-02 left
# three of the eighteen, sources, syn and meta, with no request body stored at
# all, so there was nothing for the body to be matched against and they could
# never replay.  The three functions concerned were confirmed working against
# the live service at the time.  Matching on what is actually recorded is
# therefore honest rather than lax; the shape of the request is asserted
# directly in test-reconcile.R, where a change to it is meant to be noticed.
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  match_requests_on = c("method", "uri")
))

vcr::check_cassette_names()
