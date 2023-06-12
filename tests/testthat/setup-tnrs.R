# api urls

# URL for TNRS API
# url = "https://tnrsapidev.xyz/tnrs_api.php"
# url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" # Dev (vegbiendev)
# url = http://vegbiendev.nceas.ucsb.edu:9975/tnrs_api.php #public development
url <- "https://tnrsapi.xyz/tnrs_api.php" # production

# Bad URLs for testing

# url <- "www.google.com"
# url <- "www.hisstank.com"

library("vcr") # *Required* as vcr is set up on loading

invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()
