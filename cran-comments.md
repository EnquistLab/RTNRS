## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes
* The examples may take more than 5 seconds to run depending on internet speed and how busy the API is. The slowdown is on the API side, and not the R side of things. To be safe, I've moved testing of examples to testing via testthat

* Per comments on the previous submission, I've removed the example from the documentation of an unexported function.

* Per comments on the previous submissions, I've added links to the webservices and the underlying Github repository to the description field of the description file.