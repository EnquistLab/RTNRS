## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

* The Fedora Linux checks produce an error.  This error is due to unavailability of the suggested package BIEN. My guess is that BIEN is unavailable due to its dependency on rgdal, as I've often encountered gdal related errors in the linux testing environments when submitting BIEN updates to CRAN.
