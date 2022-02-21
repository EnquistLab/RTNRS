## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.1.2
* win-builder (devel and release)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Notes
* Previous version on CRAN was archived when an API issue caused tests to produce errors
* To address these issues, we've modified our functions so that API issues or connection issues result in a message and return NULL (invisibly).
* We've also updated the packag to use vcr so that testing can be done offline.