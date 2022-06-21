## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.1.2
* win-builder (devel and release)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Notes
* This is a minor update to address problems caused by a recent change in R which exposed some character encoding bugs which impact vcr, which TNRS uses in testing. The problem seems to be caused by some non-ascii characters in an example file, so we've modified the example data to omit these non-ascii characters.