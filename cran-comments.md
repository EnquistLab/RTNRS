## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.1.2
* win-builder (devel and release)
* macOS builder
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Notes
* Package was archived due to errors in testing caused by a change in R which exposed some character encoding bugs which impacted vcr, which TNRS uses in testing. The problem seems to be caused by some non-ascii characters in an example file, so we've modified the example data to omit these non-ascii characters.

* The Windows server rhub build returns the note "Found the following files/directories: 'lastMiKTeXException'", which I'm ignoring per https://github.com/r-hub/rhub/issues/503

## Response to previous comments
*Previous version caused a note on Debian, presumably due to a change in how the news file is rendered. I've updated the formatting in the news file to fix this.