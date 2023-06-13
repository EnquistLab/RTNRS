## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.3.0
* win-builder (devel and release)
* macOS builder
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Notes
* Fedora Linux and Ubuntu Linux both return errors reading "checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found", which I beleive are unrelated to this package.
* Windows Server returns an error reading "* checking for detritus in the temp directory ... NOTE
Found the following files/directories:'lastMiKTeXException'", which I beleive is also unrelated to this package.