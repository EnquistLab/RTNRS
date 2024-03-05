## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.3.0
* win-builder
* macOS builder
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Notes

* Fedora Linux and Ubuntu Linux both return errors reading "checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found", which I believe are unrelated to this package.

* Windows Server returns errors reading 
  "* checking for detritus in the temp directory ... NOTE Found the following files/directories:'lastMiKTeXException'",
  "* checking for non-standard things in the check directory ... NOTE Found the following files/directories: ''NULL'", which I believe are also unrelated to this package.
