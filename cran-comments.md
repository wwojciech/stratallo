## Resubmission

This is a regular submission of the minor version of the package, where few minor
issues were corrected.

## Test environments

* macOS R 4.3.0
* https://win-builder.r-project.org/
* https://mac.r-project.org/macbuilder/submit.html.
* Rhub:
  - Windows Server 2022, R-release, 32/64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no ERRORs, WARNINGs. The following 4 NOTEs have been reported by
`devtools::check_rhub`. The surname Tschuprow is correctly spelled and the URL
is accessible.

```
─  checking CRAN incoming feasibility ... [15s] NOTE
   Maintainer: 'Wojciech Wójciak <wojciech.wojciak@gmail.com>'
   
   Possibly misspelled words in DESCRIPTION:
     Tschuprow (10:60)
   
   Found the following (possibly) invalid URLs:
     URL: https://www.sciencedirect.com/science/article/pii/S0167947315001413
       From: man/CapacityScaling.Rd
             man/SimpleGreedy.Rd
       Status: 403
       Message: Forbidden

─  checking PDF version of manual ... [12s] OK
N  checking HTML version of manual
   Skipping checking math rendering: package 'V8' unavailable
   Found the following files/directories:
     ''NULL''
N  checking for non-standard things in the check directory
N  checking for detritus in the temp directory
   Found the following files/directories:
     'lastMiKTeXException'
```
