## Resubmission

This is a quick re-submission of the previous major release. Please accept it
even though it is the next submission after yesterday's version, but it contains
some major typo corrections which I would like to get corrected. This is 
particularly important as I will present this package on international 
statistical conference very soon.

## Test environments

* macOS R 4.3.0
* https://win-builder.r-project.org/
* https://mac.r-project.org/macbuilder/submit.html.
* Rhub:
  - Windows Server 2022, R-release, 32/64 bit
  - Debian Linux, R-release, GCC 

## R CMD check results

There were no ERRORs, WARNINGs. The following 4 NOTEs have been reported by
`devtools::check_rhub`. The surname Tschuprow is correctly spelled. There is
no any mistake here.

```
* checking CRAN incoming feasibility ... [13s] NOTE
Maintainer: 'Wojciech Wójciak <wojciech.wojciak@gmail.com>'
Days since last update: 1


Possibly misspelled words in DESCRIPTION:
  Tschuprow (10:60)

Found the following (possibly) invalid URLs:
  URL: https://www.sciencedirect.com/science/article/pii/S0167947315001413
    From: man/CapacityScaling.Rd
          man/SimpleGreedy.Rd
    Status: 403
    Message: Forbidden
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
