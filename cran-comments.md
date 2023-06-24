## Resubmission

This is a re-submission. In this version I have updated and extended some existing
algorithms and significantly updated help pages. In particular, I extended the
`rna()` function so that it now supports specification of the unit costs.
Also, two helpers functions for rounding were added.

## Test environments

* macOS R 4.3.0
* https://win-builder.r-project.org/
* https://mac.r-project.org/macbuilder/submit.html.
* Rhub:
  - Windows Server 2022, R-release, 32/64 bit
  - Debian Linux, R-release, GCC 

## R CMD check results

There were no ERRORs, WARNINGs. The following 4 NOTEs have been reported by
`devtools::check_rhub`.

```
* checking CRAN incoming feasibility ... [18s] NOTE
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
