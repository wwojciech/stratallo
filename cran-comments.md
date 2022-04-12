## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Wojciech Wojciak <wojciech.wojciak@gmail.com>'
  
  Possibly misspelled words in DESCRIPTION:
    Tschuprov (11:3)
    optimium (16:41)
    stratallo (23:15)
  
  Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1093/jssam/smab018
      From: man/dopt.Rd
            man/dopt_upper.Rd
            man/rna_one_sided.Rd
            man/stratallo-package.Rd
            inst/doc/stratallo.html
      Status: 403
      Message: Forbidden

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✓ | 0 warnings ✓ | 2 notes x

* This is a new release.
* https://doi.org/10.1093/jssam/smab018 is accessible.
* Tschuprov, optimium, stratallo are not misspelled.
