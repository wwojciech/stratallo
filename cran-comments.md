## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Wojciech Wójciak <wojciech.wojciak@gmail.com>'
  
  Days since last update: 0
  
  New maintainer:
    Wojciech Wójciak <wojciech.wojciak@gmail.com>
  Old maintainer(s):
    Wojciech Wojciak <wojciech.wojciak@gmail.com>
  
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

* This is a new release, that fixes typos in maintainer surname, help pages,
  README, and vignette. 
* https://doi.org/10.1093/jssam/smab018 is accessible.
