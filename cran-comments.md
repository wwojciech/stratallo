## Resubmission

This is a re-submission. In this version I have added a new allocation algorithm `rnabox`.

## Test environments

* macOS R 4.1.2
* win-builder (devel)
* Rhub

## R CMD check results

There were no ERRORs or WARNINGs. There was one note related to the link below.
The https://doi.org/10.1093/jssam/smab018 link is however valid and accessible.
I'd prefer to keep this link in the documentation if possible.

```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1093/jssam/smab018
    From: man/dopt.Rd
          man/dopt_upper.Rd
          man/rna_onesided.Rd
          man/stratallo-package.Rd
          inst/doc/stratallo.html
    Status: 403
    Message: Forbidden
Maintainer: 'Wojciech Wójciak <wojciech.wojciak@gmail.com>'
```
