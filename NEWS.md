# stratallo

# stratallo 2.1.0

* Added new function `allocation_summary` that summarizes the allocation.
* Added new function `rnabox` to handle box-constraints case.
* Updated `rna_onesided` so that it optionally returns take-Neyman, take-min,
  take-max set of strata indices.
* Renamed function `rna_one_sided to `rna_onesided`.  
* Minor typos corrections in README, vignette.

# stratallo 2.0.1

* Minor typos corrections in README, vignette, and help pages.

# stratallo 2.0.0

* Added new user function `nopt` for minimization of total sample size under
  constraints on variance and upper bounds on strata sample sizes. 
* Re-factored `rNa` into `rna_one_sided` so that it also computes the allocation
  under optional lower bounds constraints.
* Renamed `nopt` do `dopt` to reflect the fact that it minimizes variance
  function D.
  
# stratallo 0.1.0

* First release.
