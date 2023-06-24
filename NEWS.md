# stratallo

# stratallo 2.2.0

* Added new user functions: `ran_round()` and `round_oric()` for rounding of
  (non-integer) numbers.
* Renamed `var_tst_si()` function to `var_st_tsi()`.
* Renamed `var_tst()` function to `var_st()`.
* Added `unit_cost` variable into `pop507`, `pop969` and `pop10_mM` datasets. 
* Renamed `allocation_summary()` function to `asummary()`.
* Added survey stratum cost parameter `unit_costs` to `optcost()` function. 
* Added survey stratum cost parameter `unit_costs` to `rna()` function.
* Renamed parameter `n` to `total_cost` in `rna()` function.
* Renamed `dopt()` function to `opt()`.
* Renamed `nopt()` function to `optcost()`.
* In `nopt()` function, changed the name of two parameters: `D` to `V` and
  `b` to `a0`.
* Significantly updated help pages for most of the functions and the vignette.
* Minor change in the implementation of the `rna()`. Vector `Ri` of take-bound
  indices is now a logical vector instead of the integer vector.
* Removed `h_get_which_violates()` helper as it is not needed anymore
* Added new argument `check_violations` to `rna()` function which replaces
  `h_get_which_violates()` approach.
* Changed the parameter name from `b` to `a0` in `var_tst_si()` function.
* Renamed `rna_onesided()` function to `rna()`.

# stratallo 2.1.1

* Minor typos corrections in manual pages.

# stratallo 2.1.0

* Added new function `allocation_summary()` that summarizes the allocation.
* Added new function `rnabox()` to handle box-constraints case.
* Updated `rna_onesided()` so that it optionally returns take-Neyman, take-min,
  take-max set of strata indices.
* Renamed function `rna_one_sided()` to `rna_onesided()`.
* Minor typos corrections in README, vignette.

# stratallo 2.0.1

* Minor typos corrections in README, vignette, and help pages.

# stratallo 2.0.0

* Added new user function `nopt()` for minimization of total sample size under
  constraints on variance and upper bounds on strata sample sizes. 
* Re-factored `rNa()` into `rna_one_sided()` so that it also computes the
  allocation under optional lower bounds constraints.
* Renamed `nopt()` do `dopt()` to reflect the fact that it minimizes variance
  function D.
  
# stratallo 0.1.0

* First release.
