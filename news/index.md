# Changelog

## stratallo 3.0.0

CRAN release: 2026-03-12

- Added new functions for controlled-precision domain allocation,
  including
  [`dopt()`](https://wwojciech.github.io/junco/reference/dopt.md).
- Renamed argument in
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md):
  - `check_violations` -\> `cmp`.
- Renamed arguments in
  [`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md):
  - `bounds1` -\> `bounds_inner`,
  - `bounds2` -\> `bounds_outer`,
  - `check_violations1` -\> `cmp_inner`,
  - `check_violations2` -\> `cmp_outer`.
- Renamed artificial population datasets.
- Renamed `var_st_tsi()` function to
  [`var_stsi()`](https://wwojciech.github.io/junco/reference/var_st.md).
- Renamed `asummary()` function to
  [`alloc_summary()`](https://wwojciech.github.io/junco/reference/alloc_summary.md).

## stratallo 2.2.1

CRAN release: 2023-11-26

- Changed the name of two parameters: `a` to `A` and `a0` to `A0` for
  several functions.
- Updated
  [`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md) so
  that it can perform both versions of the RNABOX algorithm, i.e.: step
  1 handles one-sided upper bounds whilst step 2 handles one-sided lower
  bounds, or conversely, depending on the user preferences.
- Minor help and vignette updates, corrected typos from previous
  release.

## stratallo 2.2.0

CRAN release: 2023-06-24

- Added new user functions: `ran_round()` and
  [`round_oric()`](https://wwojciech.github.io/junco/reference/rounding.md)
  for rounding of (non-integer) numbers.
- Renamed `var_tst_si()` function to `var_st_tsi()`.
- Renamed `var_tst()` function to
  [`var_st()`](https://wwojciech.github.io/junco/reference/var_st.md).
- Added `unit_cost` variable into `pop507`, `pop969` and `pop10_mM`
  datasets.
- Renamed `allocation_summary()` function to `asummary()`.
- Added survey stratum cost parameter `unit_costs` to
  [`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md)
  function.
- Added survey stratum cost parameter `unit_costs` to
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  function.
- Renamed parameter `n` to `total_cost` in
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  function.
- Renamed
  [`dopt()`](https://wwojciech.github.io/junco/reference/dopt.md)
  function to
  [`opt()`](https://wwojciech.github.io/junco/reference/opt.md).
- Renamed `nopt()` function to
  [`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md).
- In `nopt()` function, changed the name of two parameters: `D` to `V`
  and `b` to `a0`.
- Significantly updated help pages for most of the functions and the
  vignette.
- Minor change in the implementation of the
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md).
  Vector `Ri` of take-bound indices is now a logical vector instead of
  the integer vector.
- Removed `h_get_which_violates()` helper as it is not needed anymore
- Added new argument `check_violations` to
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  function which replaces `h_get_which_violates()` approach.
- Changed the parameter name from `b` to `a0` in `var_tst_si()`
  function.
- Renamed `rna_onesided()` function to
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md).

## stratallo 2.1.1

- Minor typos corrections in manual pages.

## stratallo 2.1.0

CRAN release: 2022-09-13

- Added new function `allocation_summary()` that summarizes the
  allocation.
- Added new function
  [`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md) to
  handle box-constraints case.
- Updated `rna_onesided()` so that it optionally returns take-Neyman,
  take-min, take-max set of strata indices.
- Renamed function `rna_one_sided()` to `rna_onesided()`.
- Minor typos corrections in README, vignette.

## stratallo 2.0.1

CRAN release: 2022-04-13

- Minor typos corrections in README, vignette, and help pages.

## stratallo 2.0.0

CRAN release: 2022-04-12

- Added new user function `nopt()` for minimization of total sample size
  under constraints on variance and upper bounds on strata sample sizes.
- Re-factored `rNa()` into `rna_one_sided()` so that it also computes
  the allocation under optional lower bounds constraints.
- Renamed `nopt()` do
  [`dopt()`](https://wwojciech.github.io/junco/reference/dopt.md) to
  reflect the fact that it minimizes variance function D.

## stratallo 0.1.0

CRAN release: 2020-05-25

- First release.
