# SimpleGreedy and CapacityScaling Algorithms

**\[experimental\]**

Fast integer-valued algorithms for optimum allocations under constraints
in stratified sampling proposed in Friedrich et al. (2015) .

## Usage

``` r
SimpleGreedy2(v0, Nh, Sh, mh = rep(1, length(Nh)), Mh = Nh, nh = mh)

CapacityScaling2(
  v0,
  Nh,
  Sh,
  mh = rep(1, length(Nh)),
  Mh = rep(Inf, length(Nh))
)
```

## Arguments

- v0:

  (`numeric(1)`)  
  upper bound on the variance of the estimator.

- Nh:

  (`numeric`)  
  population sizes in strata.

- Sh:

  (`numeric`)  
  standard deviations of the study variable in strata.

- mh:

  (`integerish`)  
  lower bounds on stratum sample sizes.

- Mh:

  (`integerish`)  
  upper bounds on stratum sample sizes.

- nh:

  (`integerish`)  
  initial allocation. Defaults to `mh`.

- n:

  (`integerish(1)`)  
  total sample size.

- Ah:

  (`numeric`)  
  products of population stratum sizes and standard deviations of the
  study variable, \\A_h = N_h S_h\\.

## Value

For the
[`fpia()`](https://wwojciech.github.io/junco/reference/fpia.md) - an
integer vector of optimum sample sizes allocated to each stratum.

## Functions

- `SimpleGreedy2()`: Variant of the *SimpleGreedy* algorithm based on a
  variance stopping rule.

- `CapacityScaling2()`: Variant of the *CapacityScaling* algorithm based
  on a variance stopping rule.

## References

Friedrich U, Münnich R, de Vries S, Wagner M (2015). “Fast
integer-valued algorithms for optimal allocations under constraints in
stratified sampling.” *Computational Statistics & Data Analysis*,
**92**, 1-12. ISSN 0167-9473.
[doi:10.1016/j.csda.2015.06.003](https://doi.org/10.1016/j.csda.2015.06.003)
.
