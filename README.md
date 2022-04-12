
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Optimum Sample Allocation in Stratified Sampling Schemes with Stratallo Package

<!-- badges: start -->
<!-- badges: end -->

Functions in this package provide solution to classical problem in
survey methodology - an optimum sample allocation in stratified sampling
schemes. In this context, the optimal allocation is in the classical
Tschuprov-Neyman’s sense and it satisfies additional either lower or
upper bounds restrictions imposed on sample sizes in strata. There are
few different algorithms available to use, and one them is based on
popular sample allocation method that applies Neyman allocation to
recursively reduced set of strata.

A minor modification of the classical optimium sample allocation problem
leads to the minimum sample size allocation. This problems lies in the
determination of a vector of strata sample sizes that minimizes total
sample size, under assumed fixed level of the pi-estimator’s variance.
As in the case of the classical optimal allocation, the problem of
minimum sample size allocation can be complemented by imposing upper
bounds constraints on sample sizes in strata.

*Stratallo* provides two user functions, `dopt` and `nopt` that solve
sample allocation problems briefly characterized above. In this context,
it is assumed that the sampling designs in strata are chosen so that the
variance of the pi-estimator of the population total is of the following
generic form:

<center>
D<sup>2</sup><sub style='position: relative; left: -.5em;'>st</sub>(x<sub>1</sub>,…,x<sub>H</sub>)
= a<sup>2</sup><sub style='position: relative; left: -.5em;'>1</sub>/
x<sub>1</sub> + … +
a<sup>2</sup><sub style='position: relative; left: -.5em;'>H</sub>/
x<sub>H</sub> - b,
</center>

where H denotes total number of strata, x<sub>1</sub>,…,x<sub>H</sub>
are the strata sample sizes, and parameters b and a<sub>w</sub> \> 0 do
not depend on x<sub>w</sub>, w = 1,…,H.

Apart from `dopt` and `nopt`, *stratallo* provides `var_tst` and
`var_tst_si` functions that compute a value of variance
D<sup>2</sup><sub style='position: relative; left: -.5em;'>st</sub>. The
`var_tst_si` is a simple wrapper of `var_tst` that is dedicated for the
case of simple random sampling without replacement design in each
stratum. Furthermore, the package comes with two predefined, artificial
populations with 507 and 969 strata. These are stored in `pop507` and
`pop969` objects respectively.

See package’s vignette for more details.

## Installation

You can install the released version of *stratallo* package from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("stratallo")
```

## Examples

These are basic examples that show how to use `dopt` and `nopt`
functions to solve optimal sample allocation problems for an example
population with 4 strata.

``` r
library(stratallo)
```

### Function `dopt`

``` r
# Example population.
N <- c(3000, 4000, 5000, 2000) # Strata sizes.
S <- c(48, 79, 76, 17) # Standard deviations of a study variable in strata.
a <- N * S
```

``` r
M <- c(100, 90, 70, 80) # Upper bounds constraints imposed on sample sizes in strata.
all(M <= N)
n <- 190 # Total sample size.
n < sum(M)

# Optimal allocation under one-sided upper bounds constraints.
opt <- dopt(n = n, a = a, M = M)
opt
sum(opt) # Equals to n.
all(opt <= M) # Does not violate upper bounds constraints.
# Variance of the pi-estimator that corresponds to a given optimal allocation.
var_tst_si(opt, N, S)
```

``` r
m <- c(50, 120, 1, 1) # Lower bounds constraints imposed on sample sizes in strata.
n > sum(m)

# Optimal allocation under one-sided lower bounds constraints.
opt <- dopt(n = n, a = a, m = m)
opt
sum(opt) # Equals to n.
all(opt >= m) # Does not violate lower bounds constraints.
# Variance of the pi-estimator that corresponds to a given optimal allocation.
var_tst_si(opt, N, S)
```

``` r
# Tschuprov-Neyman allocation (no inequality constraints).
opt <- dopt(n = n, a = a)
opt
sum(opt) # Equals to n.
# Variance of the pi-estimator that corresponds to a given optimal allocation.
var_tst_si(opt, N, S)
```

### Function `nopt`

``` r
a <- c(3000, 4000, 5000, 2000)
b <- 70000
M <- c(100, 90, 70, 80)
D <- 1e6 # Variance constraint.

opt <- nopt(D, a, b, M)
sum(opt)
```
