
<!-- README.md is generated from README.Rmd. Please edit that file -->
# Optimum Sample Allocation in Stratified Sampling with `stratallo` Package

<!-- badges: start -->
<!-- badges: end -->

Functions in this package provide solution to classical problem in
survey methodology - an optimum sample allocation in stratified sampling
schemes. In this context, the optimal allocation is in the classical
Tschuprov-Neyman’s sense and it satisfies additional lower or upper
bounds restrictions imposed on sample sizes in strata. There are few
different algorithms available to use, and one them is based on popular
sample allocation method that applies Neyman allocation to recursively
reduced set of strata.

A minor modification of the classical optimum sample allocation problem
leads to the minimum cost allocation. This problem lies in the
determination of a vector of strata sample sizes that minimizes total
cost of the survey, under assumed fixed level of the stratified
$\pi$-estimator’s variance. As in the case of the classical optimum
allocation, the problem of minimum cost allocation can be complemented
by imposing upper bounds on sample sizes in strata.

*stratallo* provides two **user functions**:

- `opt()`
- `optcost()`

that solve sample allocation problems briefly characterized above. In
this context, it is assumed that the variance of the stratified
estimator is of the following generic form: $$
  V_{st}(\mathbf n) = \sum_{h=1}^{H} \frac{A_h^2}{n_h} - A_0,
$$ where $H$ denotes total number of strata,
$\mathbf n= (n_h)_{h \in \{1,\ldots,H\}}$ is the allocation vector with
strata sample sizes, and population parameters $A_0$, and
$A_h > 0,\, h = 1,\ldots,H$, do not depend on the
$x_h,\, h = 1,\ldots,H$.

Among stratified estimators and stratified sampling designs that jointly
give rise to a variance of the above form, is the so called stratified
$\pi$ estimator of the population total with *stratified simple random
sampling without replacement* design, which is one of the most basic and
commonly used stratified sampling designs. This case yields
$A_0 = \sum_{h = 1}^H N_h S_h^2$, $A_h = N_h S_h,\, h = 1,\ldots,H$,
where $S_h$ denotes stratum standard deviation of study variable and
$N_h$ is the stratum size (see e.g. @sarndal, Result 3.7.2, p.103).

Apart from `opt()` and `optcost()`, *stratallo* provides the following
**helpers functions**:

- `var_st()`,
- `var_st_tsi()`,
- `asummary()`,
- `ran_round()`,
- `round_oric()`.

Functions `var_st()` and `var_st_tsi()` compute a value of the variance
$V_{st}$. The `var_st_tsi()` is a simple wrapper of `var_st()` that is
dedicated for the case when $A_0 = \sum_{h = 1}^H N_h S_h^2$ and
$A_h = N_h S_h,\, h = 1,\ldots,H$. `asummary()` creates a `data.frame`
object with summary of the allocation. Functions `ran_round()` and
`round_oric()` are the rounding functions that can be used to round
non-integers allocations (see section Rounding, below). The package
comes with three predefined, artificial populations with 10, 507 and 969
strata. These are stored under `pop10_mM`, pop507`and`pop969\` objects,
respectively.

See package’s vignette for more details.

## Installation

You can install the released version of *stratallo* package from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("stratallo")
```

## Examples

These are basic examples that show how to use `opt()` and `optcost()`
functions to solve different versions of optimum sample allocation
problem for an example population with 4 strata.

``` r
library(stratallo)
```

Define example population

``` r
N <- c(3000, 4000, 5000, 2000) # Strata sizes.
S <- c(48, 79, 76, 16) # Standard deviations of a study variable in strata.
a <- N * S
n <- 190 # Total sample size.
```

Tschuprov-Neyman allocation (no inequality constraints).

``` r
opt <- opt(n = n, a = a)
opt
#> [1] 31.376147 68.853211 82.798165  6.972477
sum(opt) == n
#> [1] TRUE
# Variance of the stratified estimator that corresponds to optimum allocation.
var_st_tsi(opt, N, S)
#> [1] 3940753053
```

One-sided upper-bounds constraints.

``` r
M <- c(100, 90, 70, 80) # Upper bounds imposed on the sample sizes in strata.
all(M <= N)
#> [1] TRUE
n <= sum(M)
#> [1] TRUE

# Solution to Problem 1.
opt <- opt(n = n, a = a, M = M)
opt
#> [1] 35.121951 77.073171 70.000000  7.804878
sum(opt) == n
#> [1] TRUE
all(opt <= M) # Does not violate upper-bounds constraints.
#> [1] TRUE
# Variance of the stratified estimator that corresponds to optimum allocation.
var_st_tsi(opt, N, S)
#> [1] 4018789143
```

One-sided lower-bounds constraints.

``` r
m <- c(50, 120, 1, 2) # Lower bounds imposed on the sample sizes in strata.
n >= sum(m)
#> [1] TRUE

# Solution to Problem 2.
opt <- opt(n = n, a = a, m = m)
opt
#> [1]  50 120  18   2
sum(opt) == n
#> [1] TRUE
all(opt >= m) # Does not violate lower-bounds constraints.
#> [1] TRUE
# Variance of the stratified estimator that corresponds to optimum allocation.
var_st_tsi(opt, N, S)
#> [1] 9719807556
```

Box constraints.

``` r
m <- c(100, 90, 500, 50) # Lower bounds imposed on sample sizes in strata.
M <- c(300, 400, 800, 90) # Upper bounds imposed on sample sizes in strata.
n <- 1284
n >= sum(m) && n <= sum(M)
#> [1] TRUE

# Optimum allocation under box-constraints.
opt <- opt(n = n, a = a, m = m, M = M)
opt
#> [1] 228.9496 400.0000 604.1727  50.8777
sum(opt) == n
#> [1] TRUE
all(opt >= m & opt <= M) # Does not violate any lower or upper bounds constraints.
#> [1] TRUE
# Variance of the stratified estimator that corresponds to optimum allocation.
var_st_tsi(opt, N, S)
#> [1] 538073357
```

Minimization of the total cost with `optcost()` function

``` r
a <- c(3000, 4000, 5000, 2000)
a0 <- 70000
unit_costs <- c(0.5, 0.6, 0.6, 0.3) # c_h, h = 1,...4.
M <- c(100, 90, 70, 80)
V <- 1e6 # Variance constraint.
V >= sum(a^2 / M) - a0
#> [1] TRUE

opt <- optcost(V = V, a = a, a0 = a0, M = M, unit_costs = unit_costs)
opt
#> [1] 40.39682 49.16944 61.46181 34.76805
sum(a^2 / opt) - a0 == V
#> [1] TRUE
all(opt <= M)
#> [1] TRUE
```

Rounding.

``` r
m <- c(100, 90, 500, 50)
M <- c(300, 400, 800, 90)
n <- 1284

# Optimum, non-integer allocation under box-constraints.
opt <- opt(n = n, a = a, m = m, M = M)
opt
#> [1] 297.4286 396.5714 500.0000  90.0000

opt_int <- round_oric(opt)
opt_int
#> [1] 297 397 500  90
```
