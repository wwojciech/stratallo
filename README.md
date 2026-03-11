
# Optimum Sample Allocation in Stratified Sampling with stratallo

The `stratallo` package provides algorithms for computing optimum sample
allocations in stratified sampling designs. The implemented methods
solve several classical optimization problems arising in survey design
using **exact analytical algorithms**, rather than relying on numerical
approximations.

The package supports a variety of practical constraints, including lower
and upper bounds on stratum sample sizes, cost constraints, and
multi-domain precision control.

------------------------------------------------------------------------

## Installation

``` r
install.packages("stratallo")

# Development version
# install.packages("remotes")
remotes::install_github("wwojciech/stratallo")
```

------------------------------------------------------------------------

## User functions

The package provides three **main user functions** for solving optimum
allocation problems:

| Function    | Description                                                |
|-------------|------------------------------------------------------------|
| `opt()`     | Optimum allocation with a fixed total sample size.         |
| `optcost()` | Minimum-cost allocation under a variance constraint.       |
| `dopt()`    | Multi-domain optimum allocation with controlled precision. |

The package also includes several **helper functions**:

| Function | Description |
|----|----|
| `var_st()` | Computes the value of the variance of the stratified $\pi$ estimator of a population total. |
| `var_stsi()` | Computes the value of the variance of the stratified $\pi$ estimator of a population total under simple random sampling without replacement within each stratum. |
| `alloc_summary()` | Summarizes an allocation produced by `opt()` or `optcost()`. |
| `round_oric()` | Deterministic rounding procedure for non-integer allocations, preserving integer constraints. |
| `round_ran()` | Randomized rounding procedure for non-integer allocations. |

For a detailed description of the allocation problems and algorithms,
see the package vignette.
<!-- [package vignette](vignettes/stratallo.html). -->

------------------------------------------------------------------------

## Example datasets

The package includes several **artificial populations** that can be used
for examples and benchmarking. The available datasets can be listed with

``` r
data(package = "stratallo")
```

------------------------------------------------------------------------

## Examples

``` r
library(stratallo)
```

### Optimum allocation with fixed sample size

``` r
N <- c(3000, 4000, 5000, 2000) # Strata sizes.
S <- c(48, 79, 76, 16) # Standard deviations of a study variable in strata.
A <- N * S
m <- c(100, 90, 500, 50) # Lower bounds.
M <- c(300, 400, 800, 90) # Upper bounds.
n <- 1284 # Total sample size.

x <- opt(n = n, A = A, m = m, M = M)
x
#> [1] 228.9496 400.0000 604.1727  50.8777

x_int <- round_oric(x)
x_int
#> [1] 229 400 604  51

var_stsi(x, N, S)
#> [1] 538073357
var_stsi(x_int, N, S)
#> [1] 538073497
```

### Multi-domain optimal allocation with controlled precision

``` r
# Three domains with 2, 2, and 3 strata, respectively.
H_counts <- c(2, 2, 3)
N <- c(140, 110, 135, 190, 200, 40, 70)
S <- c(180, 20, 5, 4, 35, 9, 40)
total <- c(2, 3, 5)
kappa <- c(0.5, 0.2, 0.3)
n <- 828

dopt(n, H_counts, N, S, total, kappa)
#> [1] 140.00000 108.06261 135.00000 154.02807 200.00000  20.90933  70.00000
```
