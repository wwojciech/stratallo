# Optimum Sample Allocation in Stratified Sampling with stratallo

Optimum sample allocation in stratified sampling is a fundamental
problem in survey methodology. In its classical formulation, it involves
determining how a given total sample size should be allocated across
strata so that specific criteria, typically related to minimizing the
variances of estimators are satisfied.

The `stratallo` package provides algorithms for computing optimum sample
allocations in several classical stratified sampling designs. These
algorithms compute exact analytical solutions rather than relying on
numerical approximations.

Below we briefly describe the main components of the package and the
types of optimum allocation problems it solves.

------------------------------------------------------------------------

## Package content

### User functions

The package provides three **main user functions** for solving optimum
allocation problems:

| Function | Description |
|----|----|
| [`opt()`](https://wwojciech.github.io/junco/reference/opt.md) | Optimum allocation with fixed total sample size (optionally with lower and/or upper bounds on stratum sample sizes). |
| [`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md) | Minimum-cost allocation under a variance constraint. |
| [`dopt()`](https://wwojciech.github.io/junco/reference/dopt.md) | Multi-domain optimal allocation with controlled precision. |

The package also includes several **helper functions**:

| Function | Description |
|----|----|
| [`var_st()`](https://wwojciech.github.io/junco/reference/var_st.md) | Computes the value of the variance of the stratified $`\pi`$ estimator of a population total. |
| [`var_stsi()`](https://wwojciech.github.io/junco/reference/var_st.md) | Computes the value of the variance of the stratified $`\pi`$ estimator of a population total under simple random sampling without replacement within each stratum. |
| [`alloc_summary()`](https://wwojciech.github.io/junco/reference/alloc_summary.md) | Summarizes an allocation produced by [`opt()`](https://wwojciech.github.io/junco/reference/opt.md) or [`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md). |
| [`round_oric()`](https://wwojciech.github.io/junco/reference/rounding.md) | Deterministic rounding procedure for non-integer allocations, preserving integer constraints |
| [`round_ran()`](https://wwojciech.github.io/junco/reference/rounding.md) | Randomized rounding procedure for non-integer allocations. |

### Datasets

The package includes several **artificial populations** that can be used
for examples and benchmarking. The available datasets can be listed with

``` r

data(package = "stratallo")
```

------------------------------------------------------------------------

## Allocation problems solved with `stratallo`

The allocation problems implemented in `stratallo` are formulated in the
language of mathematical optimization. From the practical point of view,
however, the parameters and formulas appearing in these optimization
problems are derived from a specific sampling design and estimator.

In particular, the formulations correspond to the setting in which each
stratum is sampled using **simple random sampling without replacement
(SI)**, and the parameter of interest is the **population total** of a
single study variable, estimated using the **stratified $`\pi`$
estimator**. Under this design, the variance of the estimator takes a
specific analytical form, which leads directly to the optimization
problems presented below.

More generally, the algorithms apply to any sampling design that yields
a variance expression of the same form as under SI.

### 1. Optimum allocation with fixed sample size

This is the classical optimum allocation problem of determining stratum
sample sizes that minimize the variance of the stratified $`\pi`$
estimator of a population total for a given total sample size.
Optionally, lower and/or upper bounds on the stratum sample sizes may be
imposed.

Let the following be given:

- $`H`$ - total number of strata,
- population parameters $`A_h > 0,\, h \in \{1,\, \ldots,\, H\}`$,
- (optional) lower bounds on sample sizes in strata
  $`m_h \in (0,\, M_h),\, h \in \{1,\, \ldots,\, H\}`$,
- (optional) upper bounds on sample sizes in strata
  $`M_h \in (m_h,\, N_h],\, h \in \{1,\, \ldots,\, H\}`$, where $`N_h`$
  is the stratum size,
- total sample size
  $`n \in \bigl[ \sum_{h=1}^H m_h,\, \sum_{h=1}^H M_h \bigr]`$.

Determine an allocation vector
$`\boldsymbol{x}= (x_1,\, \ldots,\, x_H)`$ that solves the following
optimization problem:

``` math
\begin{alignat}{3}
  & \underset{\boldsymbol{x}~\in~{\mathbb R}_+^H}{\text{minimize}} \quad && \sum_{h=1}^H \tfrac{A_h^2}{x_h} &  & \\
  & \text{subject to} \quad && \sum_{h=1}^H x_h = n, &  & \\
  &  && m_h \leq x_h \leq M_h, & \qquad h &\in \{1,\, \ldots,\, H\},
\end{alignat}
```

where the inequality constraints are optional.

This problem is solved with

``` r

opt()
```

The imposing of inequality constraints is achieved with the proper use
of the `m` and `M` arguments of the function. For more details, see the
help page for
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md) function.

The [`opt()`](https://wwojciech.github.io/junco/reference/opt.md)
function uses different allocation algorithms depending on which
inequality constraints are applied. Each algorithm is implemented in a
separate R function, which is generally not intended to be called
directly by the end user. The algorithms used for different types of
constraints are:

- **Lower bounds only** ($`m_1,\, \ldots,\, m_H`$):
  - LRNA -
    [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
- **Upper bounds only** ($`M_1,\, \ldots,\, M_H`$):
  - RNA -
    [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  - SGA -
    [`sga()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  - SGAPLUS -
    [`sgaplus()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  - COMA -
    [`coma()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
- **Box constraints** ($`m_h, M_h,\, h = 1,\ldots,H`$):
  - RNABOX -
    [`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md).

See the documentation of each specific function for more details about
the corresponding algorithm.

#### Examples

Example population with four strata:

``` r

N <- c(3000, 4000, 5000, 2000) # Strata sizes.
S <- c(48, 179, 176, 16) # Standard deviations of a study variable in strata.
A <- N * S
n <- 190 # Total sample size.
```

Neyman-Tschuprow allocation (no inequality constraints)

``` r

x <- opt(n = n, A = A)
x
#> [1] 15.440181 76.772009 94.356659  3.431151

# Variance of the st. estimator that corresponds to the optimum allocation.
var_stsi(x, N, S)
#> [1] 16235763579

# Round non-integer allocation.

x_int <- round(x)
x_int
#> [1] 15 77 94  3
sum(x_int)
#> [1] 189

x_int_oric <- round_oric(x)
x_int_oric
#> [1] 16 77 94  3
sum(x_int_oric)
#> [1] 190
x_int_oric <= N
#> [1] TRUE TRUE TRUE TRUE
var_stsi(x_int_oric, N, S)
#> [1] 16243033336
```

One-sided upper bounds

``` r

M <- c(100, 90, 70, 80) # Upper bounds.
all(M <= N)
#> [1] TRUE
n <= sum(M)
#> [1] TRUE

x <- opt(n = n, A = A, M = M)
x
#> [1] 24.545455 90.000000 70.000000  5.454545

# Variance of the st. estimator that corresponds to the optimum allocation.
var_stsi(x, N, S)
#> [1] 17501100254
```

Box constraints

``` r

m <- c(100, 90, 500, 50) # Lower bounds.
M <- c(300, 400, 800, 90) # Upper bounds.
n <- 1284
n >= sum(m) && n <= sum(M)
#> [1] TRUE

x <- opt(n = n, A = A, m = m, M = M)
x
#> [1] 117.2812 400.0000 716.7188  50.0000

var_stsi(x, N, S)
#> [1] 2268937372
```

### 2. Minimum-cost allocation under a variance constraint

A minor modification of the classical optimum sample allocation problem
leads to the **minimum-cost allocation**. This problem involves
determining a vector of stratum sample sizes that minimizes the total
survey cost while maintaining a fixed level of the variance of the
stratified $`\pi`$ estimator of a population total. As in the case of
the classical optimum allocation, upper bounds on stratum sample sizes
can optionally be imposed.

Let the following be given:

- $`H`$ - total number of strata,
- population parameters
  $`A_0,\, A_h > 0,\, h \in \{1,\, \ldots,\, H\}`$,
- costs $`c_h > 0,\, h \in \{1,\, \ldots,\, H\}`$ of surveying one
  element in each stratum,
- (optional) upper bounds on sample sizes in strata
  $`M_h \in (0,\, N_h],\, h \in \{1,\, \ldots,\, H\}`$, where $`N_h`$ is
  the stratum size.
- level of variance $`V`$. If upper bounds are imposed, it must satisfy:
  $`V > \sum_{h=1}^H A_h^2/M_h - A_0`$. Otherwise, $`V > 0`$.

Determine an allocation vector
$`\boldsymbol{x}= (x_1,\, \ldots,\, x_H)`$ that solves:

``` math
\begin{alignat}{3}
  & \underset{\boldsymbol{x}\in {\mathbb R}_+^H}{\text{minimize}} \quad && \sum_{h=1}^H c_h x_h &  & \\
  & \text{subject to} \quad && \sum_{h=1}^H \frac{A_h^2}{x_h} - A_0 = V, &  & \\
  &  && x_h \le M_h, & \qquad h &\in \{1,\, \ldots,\, H\},
\end{alignat}
```

where the inequality constraints are optional.

This problem is solved with

``` r

optcost()
```

The algorithm used for this problem is the LRNA, described in Wójciak
(2023).

#### Examples

``` r

A <- c(3000, 4000, 5000, 2000)
A0 <- 70000
unit_costs <- c(0.5, 0.6, 0.6, 0.3) # c_h, h = 1,...,4.
M <- c(100, 90, 70, 80)
V <- 1e6 # Variance constraint.
V >= sum(A^2 / M) - A0
#> [1] TRUE

optcost(V = V, A = A, A0 = A0, M = M, unit_costs = unit_costs)
#> [1] 40.39682 49.16944 61.46181 34.76805
```

### 3. Multi-domain optimum allocation with controlled precision

In many survey applications, estimates are required not only for the
population as a whole but also for specific subpopulations (domains).
The objective of this allocation is the simultaneous minimization of the
variance of the global total estimator and the variances of domain total
estimators under a fixed total sample size, while ensuring that
allocations do not exceed the population sizes within strata.

The settings of this problem allow for using relative variances (instead
of absolute variances) and provide explicit control of domain-wise
precisions. This is achieved through the parameter
$`\boldsymbol{\rho} = (\rho_d,\, d \in \mathcal D)`$, where each element
represents the product of the domain total $`t_d`$ and the square root
of the priority weight $`\kappa_d`$, i.e.,
$`\rho_d = t_d \, \sqrt{\kappa_d},\, d \in \mathcal D`$, where
$`\mathcal D`$ denotes the set of domain indices (see Wójciak (2026) for
more details).

Let the following be given:

- $`\mathcal H\subset {\mathbb N}^2`$ - the set of domain-stratum
  indices,
- $`\mathcal D:= \{d \in {\mathbb N}\colon\; \exists h \in {\mathbb N},\; (d,h) \in \mathcal H\}`$ -
  the set of domain indices,
- $`\mathcal H_d := \{h \in {\mathbb N}\colon\; (d,h) \in \mathcal H\}`$ -
  the set of strata indices in domain $`d \in \mathcal D`$,
- strata sizes $`N_{d,h},\, (d,h) \in \mathcal H`$,
- standard deviations of study variable in strata
  $`S_{d,h},\, (d,h) \in \mathcal H`$,
- domain totals $`t_d > 0,\, d \in \mathcal D`$,
- priority weights for domains $`\kappa_d > 0,\, d \in \mathcal D`$,
  such that $`\sum_{d \in \mathcal D} \kappa_d = 1`$,
- total sample size
  $`n \in (0,\, \sum_{(d,h) \in \mathcal H} N_{d,h}]`$.

Determine an allocation vector
$`\boldsymbol{x}= (x_{d,h},\, (d,h) \in \mathcal H)`$ and scalar $`T`$
that solve the following optimization problem:

``` math
\begin{alignat}{3}
  & \underset{(T,\, \boldsymbol{x})~\in~{\mathbb R}\times {\mathbb R}_+^{\lvert \mathcal H \rvert}}{\text{minimize}\quad} \quad && T &  & \\
    & \text{subject to} \quad && \sum_{(d,h) \in \mathcal H} x_{d,h} - n = 0, &  & \label{cpda:cnstr_n} \\
    &  && \sum_{h \in \mathcal H_d} \Bigl( \frac{1}{x_{d,h}} - \frac{1}{N_{d,h}} \Bigr)
    \frac{N_{d,h}^2 S_{d,h}^2}{\rho_d^2} - T = 0, & \qquad d &\in \mathcal D, \\
    &  && x_{d,h} - N_{d,h} \le 0, & \qquad (d,h) &\in \mathcal H, \label{cpda:cnstr_N}
\end{alignat}
```

where
$`(T,\, \boldsymbol{x}) = \bigl( T,\, (x_{d,h},\, (d,h) \in \mathcal H) \bigr)`$
is the optimization variable, and
$`\rho_d := t_d\, \sqrt{\kappa_d},\, d \in \mathcal D`$.

This problem is solved with

``` r

dopt()
```

The algorithm used is the RDCA, described in Wójciak (2026) and
implemented in
[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md).

#### Examples

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

------------------------------------------------------------------------

## Note on finite precision arithmetic

Consider the following population and total sample size:

``` r

N <- 101:104 # strata sizes
S <- 1001:1004 # standard deviations in strata
A <- N * S
n <- 409L # total sample size
```

The optimal allocation is:

``` r

x <- opt(n = n, A = A)
x
#> [1] 100.6017 101.6992 102.7988 103.9003
```

Mathematically, we expect the following equalities to hold:

``` r

sum(x) == n
#> [1] FALSE
sum((n / sum(A)) * A) == n
#> [1] FALSE
```

However, due to finite precision arithmetic, `sum(x)` may not be exactly
equal to `n`, even though mathematically it should be.

You may see something like this when displaying full precision:

``` r

options(digits = 22)
sum(x)
#> [1] 408.9999999999999431566
```

This tiny difference occurs because computers represent numbers with
limited precision.

------------------------------------------------------------------------

## References

Wójciak, W. (2023), “Another Solution for Some Optimum Allocation
Problem,” *Statistics in Transition new series*, Polish Statistical
Association; Statistics Poland, 24, 203–219.
<https://doi.org/10.59170/stattrans-2023-071>.

Wójciak, W. (2026), “Multi-domain optimum sample allocation with
controlled-precision under upper-bound constraints,” PhD thesis, Warsaw
University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/phd_wwojciech_optimum_alloc.pdf>.
