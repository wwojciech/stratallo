# Optimum Sample Allocation in Stratified Sampling

**\[stable\]**

Computes the optimum allocation for the following optimum allocation
problem, formulated in mathematical optimization terms:

Minimize \$\$f(x_1,\ldots,x_H) = \sum\_{h=1}^H \frac{A^2_h}{x_h}\$\$
over \\\mathbb R\_+^H\\, subject to \$\$\sum\_{h=1}^H x_h = n,\$\$
\$\$m_h \leq x_h \leq M_h, \qquad h = 1,\ldots,H,\$\$ where \\n \> 0,\\
A_h \> 0,\\ m_h \> 0,\\ M_h \> 0\\, such that \\m_h \< M_h,\\ h =
1,\ldots,H\\, and \\\sum\_{h=1}^H m_h \leq n \leq \sum\_{h=1}^H M_h\\,
are given numbers. Inequality constraints are optional and may be
omitted.

Inequality constraints are optional, and the user can choose whether and
how they are applied to the optimization problem. This is controlled
using the `m` and `M` arguments as follows:

- **No inequality constraints:** both `m` and `M` must be `NULL`
  (default).

- **Lower bounds only (\\m_1,\\ \ldots,\\ m_H\\):** specify `m`, and set
  `M = NULL`.

- **Upper bounds only (\\M_1,\\ \ldots,\\ M_H\\):** specify `M`, and set
  `m = NULL`.

- **Box constraints (\\m_h, M_h,\\ h = 1,\ldots,H\\):** specify both `m`
  and `M`.

## Usage

``` r
opt(n, A, m = NULL, M = NULL, M_algorithm = "rna")
```

## Arguments

- n:

  (`integerish(1)`)  
  total sample size. Must satisfy `n > 0`. Additionally:

  - If `bounds_inner` is not `NULL`, then `n >= sum(bounds_inner)` when
    `bounds_inner` are treated as lower bounds, or
    `n <= sum(bounds_inner)` when treated as upper bounds.

  - If `bounds_outer` is not `NULL`, then `n >= sum(bounds_outer)` when
    `bounds_outer` are treated as lower bounds, or
    `n <= sum(bounds_outer)` when treated as upper bounds.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\. All values must be strictly
  positive.

- m:

  (`numeric` or `NULL`)  
  optional lower bounds \\m_1,\ldots,m_H\\ for the stratum sample sizes.
  If no lower bounds are desired, set `m = NULL`. If `M` is not `NULL`,
  it is required that \\m_h \< M_h\\ for all strata.

- M:

  (`numeric` or `NULL`)  
  optional upper bounds \\M_1,\ldots,M_H\\ for the stratum sample sizes.
  If no upper bounds are desired, set `M = NULL`. If `m` is not `NULL`,
  it is required that \\m_h \< M_h\\ for all strata.

- M_algorithm:

  (`string`)  
  Name of the algorithm to use for computing the sample allocation when
  only upper-bound constraints are imposed. Must be one of `"rna"`
  (default), `"sga"`, `"sgaplus"`, or `"coma"`. This parameter is used
  only when \\H \> 1\\ and `n < sum(M)`.

## Value

A numeric vector of the optimal sample allocations for each stratum.

## Details

The `opt()` function uses different allocation algorithms depending on
which inequality constraints are applied. Each algorithm is implemented
in a separate R function, which is generally **not intended to be called
directly** by the end user. The algorithms are:

- **Lower bounds only (\\m_1,\\ \ldots,\\ m_H\\):**

  - `LRNA` -
    [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

- **Upper bounds only (\\M_1,\\ \ldots,\\ M_H\\):**

  - `RNA` -
    [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

  - `SGA` -
    [`sga()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

  - `SGAPLUS` -
    [`sgaplus()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

  - `COMA` -
    [`coma()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

- **Box constraints (\\m_h, M_h,\\ h = 1,\ldots,H\\):**

  - `RNABOX` -
    [`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md)

See the documentation of each specific function for more details about
the corresponding algorithm.

## Note

If no inequality constraints are applied, the allocation follows the
Neyman allocation: \$\$x_h = A_h \frac{n}{\sum\_{i=1}^H A_i}, \quad h =
1,\ldots,H.\$\$

For a **stratified \\\pi\\ estimator** of the population total using
stratified simple random sampling without replacement design, the
objective function parameters \\A_h\\ are: \$\$A_h = N_h S_h, \quad h =
1,\ldots,H,\$\$ where \\N_h\\ is the size of stratum \\h\\ and \\S_h\\
is the standard deviation of the study variable in stratum \\h\\.

## References

Särndal C, Swensson B, Wretman J (1992). *Model Assisted Survey
Sampling*. Springer New York, NY. ISBN 978-0-387-40620-6.

## See also

[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md),
[`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`sga()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`sgaplus()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`coma()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md).

## Examples

``` r
A <- c(3000, 4000, 5000, 2000)
m <- c(100, 90, 70, 50)
M <- c(300, 400, 200, 90)

# One-sided lower bounds.
opt(n = 340, A = A, m = m)
#> [1] 100  90 100  50
opt(n = 400, A = A, m = m)
#> [1] 100.00000 109.09091 136.36364  54.54545
opt(n = 700, A = A, m = m)
#> [1] 150 200 250 100

# One-sided upper bounds.
opt(n = 190, A = A, M = M)
#> [1] 40.71429 54.28571 67.85714 27.14286
opt(n = 700, A = A, M = M)
#> [1] 175.7143 234.2857 200.0000  90.0000

# Box-constraints.
opt(n = 340, A = A, m = m, M = M)
#> [1] 100  90 100  50
opt(n = 500, A = A, m = m, M = M)
#> [1] 107.14286 142.85714 178.57143  71.42857
x <- opt(n = 800, A = A, m = m, M = M)
x
#> [1] 218.5714 291.4286 200.0000  90.0000

# Variance corresponding to the allocation x.
var_st(x = x, A = A, A0 = 45000)
#> [1] 220522.9

# Execution-time comparison of different algorithms using the microbenchmark package.
if (FALSE) { # \dontrun{
N <- pop969s_ucost[, "N"]
S <- pop969s_ucost[, "S"]
A <- N * S
nfrac <- c(0.005, seq(0.05, 0.95, 0.05))
n <- setNames(as.integer(nfrac * sum(N)), nfrac)
lapply(
  n,
  function(ni) {
    microbenchmark::microbenchmark(
      RNA = opt(ni, A, M = N, M_algorithm = "rna"),
      SGA = opt(ni, A, M = N, M_algorithm = "sga"),
      SGAPLUS = opt(ni, A, M = N, M_algorithm = "sgaplus"),
      COMA = opt(ni, A, M = N, M_algorithm = "coma"),
      times = 200,
      unit = "us"
    )
  }
)
} # }
```
