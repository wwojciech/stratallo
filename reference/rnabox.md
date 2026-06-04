# Recursive Neyman Algorithm for Optimum Sample Allocation under Box Constraints (RNABOX)

**\[stable\]**

Implements the Recursive Neyman Algorithm for Optimum Sample Allocation
under Box Constraints (RNABOX), as proposed in Wesołowski et al. (2024)
. The algorithm solves the following optimum allocation problem,
formulated in mathematical optimization terms:

Minimize \$\$f(x_1,\ldots,x_H) = \sum\_{h=1}^H \frac{A^2_h}{x_h}\$\$
over \\\mathbb R\_+^H\\, subject to \$\$\sum\_{h=1}^H x_h = n,\$\$
\$\$m_h \leq x_h \leq M_h, \qquad h = 1,\ldots,H,\$\$ where \\n \> 0,\\
A_h \> 0,\\ m_h \> 0,\\ M_h \> 0\\, such that \\m_h \< M_h,\\ h =
1,\ldots,H\\, and \\\sum\_{h=1}^H m_h \leq n \leq \sum\_{h=1}^H M_h\\,
are given numbers. Inequality constraints are optional and may be
omitted.

## Usage

``` r
rnabox(
  n,
  A,
  bounds_inner = NULL,
  bounds_outer = NULL,
  cmp_inner = .Primitive(">="),
  cmp_outer = .Primitive("<=")
)
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

- bounds_inner:

  (`numeric` or `NULL`)  
  optional bounds on sample sizes in strata for the **interim (inner)
  allocation** phase of `rnabox()`. These can be either lower bounds
  \\m_1,\ldots,m_H\\ or upper bounds \\M_1,\ldots,M_H\\, depending on
  the value of `cmp_inner`. If `bounds_inner` is `NULL`, no bounds are
  imposed during the interim allocation phase.

  If both `bounds_inner` and `bounds_outer` are not `NULL`, the
  following element-wise relationships must hold:

  - If `bounds_inner` are treated as lower bounds, then
    `bounds_inner < bounds_outer`.

  - If `bounds_inner` are treated as upper bounds, then
    `bounds_inner > bounds_outer`.

  `bounds_inner` is passed by `rnabox()` to
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  as the `bounds` argument, serving as an interim allocation step
  applied before enforcing the outer `bounds_outer`.

- bounds_outer:

  (`numeric` or `NULL`)  
  optional bounds on sample sizes in strata, checked during the
  **violation check (outer)** phase of the iterative `rnabox()` loop.
  These can be either lower bounds \\m_1,\ldots,m_H\\ or upper bounds
  \\M_1,\ldots,M_H\\, depending on the value of `cmp_outer`. If
  `bounds_outer` is `NULL`, no bounds are imposed during the outer
  phase.

  If both `bounds_outer` and `bounds_inner` are not `NULL`, the
  following element-wise relationships must hold:

  - If `bounds_outer` are treated as lower bounds, then
    `bounds_outer < bounds_inner`.

  - If `bounds_outer` are treated as upper bounds, then
    `bounds_outer > bounds_inner`.

- cmp_inner:

  (`function`)  
  a binary comparison operator used to check for violations of
  `bounds_inner`. Must be either `.Primitive("<=")` or
  `.Primitive(">=")`. This operator determines how `bounds_inner` is
  handled:

  - `.Primitive("<=")` treats `bounds_inner` as lower bounds and causes
    `rnabox()` to apply the LRNA algorithm as an inner allocation step.

  - `.Primitive(">=")` treats `bounds_inner` as upper bounds and causes
    `rnabox()` to apply the RNA algorithm as an inner allocation step.

  The value of this argument has no effect if `bounds_inner` is `NULL`.

  `cmp_inner` is passed by `rnabox()` to
  [`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)
  as the `cmp` argument, serving as an interim allocation step applied
  before enforcing the outer `bounds_outer`.

- cmp_outer:

  (`function`)  
  a binary comparison operator used to check for violations of
  `bounds_outer`. It must be the logical complement of `cmp_inner` (up
  to equality): if `cmp_inner = .Primitive("<=")`, then
  `cmp_outer = .Primitive(">=")`, and vice versa. It determines how
  `bounds_outer` is handled:

  - `.Primitive("<=")` treats `bounds_outer` as lower bounds.

  - `.Primitive(">=")` treats `bounds_outer` as upper bounds.

  The value of this argument has no effect if `bounds_outer` is `NULL`.

  `cmp_outer` is provided solely for computational efficiency, as its
  value is fully determined by `cmp_inner`.

## Value

A numeric vector of optimum sample allocations in strata.

## Note

The `rnabox()` function is optimized for internal use and should
typically not be called directly by users. Use
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md) instead.

## References

Wesołowski J, Wieczorkowski R, Wójciak W (2024). “Recursive Neyman
algorithm for optimum sample allocation under box constraints on sample
sizes in strata.” *Survey Methodology*, **50**(2), 487–511. ISSN
1492-0921.
<https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X201200111682>.

## See also

[`opt()`](https://wwojciech.github.io/junco/reference/opt.md),
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md),
[`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`sga()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`sgaplus()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`coma()`](https://wwojciech.github.io/junco/reference/alg_1sided.md)

## Examples

``` r
N <- c(454, 10, 116, 2500, 2240, 260, 39, 3000, 2500, 400)
S <- c(0.9, 5000, 32, 0.1, 3, 5, 300, 13, 20, 7)
A <- N * S
m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294) # lower bounds
M <- N # upper bounds

# Regular allocation.
n <- 6000
opt_regular <- rnabox(n, A, M, m)

# Vertex allocation.
n <- 4076
opt_vertex <- rnabox(n, A, M, m)
```
