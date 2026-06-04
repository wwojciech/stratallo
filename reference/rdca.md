# Recursive Domain-Controlled Allocation (RDCA) Algorithm

**\[stable\]**

Implements the Recursive Domain-Controlled Allocation (RDCA) algorithm
described in Wójciak (2026) . The algorithm solves the following optimum
allocation problem, formulated in mathematical optimization terms:

Minimize \$\$f(T,\\ \boldsymbol x) = T\$\$ over \\\mathbb R \times
\mathbb R\_+^{\lvert \mathcal H \rvert}\\, subject to \$\$\sum\_{(d,h)
\in \mathcal H} x\_{d,h} = n,\$\$ \$\$\sum\_{h \in \mathcal H_d}
(\frac{1}{x\_{d,h}} - \frac{1}{N\_{d,h}}) \frac{N\_{d,h}^2
S\_{d,h}^2}{\rho_d^2} = T, \qquad d \in \mathcal D,\$\$ \$\$x\_{d,h}
\leq N\_{d,h}, \qquad (d,h) \in \mathcal H,\$\$ where:

- \\(T,\\ \boldsymbol x) = (T,\\ (x\_{d,h},\\ (d,h) \in \mathcal H))\\:

  the optimization variable,

- \\\mathcal H \subset \mathbb N^2\\:

  the set of domain-stratum indices,

- \\\mathcal D := \\d \in \mathbb N \colon\\ \exists h,\\ (d,h) \in
  \mathcal H\\\\:

  the set of domain indices,

- \\\mathcal H_d := \\h \in \mathbb N \colon\\ (d,h) \in \mathcal H\\\\:

  the set of strata indices in domain \\d\\,

- \\N\_{d,h} \> 0\\:

  size of stratum \\(d,h)\\,

- \\S\_{d,h} \> 0\\:

  standard deviation of the study variable in stratum \\(d,h)\\,

- \\\rho_d := t_d\\ \sqrt{\kappa_d}\\:

  where \\t_d\\ denotes the total in domain \\d\\, i.e., the sum of the
  values of the study variable for population elements in domain \\d\\,
  and \\\kappa_d\\ is a priority weight for domain \\d\\,

- \\n \in (0,\\ \sum\_{(d,h) \in \mathcal H} N\_{d,h}\]\\:

  total sample size.

## Usage

``` r
rdca(n, H_counts, N, S, rho, rho2 = rho^2, U = NULL, J = NULL)
```

## Arguments

- n:

  (`integerish(1)`)  
  total sample size \\n\\. Must satisfy `0 < n <= sum(N)`.

- H_counts:

  (`integerish`)  
  strata counts in each domain.

- N:

  (`integerish`)  
  strata sizes \\(N\_{d,h},\\ (d,h) \in \mathcal H)\\.

- S:

  (`numeric`)  
  standard deviations \\(S\_{d,h},\\ (d,h) \in \mathcal H)\\ of surveyed
  variable in strata.

- rho:

  (`numeric`)  
  parameters \\(\rho_d,\\ d \in \mathcal D)\\ of the optimization
  problem.

- rho2:

  (`numeric`)  
  the square of `rho` (`rho^2`), provided to reduce potential loss of
  precision due to finite-precision arithmetic.

- U:

  (`integerish` or `NULL`)  
  a vector of indices identifying the *take-max* strata, i.e., the
  strata \\(d,h)\\ for which the allocation is fixed to \\x\_{d,h} =
  N\_{d,h}\\. The indices refer to the positions of strata in the set
  \\\mathcal H\\, in the same order as in the input vectors (`N`, `S`,
  etc.).

  For example, if \\\mathcal H = \\(1,1),\\ (2,1)\\\\ and stratum
  \\(2,1)\\ is a take-max stratum, then `U = 2`.

  If `U` contains all strata from a domain, the dimension of the D
  matrix is reduced accordingly.

  `U` must satisfy one of the following conditions:

  - `n > sum(N[U])`,

  - `n = sum(N[U])` and `n = sum(N)`.

- J:

  (`integerish` or `NULL`)  
  vector of domain indices, subset of \\\mathcal D\\. Specifies the
  domains for which allocated sample sizes must not exceed the
  corresponding strata sizes `N`. For domains not included in `J`,
  allocations may exceed strata sizes. Must not be empty. If `J` is
  `NULL`, it is treated as containing all domains. `U` must not contain
  any strata from domains included in `J`.

## Details

The upper-bound constraints \\x\_{d,h} \le N\_{d,h}\\ are guaranteed to
be preserved only if domain \\d\\ is in `J`. The parameter `J` is used
in the recursion. The specified optimization problem is solved when
`J = NULL`, i.e., when `J` contains all domains.

## Note

This function is optimized for internal use and should typically not be
called directly by users. It is designed to handle a large number of
invocations, specifically recursive invocations of `rdca()`, and, as a
result, parameter assertions are minimal.

## References

Wójciak W (2026). *Multi-Domain Optimum Sample Allocation with
Controlled-Precision under Upper-Bound Constraints*. Ph.D. thesis,
Warsaw University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/phd_wwojciech_optimum_alloc.pdf>.

## See also

[`dca()`](https://wwojciech.github.io/junco/reference/dca.md)

## Examples

``` r
# Three domains with 2, 2, and 3 strata, respectively,
# that is, H = {(1,1), (1,2), (2,1), (2,2), (3,1), (3,2), (3,3)}.
H_counts <- c(2, 2, 3)
# (N_{1,1}, N_{1,2}, N_{2,1}, N_{2,2}, N_{3,1}, N_{3,2}, N_{3,3})
N <- c(140, 110, 135, 190, 200, 40, 70)
# (S_{1,1}, S_{1,2}, S_{2,1}, S_{2,2}, S_{3,1}, S_{3,2}, S_{3,3})
S <- c(180, 20, 5, 4, 35, 9, 40)
total <- c(2, 3, 5)
kappa <- c(0.5, 0.2, 0.3)
rho <- total * sqrt(kappa) # (rho_1, rho_2, rho_3)
rho2 <- total^2 * kappa
sum(N)
#> [1] 885
n <- 828

# Optimum allocation.
rdca(n, H_counts, N, S, rho, rho2)
#> [1] 140.00000 108.06261 135.00000 154.02807 200.00000  20.90933  70.00000

# Upper bounds enforced only for domain 1.
rdca(n, H_counts, N, S, rho, rho2, J = 1)
#> [1] 140.00000 108.44385 138.70684 156.17363 196.13482  10.08693  78.45393
```
