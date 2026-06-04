# Domain-Controlled Allocation (DCA) Algorithm

**\[stable\]**

Functions implementing the Domain-Controlled Allocation (DCA) algorithm
described in Wesołowski (2019) and Wójciak (2026) . The algorithm solves
the following optimum allocation problem, formulated in mathematical
optimization terms:

Minimize \$\$f(T,\\ \boldsymbol x) = T\$\$ over \\\mathbb R \times
\mathbb R\_+^{\lvert \mathcal H \rvert}\\, subject to \$\$\sum\_{(d,h)
\in \mathcal H} x\_{d,h} = n,\$\$ \$\$\sum\_{h \in \mathcal H_d}
(\frac{1}{x\_{d,h}} - \frac{1}{N\_{d,h}}) \frac{N\_{d,h}^2
S\_{d,h}^2}{\rho_d^2} = T, \qquad d \in \mathcal D,\$\$ where:

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
dca0(n, H_counts, N, S, rho, rho2, details = FALSE)

dca(n, H_counts, N, S, rho, rho2, U = NULL, details = FALSE)

dca_nmax(H_counts, N, S)
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

- details:

  (`logical(1)`)  
  whether to produce detailed debug output.

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

## Value

If `details = FALSE`, the optimal \\\boldsymbol x^\*\\ is returned.
Otherwise, a list is returned containing the optimal \\\boldsymbol
x^\*\\ (element named `x`) along with other internal details of this
algorithm. In particular, the `lambda` element of the list corresponds
to the optimal \\T^\*\\.

## Details

For \\n \in (0,\\ n\_{max})\\, the optimal value satisfies \\T^\* \>
0\\, where \$\$ n\_{max} := \sum\_{d \in \mathcal D} \frac{\bigl(
\sum\_{h \in \mathcal H_d} N\_{d,h} S\_{d,h} \bigr)^2}{\sum\_{h \in
\mathcal H_d} N\_{d,h} S\_{d,h}^2}. \$\$ See Proposition 2.1 in
Wesołowski (2019) or Wójciak (2026) for details. The value \\n\_{max}\\
is less than or equal to `sum(N)` and can be computed with `dca_nmax()`.

## Functions

- `dca0()`: Domain-Controlled Allocation algorithm by Wesołowski (2019)

- `dca()`: Domain-Controlled Allocation algorithm by Wesołowski (2019) ,
  optionally using a set of take-max strata as described in
  Wójciak (2026) .

- `dca_nmax()`: Computes the maximum total sample size \\n\_{max}\\ such
  that the optimization problem solved by the Domain-Controlled
  Allocation (DCA) algorithm admits a strictly positive optimal value
  \\T^\*\\.

## Note

These functions are optimized for internal use and should typically not
be called directly by users. They are designed to handle a large number
of invocations, specifically recursive calls from
[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md), and, as
a result, parameter assertions are minimal.

## References

Wójciak W (2026). *Multi-Domain Optimum Sample Allocation with
Controlled-Precision under Upper-Bound Constraints*. Ph.D. thesis,
Warsaw University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/phd_wwojciech_optimum_alloc.pdf>.

Wesołowski J (2019). “Multi-domain Neyman-Tchuprov optimal allocation.”
*Statistics in Transition new series*, **20**(4), 1–12.
[doi:10.21307/stattrans-2019-031](https://doi.org/10.21307/stattrans-2019-031)
.

Wesołowski J, Wieczorkowski R (2017). “An eigenproblem approach to
optimal equal-precision sample allocation in subpopulations.”
*Communications in Statistics - Theory and Methods*, **46**(5),
2212–2231.
[doi:10.1080/03610926.2015.1040501](https://doi.org/10.1080/03610926.2015.1040501)
.

## See also

[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md)

## Examples

``` r
# Two domains with 1 and 3 strata, respectively,
# that is, H = {(1,1), (2,1), (2,2), (2,3)}.
H_counts <- c(1, 3)
N <- c(140, 110, 135, 190) # (N_{1,1}, N_{2,1}, N_{2,2}, N_{2,3})
S <- sqrt(c(180, 20, 5, 4)) # (S_{1,1}, S_{2,1}, S_{2,2}, S_{2,3})
total <- c(2, 3)
kappa <- c(0.4, 0.6)
rho <- total * sqrt(kappa) # (rho_1, rho_2)
rho2 <- total^2 * kappa
sum(N) # 575
#> [1] 575
n_max <- dca_nmax(H_counts, N, S) # 519.0416

n <- floor(n_max) - 1

dca0(n, H_counts, N, S, rho, rho2)
#> [1] 139.98377 158.42455  97.21507 122.37661
x0 <- dca0(n, H_counts, N, S, rho, rho2, details = TRUE)
x0$x
#> [1] 139.98377 158.42455  97.21507 122.37661
x0$lambda
#> [1] 1.825882
x0$k
#> [1] -0.7542761
x0$v
#> [1] -0.1249807 -0.9921592
x0$s
#> [1] 0.09426998 0.74836194

n <- ceiling(n_max) + 1
x0 <- dca0(n, H_counts, N, S, rho, rho2, details = TRUE)
x0$x
#> [1] 140.03029 159.66234  97.97462 123.33275
x0$lambda
#> [1] -3.406879

n <- floor(n_max) - 1

x1 <- dca(n, H_counts, N, S, rho, rho2, details = TRUE)
x1$x
#> [1] 139.98377 158.42455  97.21507 122.37661
x1$x_Uc
#> [1] 139.98377 158.42455  97.21507 122.37661
x1$lambda
#> [1] 1.825882
x1$s
#> [1] 0.09426998 0.74836194

dca(n, H_counts, N, S, rho, rho2, U = 1)
#> [1] 140.00000 158.41775  97.21089 122.37135
x2 <- dca(n, H_counts, N, S, rho, rho2, U = 1, details = TRUE)
x2$x
#> [1] 140.00000 158.41775  97.21089 122.37135
x2$x_Uc
#> [1] 158.41775  97.21089 122.37135
x2$lambda
#> [1] 1.85486
x2$s
#> [1] 0.7483298
```
