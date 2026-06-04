# DCA Algorithm for Upper-Bound Constrained Allocations (M \<= N)

**\[experimental\]**

Prototype (under testing).

## Usage

``` r
dca_M(n, H_counts, N, S, rho, rho2, M = N, U = NULL)
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

- M:

  (`integerish`)  
  upper bounds on sample sizes in strata. Defaults to `N`.

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

## Examples

``` r
H_counts <- c(5, 2)
H_names <- rep(seq_along(H_counts), times = H_counts)
S <- c(154, 178, 134, 213, 124, 102, 12)
N <- c(100, 100, 100, 100, 100, 100, 100)
M <- c(80, 90, 70, 40, 10, 90, 100)
names(M) <- names(N) <- H_names
total <- c(13, 2)
kappa <- c(0.8, 0.2)
n <- 150

# experimental function (not exported) – examples skipped
if (FALSE) { # \dontrun{
dca_M(n, H_counts, N, S, total, kappa, M = M, U = 5)
#         1         1         1         1         1         2         2
# 12.754880 14.742653 11.098402 17.641490 10.000000 74.945462  8.817113
} # }
```
