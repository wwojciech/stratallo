# Iterative RDCA Implementation

**\[experimental\]**

Iterative implementation of the Recursive Domain-Controlled Allocation
(RDCA) algorithm. Not tested.

## Usage

``` r
rdca_iter(n, H_counts, N, S, rho, rho2 = NULL, ref_domain = 1L)
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

- ref_domain:

  (`integerish(1)`)  
  reference domain (denoted by j in the thesis).

## Examples

``` r
H_counts <- c(2, 2, 3)
N <- c(140, 110, 135, 190, 200, 40, 70)
S <- sqrt(c(180, 20, 5, 4, 35, 9, 40))
total <- c(2, 3, 5)
kappa <- c(0.5, 0.2, 0.3)
rho <- total * sqrt(kappa)
(n <- dca_nmax(H_counts, N, S) - 1)
#> [1] 828.0688

# experimental function (not exported) – examples skipped
if (FALSE) { # \dontrun{
rdca_iter(n, H_counts, N, S, rho)
# 140.0000 103.6139 132.1970 166.4127 195.9701  19.8750  70.0000
} # }
```
