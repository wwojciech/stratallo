# Internal Diagnostic Functions for Checking Optimality of `rdca()` Allocations

**\[stable\]**

Diagnostic tools to verify the objective function and constraint
satisfaction for allocations computed by the
[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md)
algorithm.

## Usage

``` r
rdca_obj_cnstr(x, n, H_counts, N, S, rho2, J = integer(0))

rdca_cnstr_check(x, n, H_counts, N, S, rho2, J = integer(0), tol_max = 0.1)

rdca_optcond_sU(H_counts, S, rho, s, U, return_diff = FALSE)
```

## Arguments

- x:

  (numeric)  
  vector of sample allocations.

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

- rho2:

  (`numeric`)  
  the square of `rho` (`rho^2`).

- J:

  (`integerish` or `NULL`)  
  vector of domain indices, a subset of \\\mathcal D\\. Specifies
  domains \\d \in \mathcal D\\ for which values of constraint functions
  \\g\_{dh}\\ are computed. If `integer(0)`, these values are not
  computed. If `NULL`, all domains are included.

- tol_max:

  (`integerish(1)`)  
  the maximum tolerance exponent (as a power of 10).

- rho:

  (`numeric`)  
  parameters \\(\rho_d,\\ d \in \mathcal D)\\ of the optimization
  problem.

- s:

  (`numeric`)  
  vector of values for function \\s(\mathcal U, \boldsymbol{v}, d \mid
  p)\\ calculated only for domains included in `U` that are not fully
  blocked by the take-max assignment.

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

- return_diff:

  (`logical(1)`)  
  If `FALSE`, the function returns a logical vector indicating whether
  the optimality condition \$\$s(\mathcal{U},\\ \boldsymbol{v},\\ d
  \mid p) \ge \rho_d / S\_{d,h}\$\$ is satisfied for each unblocked
  stratum \\(d,h) \in \mathcal U.\\ If `TRUE`, the function returns the
  **differences** \$\$s(\mathcal{U},\\ \boldsymbol{v},\\ d \mid p) -
  \rho_d / S\_{d,h}\$\$ instead of a logical vector, which can be used
  to assess by how much the condition is satisfied or violated.

## Functions

- `rdca_obj_cnstr()`: Compute the value of the objective function and
  constraint functions for a given allocation.

- `rdca_cnstr_check()`: Check whether the equality and inequality
  constraints are satisfied for a given allocation, within a specified
  tolerance. The tolerance applies to equality constraints only.

- `rdca_optcond_sU()`: Check the optimality condition related to \\s\\.
  Specifically, verifies whether \\s(\mathcal{U},\\ \boldsymbol{v},\\ d
  \mid p) \ge \rho_d / S\_{d,h}\\ for all \\(d,h) \in \mathcal{U}\\ such
  that \\d\\ is not fully blocked by \\\mathcal{U}\\.

## Examples

``` r
H_counts <- c(2, 2) # 2 domains with 2 strata each
N <- c(140, 110, 135, 190)
S <- sqrt(c(180, 20, 5, 4))
total <- c(2, 3)
kappa <- c(0.4, 0.6)
rho <- total * sqrt(kappa)
rho2 <- total^2 * kappa
n <- 500

(x <- dca(n, H_counts, N, S, rho, rho2))
#> [1] 162.23678  42.49059 130.71966 164.55297

# internal functions (not exported) – examples skipped

if (FALSE) { # \dontrun{
rdca_obj_cnstr(x, n, H_counts, N, S, rho2)
rdca_obj_cnstr(x, n, H_counts, N, S, rho2, 2)
rdca_obj_cnstr(x, n, H_counts, N, S, rho2, NULL)
} # }

if (FALSE) { # \dontrun{
rdca_cnstr_check(x, n, H_counts, N, S, rho2)
rdca_cnstr_check(x, n, H_counts, N, S, rho2, 1)
rdca_cnstr_check(x, n, H_counts, N, S, rho2, 2)
rdca_cnstr_check(x, n, H_counts, N, S, rho2, NULL)
} # }

if (FALSE) { # \dontrun{
(n <- dca_nmax(H_counts, N, S) - 1)

U <- 1
(x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
rdca_optcond_sU(H_counts, S, rho, x$s, U) # TRUE

U <- 2
(x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
rdca_optcond_sU(H_counts, S, rho, x$s, U) # FALSE

U <- 3
(x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
rdca_optcond_sU(H_counts, S, rho, x$s, U) # TRUE

U <- 1:2 # domain 2 blocked
(x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
rdca_optcond_sU(H_counts, S, rho, x$s, U) # no unblocked strata in `U`
} # }
```
