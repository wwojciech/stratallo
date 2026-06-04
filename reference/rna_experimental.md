# RNA – Experimental Versions

**\[experimental\]**

Experimental variants of the Recursive Neyman Algorithm (RNA).

## Usage

``` r
rna_rec(
  total_cost,
  A,
  bounds = NULL,
  unit_costs = rep(1, length(A)),
  cmp = .Primitive(">=")
)

rna_prior(
  total_cost,
  A,
  bounds = NULL,
  check = NULL,
  cmp = .Primitive(">="),
  details = FALSE
)
```

## Arguments

- total_cost:

  (`numeric(1)`)  
  total survey cost \\c\\. Must be strictly positive. Additionally:

  - If one-sided lower bounds \\m_1, \ldots, m_H\\ are imposed, it is
    required that \\c \geq \sum\_{h=1}^H c_h m_h\\, i.e.
    `total_cost >= sum(unit_costs * bounds)`.

  - If one-sided upper bounds \\M_1, \ldots, M_H\\ are imposed, it is
    required that \\c \leq \sum\_{h=1}^H c_h M_h\\, i.e.
    `total_cost <= sum(unit_costs * bounds)`.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\. All values must be strictly
  positive.

- bounds:

  (`numeric` or `NULL`)  
  optional lower bounds \\m_1,\ldots,m_H\\, or upper bounds
  \\M_1,\ldots,M_H\\, or `NULL` to indicate that no inequality
  constraints are imposed. If not `NULL`, `bounds` is interpreted as:

  - lower bounds, if `cmp = .Primitive("<=")`, or

  - upper bounds, if `cmp = .Primitive(">=")`.

  See also `total_cost`.

- unit_costs:

  (`numeric`)  
  costs \\c_1,\ldots,c_H\\ of surveying one element in each stratum.
  Strictly positive values. May also be of length 1, in which case the
  value is recycled to match the length of `bounds`.

- cmp:

  (`function`)  
  a binary comparison operator used to check for violations of `bounds`.
  Must be either `.Primitive("<=")` (treating `bounds` as lower bounds
  and invoking the LRNA algorithm) or `.Primitive(">=")` (treating
  `bounds` as upper bounds and invoking the RNA algorithm).

  The value of this argument has no effect if `bounds` is `NULL`.

- check:

  (`integer`)  
  indices of strata for which allocation constraints may be violated.
  For all other strata, constraint violations are not checked.

- details:

  (`logical(1)`)  
  should detailed information on stratum assignments (either take-Neyman
  or take-bound), values of the set function \\s\\, and the number of
  iterations be included in the output?

## Functions

- `rna_rec()`: Recursive implementation of the RNA.

- `rna_prior()`: A variant of the Recursive Neyman Algorithm (RNA) that
  uses prior information about strata for which allocation constraints
  may be violated. For all other strata, allocations are assumed to
  satisfy the bounds.

## Note

This code has not been extensively tested and may change in future
releases.

## Examples

``` r
A <- c(3000, 4000, 5000, 2000)
M <- c(100, 90, 70, 80) # upper bounds

# experimental function (not exported) – examples skipped
if (FALSE) { # \dontrun{
rna_rec(total_cost = 190, A = A, bounds = M)
rna_rec(total_cost = 312, A = A, bounds = M)
rna_rec(total_cost = 339, A = A, bounds = M)
rna_rec(total_cost = 340, A = A, bounds = M)
} # }
```
